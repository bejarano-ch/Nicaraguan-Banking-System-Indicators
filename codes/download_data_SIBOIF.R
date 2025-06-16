################################################################################
# update_siboif.R — Incremental download & processing of SIBOIF banking data
# Author: Christian Bejarano
# Last update: 2025-06-11
#
# • Downloads only the months not yet stored locally.
# • Saves three Excel files:
#       - Estado de Resultado.xlsx
#       - Estado de Situación Financiera.xlsx
#       - Activos_promedio.xlsx
# • Adds rolling 12-month metrics (sum12, mean12) for analysis.
################################################################################

# ──────────────────────────────────────────────────────────────────────────────
# 1. Load libraries -----------------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  httr, jsonlite, dplyr, tidyr, lubridate, slider,
  openxlsx, readxl, purrr, stringr
)

# ──────────────────────────────────────────────────────────────────────────────
# 2. Global paths and parameters ---------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
superintendency      <- "Bancos"
exchange_rate_path   <- "data/exchange_rate.xlsx"

xlsx_income          <- "data/Estado de Resultado.xlsx"
xlsx_balance         <- "data/Estado de Situación Financiera.xlsx"
xlsx_assets          <- "data/Activos_promedio.xlsx"

# ──────────────────────────────────────────────────────────────────────────────
# 3. API helper ---------------------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
get_siboif_data <- function(superintendency, min_date, max_date, report_type) {
  url <- paste0(
    "https://www.siboif.gob.ni/rest/estadisticas",
    "?intendencia=", superintendency,
    "&fecha[min]=", min_date,
    "&fecha[max]=", max_date,
    "&tipo_reporte=", URLencode(report_type)
  )
  
  resp <- GET(url)
  if (http_status(resp)$category != "Success")
    stop("API error: ", http_status(resp)$message)
  
  parsed <- content(resp, as = "parsed", type = "application/json")
  if (!length(parsed)) return(tibble())                # Gracefully handle empty
  
  bind_rows(lapply(parsed, as.data.frame)) |>
    mutate(across(starts_with("valor"), ~ as.numeric(gsub(",", "", .))))
}

# ──────────────────────────────────────────────────────────────────────────────
# 4. Domain-specific cleaning -------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
process_data <- function(raw, ex_rate_tbl) {
  if (nrow(raw) == 0) return(tibble())
  
  raw |>
    rename(variable = variable_1) |>
    mutate(fecha = ymd(fecha)) |>
    
    # Merge exchange rate and convert to millions of C$
    left_join(ex_rate_tbl, by = "fecha") |>
    mutate(
      valor    = as.numeric(valor_1) / exchange_rate / 1e6,
      orden    = as.integer(orden),
      institucion = recode(
        institucion,
        "BANCO LAFISE BANCENTRO"      = "LAFISE",
        "BANCO PRODUZCAMOS"           = "BFP",
        "BANCO FICOHSA"               = "FICOHSA",
        "Financiera FDL, S.A."        = "FDL",
        "FINANCIERA FAMA"             = "FAMA",
        "FINANCIERA FINCA NICARAGUA"  = "FINCA",
        "FINANCIERA FUNDESER"         = "FUNDESER",
        "BANCO ATLÁNTIDA"             = "ATLÁNTIDA",
        .default                      = institucion
      )
    ) |>
    select(institucion, fecha, orden, valor) |>
    filter(!institucion %in% c("SFB", "SFN", "SF"))
}

# ──────────────────────────────────────────────────────────────────────────────
# 5. Incremental updater ------------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
update_siboif <- function(out_path, report_type, ex_rate_tbl) {
  
  # Load current data (if any) -------------------------------------------------
  existing <- if (file.exists(out_path)) {
    read_excel(out_path) |> mutate(fecha = ymd(fecha))
  } else tibble()
  
  # Compute missing date window ----------------------------------------------
  start_date <- if (nrow(existing)) max(existing$fecha) + days(1)
  else ymd("2019-01-01")
  end_date   <- floor_date(Sys.Date(), "month") - days(1)  # latest complete mo.
  
  if (start_date > end_date) {
    message("✔ ", report_type, " is already up-to-date.")
    return(existing)
  }
  
  # Break into ≤2-year chunks (API stability) ---------------------------------
  seq_dates  <- seq(start_date, end_date, by = "2 years")
  ranges     <- Map(c, seq_dates, c(seq_dates[-1] - days(1), end_date))
  
  raw_new <- map_dfr(ranges, ~
                       get_siboif_data(superintendency, .x[1], .x[2], report_type)
  )
  if (nrow(raw_new) == 0) {
    message("ℹ No new rows published yet for ", report_type, ".")
    return(existing)
  }
  
  # Clean, merge, save ---------------------------------------------------------
  clean_new <- process_data(raw_new, ex_rate_tbl)
  updated   <- bind_rows(existing, clean_new) |> distinct()
  
  write.xlsx(updated, out_path, overwrite = TRUE)
  message("✔ Updated ", report_type, " → ", end_date)
  updated
}

# ──────────────────────────────────────────────────────────────────────────────
# 6. Load reference data (exchange rate) --------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
exchange_rate_tbl <- read_excel(exchange_rate_path) |> 
  mutate(fecha = ymd(fecha))

# ──────────────────────────────────────────────────────────────────────────────
# 7. Refresh both reports -----------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
income_raw  <- update_siboif(xlsx_income,  "Estado de Resultados (ER)",
                             exchange_rate_tbl)
balance_raw <- update_siboif(xlsx_balance, "Estado de Situación Financiera (ESF)",
                             exchange_rate_tbl)

# ──────────────────────────────────────────────────────────────────────────────
# 8. Extra metrics: Income Statement (delta & 12-mo sum) ----------------------
# ──────────────────────────────────────────────────────────────────────────────
income_statement <- income_raw |>
  group_by(institucion, orden) |>
  arrange(fecha) |>
  mutate(month = floor_date(fecha, "month")) |>
  complete(month = seq(min(month), max(month), by = "month"),
           fill  = list(valor = 0)) |>
  ungroup() |>
  mutate(
    fecha  = ceiling_date(month, "month") - days(1),
    Month  = month(fecha)
  ) |>
  select(-month) |>
  group_by(institucion, orden) |>
  arrange(fecha) |>
  mutate(
    delta = case_when(
      is.na(valor) ~ 0,
      Month == 1   ~ valor,
      TRUE         ~ valor - lag(valor)
    ),
    sum12 = slide_dbl(delta, .before = 11, .complete = TRUE,
                      .f = ~ sum(.x, na.rm = TRUE))
  ) |>
  ungroup()

# ──────────────────────────────────────────────────────────────────────────────
# 9. Extra metrics: Balance Sheet (12-mo rolling mean of assets) --------------
# ──────────────────────────────────────────────────────────────────────────────
assets_avg <- balance_raw |>
  filter(orden == 10) |>
  arrange(institucion, fecha) |>
  group_by(institucion) |>
  mutate(mean12 = slide_dbl(valor, .before = 11, .complete = TRUE,
                            .f = ~ mean(.x, na.rm = TRUE))) |>
  ungroup()

# # ──────────────────────────────────────────────────────────────────────────────
# # 10. Export final Excel files -------------------------------------------------
# # ──────────────────────────────────────────────────────────────────────────────
# write.xlsx(income_statement, xlsx_income,  overwrite = TRUE)
# write.xlsx(balance_raw,     xlsx_balance, overwrite = TRUE)
# write.xlsx(assets_avg,      xlsx_assets,  overwrite = TRUE)
# 
# message("🎉 All files exported successfully.")
# 

# ──────────────────────────────────────────────────────────────────────────────
# 10. Export final Excel files (with proper date formatting) ------------------
# ──────────────────────────────────────────────────────────────────────────────

# Estilo para fechas
date_style <- createStyle(numFmt = "yyyy-mm-dd")

# Función de exportación robusta con estilo de fecha
export_excel <- function(df, path) {
  wb <- createWorkbook()
  addWorksheet(wb, "Sheet 1")
  writeData(wb, "Sheet 1", df)
  
  if ("fecha" %in% names(df)) {
    col_fecha <- which(names(df) == "fecha")
    addStyle(
      wb,
      sheet = "Sheet 1",
      style = date_style,
      cols = col_fecha,
      rows = 2:(nrow(df) + 1),
      gridExpand = TRUE
    )
  }
  
  saveWorkbook(wb, path, overwrite = TRUE)
}

# Exporta los tres archivos
export_excel(income_statement, xlsx_income)
export_excel(balance_raw,      xlsx_balance)
export_excel(assets_avg,       xlsx_assets)

message("🎉 All files exported successfully (with date formatting).")


