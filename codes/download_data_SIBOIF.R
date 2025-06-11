# Load required libraries using pacman ---------------------------------------------

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(httr, jsonlite, dplyr, openxlsx, lubridate, slider, readxl, tidyr)

# Function to fetch data from SIBOIF API -----------------------------------------
get_siboif_data <- function(superintendency, min_date, max_date, report_type) {
  # Build API URL
  url <- paste0(
    "https://www.siboif.gob.ni/rest/estadisticas",
    "?intendencia=", superintendency,
    "&fecha[min]=", min_date,
    "&fecha[max]=", max_date,
    "&tipo_reporte=", URLencode(report_type)
  )

  # Make API request
  response <- GET(url)

  # Check response status
  if (http_status(response)$category != "Success") {
    stop("Connection error: ", http_status(response)$message)
  }
  
  # Convert JSON response to data.frame if data exists
  data <- content(response, as = "parsed", type = "application/json")
  if (length(data) == 0) stop("No data found for the specified parameters.")
  
  data_frame <- do.call(rbind, lapply(data, as.data.frame))
  
  # Process data (convert values to numeric if needed)
  data_frame %>% 
    mutate(across(starts_with("valor"), ~ as.numeric(gsub(",", "", .))))
}


# Extract banking data ----------------------------------------------------------

superintendency <- "Bancos"
date_segments <- list(
  c("2019-01-01", "2020-12-31"),
  c("2021-01-01", "2022-12-31"),
  c("2023-01-01", "2024-12-31"),
  c("2025-01-01", "2026-12-31")
)

# Get Income Statement data
income_statement_data <- bind_rows(lapply(date_segments, function(dates) {
  get_siboif_data(superintendency, dates[1], dates[2], "Estado de Resultados (ER)")
}))

# Get Balance Sheet data
balance_sheet_data <- bind_rows(lapply(date_segments, function(dates) {
  get_siboif_data(superintendency, dates[1], dates[2], "Estado de Situación Financiera (ESF)")
}))

# Get Income Statement data
loan_portfolio <- bind_rows(lapply(date_segments, function(dates) {
  get_siboif_data(superintendency, dates[1], dates[2], "Estratificación de la Cartera por Actividad y Monto")
}))

# Import exchange rate data
exchange_rate <- read_excel("data/exchange_rate.xlsx")

# Data transformation ----------------------------------------------------------

# Function to process data: rename institutions and calculate values
process_data <- function(data, report_type) {
  data |>
    rename(variable = variable_1) |> 
    mutate(
      fecha = ymd(fecha)
    ) |>
    left_join(exchange_rate, by = "fecha") |>
    mutate(
      valor_1 = as.numeric(valor_1),
      valor = valor_1 / exchange_rate / 1000000,  # Convert to millions
      orden = as.integer(orden),
      institucion = case_when(
        institucion == "BANPRO" ~ "BANPRO",
        institucion == "BANCO LAFISE BANCENTRO" ~ "LAFISE",
        institucion == "BAC" ~ "BAC",
        institucion == "BANCO FICOHSA" ~ "FICOHSA",
        institucion == "BDF" ~ "BDF",
        institucion == "AVANZ" ~ "AVANZ",
        institucion == "BANCO PRODUZCAMOS" ~ "BFP",
        institucion == "Financiera FDL, S.A." ~ "FDL",
        institucion == "FINANCIERA FAMA" ~ "FAMA",
        institucion == "FINANCIERA FINCA NICARAGUA" ~ "FINCA",
        institucion == "FINANCIERA FUNDESER" ~ "FUNDESER",
        institucion == "BANCO ATLÁNTIDA" ~ "ATLÁNTIDA",
        TRUE ~ institucion
      )
    ) |>  
    select(institucion, fecha, orden, valor) |> 
    filter(!institucion %in% c("SFB", "SFN", "SF"))
}

# Process both datasets
income_statement_data <- process_data(income_statement_data, "Income Statement")
balance_sheet_data <- process_data(balance_sheet_data, "Balance Sheet")

# Additional processing for Income Statement (calculate delta and 12-month sum)
income_statement_data <- income_statement_data %>%
  mutate(fecha = ymd(fecha)) %>%
  group_by(institucion, orden) %>%
  arrange(fecha) %>%
  mutate(month = floor_date(fecha, "month")) %>%
  
  # Complete calendar months (NA where missing)
  complete(
    month = seq(min(month), max(month), by = "month"),
    fill = list(valor = 0)
  ) %>%
  ungroup() %>%
  
  # Convert month to last day of month and extract Month
  mutate(
    fecha = ceiling_date(month, "month") - days(1),
    Month = month(fecha)
  ) %>%
  select(-month) %>%
  
  # Calculate delta
  group_by(institucion, orden) %>%
  arrange(fecha) %>%
  mutate(
    delta = case_when(
      is.na(valor)   ~ 0,           # Non-existent month → 0
      Month == 1     ~ valor,       # January → full value
      TRUE           ~ valor - lag(valor)  # Others → difference with previous month
    )
  ) %>%
  
  # Calculate 12-month moving sum
  mutate(
    sum12 = slide_dbl(
      delta,
      .before   = 11,        # 11 previous rows + current = 12
      .after    = 0,
      .complete = TRUE,      # NA if any of the 12 values is NA
      ~ sum(.x, na.rm = TRUE)
    )
  ) %>% 
  arrange(institucion, orden, fecha) %>% 
  ungroup() %>%
  select(institucion, orden, fecha, valor, delta, sum12)

# Additional processing for Balance Sheet (calculate average assets)
assets <- balance_sheet_data %>% 
  filter(orden == 10) %>%
  arrange(institucion, fecha) %>% 
  group_by(institucion) %>%
  mutate(
    mean12 = slide_dbl(
      .x        = valor,
      .before   = 11,
      .after    = 0,
      .complete = TRUE,
      ~ mean(.x, na.rm = TRUE)
    )
  ) %>%
  ungroup()

# Exportar dataframes -----------------------------------------------------
write.xlsx(income_statement_data, "data/Estado de Resultado.xlsx", overwrite = TRUE)
write.xlsx(balance_sheet_data, "data/Estado de Situación Financiera.xlsx", overwrite = TRUE)
write.xlsx(assets, "data/Activos_promedio.xlsx", overwrite = TRUE)

