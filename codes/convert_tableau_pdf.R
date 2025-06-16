# ─────────────────────────────────────────────────────────────────────
# Batch PDF ➜ SVG (vectorial) vía Inkscape CLI
# ─────────────────────────────────────────────────────────────────────

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(fs, purrr, glue)

# Parámetros
input_dir  <- "tableau"                          
output_dir <- fs::path(input_dir)        
fs::dir_create(output_dir)                      

# Verifica Inkscape
ink_path <- Sys.which("inkscape")
if (ink_path == "") stop("❌ No encontré Inkscape en tu PATH")

# Función de conversión
convert_pdf_to_svg <- function(pdf_path) {
  base    <- fs::path_ext_remove(fs::path_file(pdf_path))
  out_svg <- fs::path(output_dir, paste0(base, ".svg"))
  
  # 1) Intento con sintaxis moderna
  args1 <- c(
    pdf_path,
    "--export-type=svg",
    glue("--export-filename={out_svg}")
  )
  suppressWarnings(system2(ink_path, args1, stdout = FALSE, stderr = FALSE))
  if (fs::file_exists(out_svg) && fs::file_info(out_svg)$size > 0) {
    message(glue("✔ '{base}.pdf' ➜ '{base}.svg' (export-type)"))
    return()
  }
  
  # 2) Intento con sintaxis legacy
  args2 <- c(
    pdf_path,
    glue("--export-plain-svg={out_svg}")
  )
  suppressWarnings(system2(ink_path, args2, stdout = FALSE, stderr = FALSE))
  if (fs::file_exists(out_svg) && fs::file_info(out_svg)$size > 0) {
    message(glue("✔ '{base}.pdf' ➜ '{base}.svg' (export-plain-svg)"))
  } else {
    warning(glue("⚠️ Falló la conversión de '{base}.pdf' a SVG"))
  }
}

# Ejecuta sobre todos los PDFs de la carpeta
pdf_files <- fs::dir_ls(input_dir, regexp = "\\.pdf$", recurse = FALSE)
if (length(pdf_files) == 0) stop("No hay archivos PDF en ", input_dir)

purrr::walk(pdf_files, convert_pdf_to_svg)

