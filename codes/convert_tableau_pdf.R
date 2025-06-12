# ─────────────────────────────────────────────────────────────────────────────
# Bulk PDF-to-PNG Converter
# Converts every page of every PDF in a directory to PNG without losing resolution
# Dependencies: pdftools (Poppler backend)   ──────────────────────────────────
# ─────────────────────────────────────────────────────────────────────────────

# 1) Load (or install on-the-fly) required package ----------------------------
if (!requireNamespace("pdftools", quietly = TRUE)) {
  install.packages("pdftools")
}
library(pdftools)  # provides pdf_convert()

# 2) User-defined parameters --------------------------------------------------
input_dir  <- "tableau"   # 📂 Folder containing PDFs
output_dir <- input_dir # 📂 Where PNGs will be written
dpi        <- 600                         # 🖨  Typical print-quality resolution
overwrite  <- TRUE                       # 🔄 Skip already-converted pages?

# 3) Create output directory if it does not exist ----------------------------
if (!dir.exists(output_dir)) dir.create(output_dir)

# 4) Helper: convert one PDF --------------------------------------------------
convert_pdf <- function(pdf_file, dpi, output_dir, overwrite = FALSE) {
  # Build output file pattern: <basename>_page%02d.png
  base <- tools::file_path_sans_ext(basename(pdf_file))
  out_pattern <- file.path(output_dir, paste0(base, "_page%02d.png"))
  
  # Skip if files already exist and overwrite == FALSE
  if (!overwrite) {
    first_target <- sprintf(out_pattern, 1L)
    if (file.exists(first_target)) {
      message(sprintf("Skipping %s (already converted).", base))
      return(invisible(NULL))
    }
  }
  
  # pdf_convert renders & writes images; returns vector of written file names
  png_files <- pdf_convert(
    pdf      = pdf_file,
    format   = "png",
    dpi      = dpi,
    filenames = sprintf(out_pattern, seq_len(pdf_info(pdf_file)$pages))
  )
  message(sprintf("✔ %s ➜ %d page(s) converted", basename(pdf_file), length(png_files)))
}

# 5) Batch process all PDFs ---------------------------------------------------
pdf_files <- list.files(input_dir, pattern = "\\.pdf$", full.names = TRUE, ignore.case = TRUE)

if (length(pdf_files) == 0) {
  stop("No PDF files found in the specified directory.")
}

invisible(lapply(pdf_files, convert_pdf, dpi = dpi, output_dir = output_dir, overwrite = overwrite))

# ─────────────────────────────────────────────────────────────────────────────
# Usage notes:
# • Set `dpi` ≥ 300 for on-screen quality, 600+ for print-ready clarity.
# • Files are named <pdfname>_page01.png, <pdfname>_page02.png, …
# • Each page becomes an independent PNG, preserving original dimensions
#   (scaled by the selected DPI).
# • Poppler must be available; on Windows, pdftools ships pre-compiled binaries.
# ─────────────────────────────────────────────────────────────────────────────
