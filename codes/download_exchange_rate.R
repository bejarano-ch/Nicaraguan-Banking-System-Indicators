# Load required packages using pacman
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readxl, httr, tidyverse, writexl)

# URL of the Excel file
url <- "https://www.bcn.gob.ni/sites/default/files/estadisticas/monetario_financiero/monetario/monetario_mensual/4-23.xlsx"

# Create a temporary file to store the downloaded Excel
temp_file <- tempfile(fileext = ".xlsx")

# Download the file with error handling
tryCatch({
  # Download the file using httr
  response <- GET(url)
  if (http_status(response)$category == "Success") {
    writeBin(content(response, "raw"), temp_file)
    
    # Read the Excel file
    data <- read_excel(temp_file)
    
    # Print the first few rows to verify the data
    print(head(data))
  } else {
    stop("Failed to download file: ", http_status(response)$message)
  }
  
  # Clean up - remove the temporary file
  unlink(temp_file)
  
}, error = function(e) {
  cat("Error:", conditionMessage(e), "\n")
  # Clean up in case of error
  if (file.exists(temp_file)) unlink(temp_file)
})

# Process the data
df <- data %>%
  select(1, 4) %>%
  rename(fecha = 1, exchange_rate = 2) %>%
  filter(!is.na(fecha)) %>%
  mutate(across(everything(), ~ifelse(is.na(.) | . == "", NA, .))) %>%
  # Filter out numeric values in fecha column
  filter(!grepl("^\\d+$", fecha)) %>%
  drop_na() %>% 
  mutate(fecha = seq(as.Date("2002-01-01"), by = "month", length.out = n())) %>% 
  complete(fecha = seq(as.Date("2002-01-01"), as.Date("2025-12-01"), by = "month")) %>%
  mutate(fecha = ceiling_date(fecha, "month") - days(1)) %>%
  fill(exchange_rate) %>% 
  mutate(exchange_rate=parse_number(exchange_rate))


# Export to Excel
write_xlsx(df, "data/exchange_rate.xlsx")


