# Test script to verify gray shading fix for mouse 162593
library(DBI)
library(RSQLite)
library(plotly)

# Database connection
DB_PATH <- "mice_colony_test.db"
con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)

# Get data for mouse 162593
asu_id <- "162593"
body_weight_data <- DBI::dbGetQuery(con, paste0("SELECT * FROM body_weight_history WHERE asu_id = '", asu_id, "' ORDER BY measurement_date"))
plugging_data <- DBI::dbGetQuery(con, paste0("SELECT * FROM plugging_history WHERE (male_id = '", asu_id, "' OR female_id = '", asu_id, "') AND plugging_status != 'Deleted'"))

DBI::dbDisconnect(con)

# Print the data to verify
cat("Body Weight Data for", asu_id, ":\n")
print(body_weight_data)
cat("\nPlugging Data for", asu_id, ":\n")
print(plugging_data)

# Test date range calculation
if (nrow(body_weight_data) > 0) {
  weight_data <- body_weight_data
  weight_data$measurement_date <- as.Date(weight_data$measurement_date)
  
  # Original range (body weight only)
  original_range <- range(weight_data$measurement_date)
  cat("\nOriginal plot range (body weight only):\n")
  cat("From:", as.character(original_range[1]), "to", as.character(original_range[2]), "\n")
  
  # Enhanced range (including pairing periods)
  plot_date_range <- original_range
  
  if (nrow(plugging_data) > 0) {
    for (i in seq_len(nrow(plugging_data))) {
      row <- plugging_data[i, ]
      
      # Check pairing start date
      if (!is.na(row$pairing_start_date) && row$pairing_start_date != "") {
        start_date <- as.Date(row$pairing_start_date)
        if (!is.null(start_date)) {
          plot_date_range[1] <- min(plot_date_range[1], start_date)
        }
      }
      
      # Check pairing end date
      if (!is.na(row$pairing_end_date) && row$pairing_end_date != "") {
        end_date <- as.Date(row$pairing_end_date)
        if (!is.null(end_date)) {
          plot_date_range[2] <- max(plot_date_range[2], end_date)
        }
      }
    }
  }
  
  cat("\nEnhanced plot range (including pairing periods):\n")
  cat("From:", as.character(plot_date_range[1]), "to", as.character(plot_date_range[2]), "\n")
  
  cat("\nPairing period will now be visible:", plot_date_range[1] < original_range[1], "\n")
}
