#!/usr/bin/env Rscript

# Script to fix mixed timestamp formats in mice_stock table
library(DBI)
library(RSQLite)

# Database path
DB_PATH <- "mice_colony_test.db"

# Connect to database
con <- dbConnect(SQLite(), DB_PATH)

# Function to format timestamp
format_timestamp <- function(timestamp) {
  if (is.null(timestamp) || is.na(timestamp) || timestamp == "") {
    return(NULL)
  }
  # Handle Unix timestamp (seconds since epoch)
  if (is.numeric(timestamp) && timestamp > 1000000000) {
    return(format(as.POSIXct(timestamp, origin = "1970-01-01"), "%d-%b-%Y %H:%M:%S"))
  }
  # Handle string timestamps
  if (is.character(timestamp)) {
    # Try to parse as POSIXct
    parsed <- tryCatch({
      as.POSIXct(timestamp)
    }, error = function(e) {
      # If it's a Unix timestamp string, convert to numeric first
      if (grepl("^\\d+\\.?\\d*$", timestamp)) {
        as.POSIXct(as.numeric(timestamp), origin = "1970-01-01")
      } else {
        NA
      }
    })
    if (!is.na(parsed)) {
      return(format(parsed, "%d-%b-%Y %H:%M:%S"))
    }
  }
  return(as.character(timestamp))
}

# Get all records with timestamp issues
cat("Checking for timestamp issues...\n")
records <- dbGetQuery(con, "SELECT asu_id, date_created, last_updated FROM mice_stock")

# Check for mixed formats
mixed_formats <- records[!is.na(records$date_created) | !is.na(records$last_updated), ]
cat("Found", nrow(mixed_formats), "records with timestamps\n")

# Show some examples
cat("\nSample timestamps:\n")
for (i in 1:min(5, nrow(mixed_formats))) {
  row <- mixed_formats[i, ]
  cat("ASU ID:", row$asu_id, 
      "| date_created:", row$date_created, "(", typeof(row$date_created), ")",
      "| last_updated:", row$last_updated, "(", typeof(row$last_updated), ")\n")
}

# Fix timestamps
cat("\nFixing timestamps...\n")
for (i in 1:nrow(mixed_formats)) {
  row <- mixed_formats[i, ]
  
  # Format date_created
  if (!is.na(row$date_created)) {
    formatted_created <- format_timestamp(row$date_created)
    if (!is.null(formatted_created)) {
      dbExecute(con, "UPDATE mice_stock SET date_created = ? WHERE asu_id = ?", 
                params = list(formatted_created, row$asu_id))
    }
  }
  
  # Format last_updated
  if (!is.na(row$last_updated)) {
    formatted_updated <- format_timestamp(row$last_updated)
    if (!is.null(formatted_updated)) {
      dbExecute(con, "UPDATE mice_stock SET last_updated = ? WHERE asu_id = ?", 
                params = list(formatted_updated, row$asu_id))
    }
  }
}

cat("Timestamp fix completed!\n")

# Show results
cat("\nUpdated timestamps:\n")
updated_records <- dbGetQuery(con, "SELECT asu_id, date_created, last_updated FROM mice_stock LIMIT 5")
for (i in 1:nrow(updated_records)) {
  row <- updated_records[i, ]
  cat("ASU ID:", row$asu_id, 
      "| date_created:", row$date_created,
      "| last_updated:", row$last_updated, "\n")
}

dbDisconnect(con)
cat("\nDatabase connection closed.\n") 