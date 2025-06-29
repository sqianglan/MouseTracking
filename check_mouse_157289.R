# Script to check plugging history for mouse 157289
# Run this script to analyze the plugging history and modifications

library(DBI)
library(RSQLite)
library(jsonlite)

# Database path - update this to your actual database path
DB_PATH <- "mice_colony_test.db"

# Function to safely parse JSON
safe_parse_json <- function(json_string) {
  if (is.null(json_string) || json_string == "" || is.na(json_string)) {
    return(list())
  }
  tryCatch({
    fromJSON(json_string)
  }, error = function(e) {
    cat("Error parsing JSON:", e$message, "\n")
    return(list())
  })
}

# Function to format timestamp
format_timestamp <- function(timestamp) {
  if (is.null(timestamp) || is.na(timestamp)) return("N/A")
  tryCatch({
    as.character(as.POSIXct(timestamp, format="%Y-%m-%d %H:%M:%S"))
  }, error = function(e) {
    as.character(timestamp)
  })
}

cat("=== MOUSE 157289 PLUGGING HISTORY ANALYSIS ===\n\n")

# Connect to database
con <- dbConnect(SQLite(), DB_PATH)

tryCatch({
  
  # 1. Check if mouse 157289 exists in mice_stock
  cat("1. CHECKING MOUSE 157289 BASIC INFORMATION:\n")
  cat("===========================================\n")
  
  mouse_info <- dbGetQuery(con, "SELECT * FROM mice_stock WHERE asu_id = ?", params = list("157289"))
  
  if (nrow(mouse_info) == 0) {
    cat("❌ Mouse 157289 not found in mice_stock table!\n\n")
  } else {
    cat("✅ Mouse 157289 found!\n")
    cat("   ASU ID:", mouse_info$asu_id[1], "\n")
    cat("   Animal ID:", mouse_info$animal_id[1], "\n")
    cat("   Gender:", mouse_info$gender[1], "\n")
    cat("   Breeding Line:", mouse_info$breeding_line[1], "\n")
    cat("   Genotype:", mouse_info$genotype[1], "\n")
    cat("   Status:", mouse_info$status[1], "\n")
    cat("   Date of Birth:", mouse_info$dob[1], "\n")
    if (!is.na(mouse_info$date_of_death[1])) {
      cat("   Date of Death:", mouse_info$date_of_death[1], "\n")
    }
    cat("   Last Updated:", format_timestamp(mouse_info$last_updated[1]), "\n\n")
  }
  
  # 2. Check plugging records involving mouse 157289
  cat("2. CHECKING PLUGGING RECORDS INVOLVING MOUSE 157289:\n")
  cat("==================================================\n")
  
  plugging_records <- dbGetQuery(con, 
    "SELECT * FROM plugging_history 
     WHERE male_id = ? OR female_id = ? 
     ORDER BY created_at DESC", 
    params = list("157289", "157289"))
  
  if (nrow(plugging_records) == 0) {
    cat("❌ No plugging records found for mouse 157289!\n\n")
  } else {
    cat("✅ Found", nrow(plugging_records), "plugging record(s):\n\n")
    
    for (i in 1:nrow(plugging_records)) {
      record <- plugging_records[i, ]
      cat("   Record ID:", record$id, "\n")
      cat("   Role:", ifelse(record$male_id == "157289", "Male", "Female"), "\n")
      cat("   Partner:", ifelse(record$male_id == "157289", record$female_id, record$male_id), "\n")
      cat("   Pairing Start:", record$pairing_start_date, "\n")
      cat("   Pairing End:", record$pairing_end_date, "\n")
      cat("   Plug Observed:", record$plug_observed_date, "\n")
      cat("   Status:", record$plugging_status, "\n")
      cat("   Expected Harvesting Age:", record$expected_age_for_harvesting, "\n")
      cat("   Notes:", ifelse(is.na(record$notes) || record$notes == "", "None", record$notes), "\n")
      cat("   Created:", format_timestamp(record$created_at), "\n")
      cat("   Updated:", format_timestamp(record$updated_at), "\n")
      cat("   ---\n")
    }
    cat("\n")
  }
  
  # 3. Check audit trail for mouse 157289
  cat("3. CHECKING AUDIT TRAIL FOR MOUSE 157289:\n")
  cat("=========================================\n")
  
  # Get plugging IDs for this mouse
  plugging_ids <- if (nrow(plugging_records) > 0) {
    plugging_records$id
  } else {
    integer(0)
  }
  
  # Build audit trail query
  if (length(plugging_ids) > 0) {
    placeholders <- paste(rep("?", length(plugging_ids)), collapse = ",")
    audit_query <- paste0(
      "SELECT * FROM audit_trail 
       WHERE (table_name = 'mice_stock' AND record_id = ?) 
          OR (table_name = 'plugging_history' AND record_id IN (", placeholders, ")) 
       ORDER BY timestamp DESC"
    )
    audit_params <- c("157289", plugging_ids)
  } else {
    audit_query <- "SELECT * FROM audit_trail WHERE table_name = 'mice_stock' AND record_id = ? ORDER BY timestamp DESC"
    audit_params <- list("157289")
  }
  
  audit_records <- dbGetQuery(con, audit_query, params = audit_params)
  
  if (nrow(audit_records) == 0) {
    cat("❌ No audit trail records found for mouse 157289!\n\n")
  } else {
    cat("✅ Found", nrow(audit_records), "audit trail record(s):\n\n")
    
    for (i in 1:nrow(audit_records)) {
      record <- audit_records[i, ]
      cat("   Record ID:", record$id, "\n")
      cat("   Table:", record$table_name, "\n")
      cat("   Action:", record$action, "\n")
      cat("   User:", record$user_id, "\n")
      cat("   Timestamp:", format_timestamp(record$timestamp), "\n")
      
      # Parse old and new values
      old_vals <- safe_parse_json(record$old_values)
      new_vals <- safe_parse_json(record$new_values)
      
      if (record$table_name == "mice_stock") {
        if (!is.null(old_vals$status) && !is.null(new_vals$status) && old_vals$status != new_vals$status) {
          cat("   Status Change:", old_vals$status, "→", new_vals$status, "\n")
        }
        if (!is.null(new_vals$source)) {
          cat("   Source:", new_vals$source, "\n")
        }
        if (!is.null(new_vals$notes)) {
          cat("   Notes:", new_vals$notes, "\n")
        }
      } else if (record$table_name == "plugging_history") {
        if (!is.null(old_vals$plugging_status) && !is.null(new_vals$plugging_status) && old_vals$plugging_status != new_vals$plugging_status) {
          cat("   Plugging Status Change:", old_vals$plugging_status, "→", new_vals$plugging_status, "\n")
        }
        if (!is.null(new_vals$notes)) {
          cat("   Notes:", new_vals$notes, "\n")
        }
      }
      
      cat("   ---\n")
    }
    cat("\n")
  }
  
  # 4. Summary
  cat("4. SUMMARY:\n")
  cat("===========\n")
  
  if (nrow(mouse_info) > 0) {
    cat("✅ Mouse 157289 exists in the database\n")
    cat("   Current Status:", mouse_info$status[1], "\n")
    cat("   Gender:", mouse_info$gender[1], "\n")
  } else {
    cat("❌ Mouse 157289 not found in database\n")
  }
  
  cat("   Plugging Records:", nrow(plugging_records), "\n")
  cat("   Audit Trail Entries:", nrow(audit_records), "\n")
  
  if (nrow(plugging_records) > 0) {
    cat("\n   Plugging Status Timeline:\n")
    for (i in 1:nrow(plugging_records)) {
      record <- plugging_records[i, ]
      cat("     ", format_timestamp(record$created_at), ": ", record$plugging_status, "\n")
    }
  }
  
  cat("\n=== ANALYSIS COMPLETE ===\n")
  
}, error = function(e) {
  cat("❌ Error accessing database:", e$message, "\n")
  cat("   Please check if the database file exists and is accessible.\n")
}, finally = {
  dbDisconnect(con)
}) 