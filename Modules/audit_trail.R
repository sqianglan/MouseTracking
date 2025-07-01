# audit_trail.R - Enhanced audit trail system for Mouse Management System

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(jsonlite)
})

# Enhanced audit trail table structure
create_enhanced_audit_trail_table <- function(db_path = DB_PATH) {
  con <- dbConnect(RSQLite::SQLite(), db_path)
  
  # Create enhanced audit trail table with more detailed tracking
  dbExecute(con, paste0(
    "CREATE TABLE IF NOT EXISTS audit_trail (",
    "id INTEGER PRIMARY KEY AUTOINCREMENT,",
    "table_name TEXT NOT NULL,",
    "record_id TEXT NOT NULL,",
    "action TEXT NOT NULL CHECK (action IN ('INSERT', 'UPDATE', 'DELETE', 'BULK_UPDATE', 'BULK_DELETE')),",
    "field_name TEXT,",  # For tracking specific field changes
    "old_value TEXT,",   # Old value of the field
    "new_value TEXT,",   # New value of the field
    "old_values TEXT,",  # JSON of all old values (for backward compatibility)
    "new_values TEXT,",  # JSON of all new values (for backward compatibility)
    "timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,",
    "user_id TEXT DEFAULT 'system',",
    "session_id TEXT,",  # To track user sessions
    "ip_address TEXT,",  # To track source IP
    "user_agent TEXT,",  # Browser/client information
    "operation_details TEXT"  # Additional details about the operation
    ,")"
  ))
  
  # Create indexes for better performance
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_audit_table_record ON audit_trail(table_name, record_id)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_audit_timestamp ON audit_trail(timestamp)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_audit_action ON audit_trail(action)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_audit_user ON audit_trail(user_id)")
  
  dbDisconnect(con)
}

# Enhanced audit logging function (compatible with existing modules)
log_audit_action <- function(con, table_name, action, record_id, changed_values = NULL, 
                           user = 'system', session_id = NULL, ip_address = NULL, 
                           user_agent = NULL, operation_details = NULL) {
  tryCatch({
    # For backward compatibility, also store the full JSON
    old_values_json <- ""
    new_values_json <- ""
    
    if (action == "INSERT") {
      new_values_json <- toJSON(changed_values, auto_unbox = TRUE)
    } else if (action == "DELETE") {
      old_values_json <- toJSON(changed_values, auto_unbox = TRUE)
    } else if (action == "UPDATE" && is.list(changed_values)) {
      # For updates, changed_values should be a list with old and new values
      if ("old" %in% names(changed_values) && "new" %in% names(changed_values)) {
        old_values_json <- toJSON(changed_values$old, auto_unbox = TRUE)
        new_values_json <- toJSON(changed_values$new, auto_unbox = TRUE)
      } else {
        # If it's just the new values, store them
        new_values_json <- toJSON(changed_values, auto_unbox = TRUE)
      }
    }
    
    # Insert the audit record
    params_list <- list(
      table_name,
      record_id,
      action,
      old_values_json,
      new_values_json,
      user,
      ifelse(is.null(session_id), NA, session_id),
      ifelse(is.null(ip_address), NA, ip_address),
      ifelse(is.null(user_agent), NA, user_agent),
      ifelse(is.null(operation_details), NA, operation_details)
    )
    
    result <- dbExecute(con, 
      "INSERT INTO audit_trail (table_name, record_id, action, old_values, new_values, timestamp, user_id, session_id, ip_address, user_agent, operation_details) 
       VALUES (?, ?, ?, ?, ?, DATETIME('now'), ?, ?, ?, ?, ?)",
      params = params_list
    )
    
    return(TRUE)
    
  }, error = function(e) {
    # Log the error but don't fail the main operation
    cat("Audit trail logging failed:", e$message, "\n")
    return(FALSE)
  })
}

# Compatible audit trail function for existing modules (like tab_plugging.R)
log_audit_trail <- function(table_name, record_id, action, old_values, new_values, user_id = 'system') {
  con <- dbConnect(RSQLite::SQLite(), DB_PATH)
  tryCatch({
    # Use the enhanced function but maintain compatibility
    log_audit_action(con, table_name, action, record_id, 
                    if (action == "INSERT") new_values else list(old = old_values, new = new_values),
                    user = user_id, operation_details = paste("Legacy audit trail call for", action))
  }, finally = {
    dbDisconnect(con)
  })
}

# Enhanced audit logging for field-level changes
log_field_change <- function(con, table_name, record_id, field_name, old_value, new_value,
                           user = 'system', session_id = NULL, ip_address = NULL, 
                           user_agent = NULL, operation_details = NULL) {
  tryCatch({
    # Only log if values are actually different
    if (is.null(old_value) && is.null(new_value)) return(TRUE)
    if (!is.null(old_value) && !is.null(new_value) && old_value == new_value) return(TRUE)
    
    # Convert values to strings for storage
    old_value_str <- ifelse(is.null(old_value), "", as.character(old_value))
    new_value_str <- ifelse(is.null(new_value), "", as.character(new_value))
    
    dbExecute(con, 
      "INSERT INTO audit_trail (table_name, record_id, action, field_name, old_value, new_value, timestamp, user_id, session_id, ip_address, user_agent, operation_details) 
       VALUES (?, ?, 'UPDATE', ?, ?, ?, DATETIME('now'), ?, ?, ?, ?, ?)",
      params = list(
        table_name,
        record_id,
        field_name,
        old_value_str,
        new_value_str,
        user,
        session_id,
        ip_address,
        user_agent,
        operation_details
      )
    )
    
    return(TRUE)
  }, error = function(e) {
    cat("Field-level audit logging failed:", e$message, "\n")
    return(FALSE)
  })
}

# Bulk audit logging for multiple records
log_bulk_audit_action <- function(con, table_name, action, record_ids, changed_values = NULL,
                                user = 'system', session_id = NULL, ip_address = NULL, 
                                user_agent = NULL, operation_details = NULL) {
  tryCatch({
    # Log each record individually for detailed tracking
    for (record_id in record_ids) {
      log_audit_action(con, table_name, action, record_id, changed_values, 
                      user, session_id, ip_address, user_agent, operation_details)
    }
    
    # Also log a summary record
    summary_details <- paste0("Bulk operation: ", length(record_ids), " records affected. Record IDs: ", 
                             paste(record_ids, collapse = ", "))
    
    dbExecute(con, 
      "INSERT INTO audit_trail (table_name, record_id, action, operation_details, timestamp, user_id, session_id, ip_address, user_agent) 
       VALUES (?, 'BULK_OPERATION', ?, ?, DATETIME('now'), ?, ?, ?, ?)",
      params = list(
        table_name,
        paste0("BULK_", action),
        summary_details,
        user,
        session_id,
        ip_address,
        user_agent
      )
    )
    
    return(TRUE)
  }, error = function(e) {
    cat("Bulk audit logging failed:", e$message, "\n")
    return(FALSE)
  })
}

# Enhanced function to get modification history for a mouse (compatible with existing usage)
get_mouse_modification_history <- function(asu_id, db_path = DB_PATH, limit = 100) {
  con <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  
  # Get all plugging_history IDs where this mouse is involved
  plugging_ids <- dbGetQuery(con, "SELECT id FROM plugging_history WHERE male_id = ? OR female_id = ?", 
                            params = list(asu_id, asu_id))$id
  
  if (length(plugging_ids) == 0) {
    # Only query for mice_stock
    query <- paste0(
      "SELECT * FROM audit_trail WHERE (table_name = 'mice_stock' AND record_id = ?) ",
      "ORDER BY timestamp DESC LIMIT ?"
    )
    params <- list(asu_id, limit)
  } else {
    # Query for both mice_stock and plugging_history
    in_clause <- paste(rep("?", length(plugging_ids)), collapse = ",")
    query <- paste0(
      "SELECT * FROM audit_trail WHERE (table_name = 'mice_stock' AND record_id = ?) ",
      "OR (table_name = 'plugging_history' AND record_id IN (", in_clause, ")) ",
      "ORDER BY timestamp DESC LIMIT ?"
    )
    params <- c(asu_id, plugging_ids, limit)
  }
  
  history <- dbGetQuery(con, query, params = params)
  
  # Format the history for better display
  if (nrow(history) > 0) {
    tz <- tryCatch({
      if (exists('user_timezone', envir = .GlobalEnv)) {
        user_timezone <- get('user_timezone', envir = .GlobalEnv)
        if (is.function(user_timezone)) user_timezone() else user_timezone
      } else if (!is.null(getOption('user_timezone'))) {
        getOption('user_timezone')
      } else {
        Sys.timezone()
      }
    }, error = function(e) Sys.timezone())
    history$timestamp <- as.POSIXct(history$timestamp, format = "%Y-%m-%d %H:%M:%S", tz = 'UTC')
    history$formatted_time <- format(history$timestamp, tz = tz, usetz = TRUE, '%d-%b-%Y %H:%M:%S')
    
    # Parse JSON values if they exist
    history$old_values_parsed <- sapply(history$old_values, function(x) {
      if (x != "" && !is.na(x)) {
        tryCatch({
          parsed <- fromJSON(x)
          if (is.list(parsed) && length(parsed) > 0) {
            paste(names(parsed), "=", unlist(parsed), collapse = ", ")
          } else {
            x
          }
        }, error = function(e) x)
      } else {
        ""
      }
    })
    
    history$new_values_parsed <- sapply(history$new_values, function(x) {
      if (x != "" && !is.na(x)) {
        tryCatch({
          parsed <- fromJSON(x)
          if (is.list(parsed) && length(parsed) > 0) {
            paste(names(parsed), "=", unlist(parsed), collapse = ", ")
          } else {
            x
          }
        }, error = function(e) x)
      } else {
        ""
      }
    })
  }
  
  return(history)
}

# Function to get audit trail summary
get_audit_summary <- function(db_path = DB_PATH, days = 30) {
  con <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  
  # Get summary of recent audit activities
  query <- paste0(
    "SELECT ",
    "table_name, ",
    "action, ",
    "COUNT(*) as count, ",
    "MAX(timestamp) as last_activity ",
    "FROM audit_trail ",
    "WHERE timestamp >= datetime('now', '-", days, " days') ",
    "GROUP BY table_name, action ",
    "ORDER BY table_name, action"
  )
  
  summary <- dbGetQuery(con, query)
  
  # Get user activity summary
  user_query <- paste0(
    "SELECT ",
    "user_id, ",
    "COUNT(*) as actions, ",
    "MAX(timestamp) as last_activity ",
    "FROM audit_trail ",
    "WHERE timestamp >= datetime('now', '-", days, " days') ",
    "GROUP BY user_id ",
    "ORDER BY actions DESC"
  )
  
  user_summary <- dbGetQuery(con, user_query)
  
  return(list(
    table_summary = summary,
    user_summary = user_summary
  ))
}

# Function to get detailed audit trail with filtering
get_detailed_audit_trail <- function(db_path = DB_PATH, 
                                   table_name = NULL, 
                                   record_id = NULL, 
                                   action = NULL, 
                                   user_id = NULL, 
                                   start_date = NULL, 
                                   end_date = NULL, 
                                   limit = 1000) {
  con <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  
  # Build the WHERE clause
  where_conditions <- c()
  params <- list()
  
  if (!is.null(table_name)) {
    where_conditions <- c(where_conditions, "table_name = ?")
    params <- c(params, table_name)
  }
  
  if (!is.null(record_id)) {
    where_conditions <- c(where_conditions, "record_id = ?")
    params <- c(params, record_id)
  }
  
  if (!is.null(action)) {
    where_conditions <- c(where_conditions, "action = ?")
    params <- c(params, action)
  }
  
  if (!is.null(user_id)) {
    where_conditions <- c(where_conditions, "user_id = ?")
    params <- c(params, user_id)
  }
  
  if (!is.null(start_date)) {
    where_conditions <- c(where_conditions, "timestamp >= ?")
    params <- c(params, start_date)
  }
  
  if (!is.null(end_date)) {
    where_conditions <- c(where_conditions, "timestamp <= ?")
    params <- c(params, end_date)
  }
  
  # Build the complete query
  query <- "SELECT * FROM audit_trail"
  if (length(where_conditions) > 0) {
    query <- paste0(query, " WHERE ", paste(where_conditions, collapse = " AND "))
  }
  query <- paste0(query, " ORDER BY timestamp DESC LIMIT ?")
  params <- c(params, limit)
  
  # Execute the query
  audit_data <- dbGetQuery(con, query, params = params)
  
  # Format the data
  if (nrow(audit_data) > 0) {
    audit_data$timestamp <- as.POSIXct(audit_data$timestamp)
    audit_data$formatted_time <- format(audit_data$timestamp, "%d-%b-%Y %H:%M:%S")
  }
  
  return(audit_data)
}

# Function to clean old audit trail records
clean_old_audit_records <- function(db_path = DB_PATH, days_to_keep = 365) {
  con <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  
  # Delete records older than specified days
  query <- paste0(
    "DELETE FROM audit_trail WHERE timestamp < datetime('now', '-", days_to_keep, " days')"
  )
  
  result <- dbExecute(con, query)
  
  # Get count of remaining records
  remaining_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM audit_trail")$count
  
  return(list(
    deleted_count = result,
    remaining_count = remaining_count
  ))
}

# Function to export audit trail to CSV
export_audit_trail <- function(db_path = DB_PATH, 
                              start_date = NULL, 
                              end_date = NULL, 
                              table_name = NULL) {
  con <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  
  # Get the audit data
  audit_data <- get_detailed_audit_trail(db_path, table_name, 
                                        start_date = start_date, 
                                        end_date = end_date, 
                                        limit = 100000) # Large limit for export
  
  # Format for export
  export_data <- audit_data[, c("id", "table_name", "record_id", "action", "field_name", 
                               "old_value", "new_value", "timestamp", "user_id", 
                               "session_id", "ip_address", "user_agent", "operation_details")]
  
  return(export_data)
}

# Initialize the enhanced audit trail table
initialize_enhanced_audit_trail <- function(db_path = DB_PATH) {
  create_enhanced_audit_trail_table(db_path)
}

# Initialize audit trail system (compatibility function)
initialize_audit_trail <- function(db_path = DB_PATH) {
  initialize_enhanced_audit_trail(db_path)
}

# New function to get modification history for a plugging_history record
get_plugging_modification_history <- function(plugging_id, db_path = DB_PATH, limit = 100) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  tryCatch({
    # First, get the female_id from the plugging record
    female_id <- DBI::dbGetQuery(con, "SELECT female_id FROM plugging_history WHERE id = ?", params = list(plugging_id))
    
    if (nrow(female_id) == 0) {
      return(data.frame(
        id = integer(0),
        table_name = character(0),
        record_id = character(0),
        action = character(0),
        old_values = character(0),
        new_values = character(0),
        timestamp = character(0),
        user_id = character(0),
        formatted_time = character(0),
        old_values_parsed = character(0),
        new_values_parsed = character(0),
        stringsAsFactors = FALSE
      ))
    }
    
    female_id <- female_id$female_id[1]
    
    # Get audit trail records for both plugging_history and mice_stock (for the female mouse)
    history <- DBI::dbGetQuery(con, 
      "SELECT * FROM audit_trail WHERE (table_name = 'plugging_history' AND record_id = ?) OR (table_name = 'mice_stock' AND record_id = ?) ORDER BY timestamp DESC LIMIT ?", 
      params = list(plugging_id, female_id, limit))
    
    if (nrow(history) > 0) {
      # Get timezone
      tz <- tryCatch({
        if (exists('user_timezone', envir = .GlobalEnv)) {
          user_timezone <- get('user_timezone', envir = .GlobalEnv)
          if (is.function(user_timezone)) user_timezone() else user_timezone
        } else if (!is.null(getOption('user_timezone'))) {
          getOption('user_timezone')
        } else {
          Sys.timezone()
        }
      }, error = function(e) Sys.timezone())
      
      # Parse timestamps more robustly
      history$timestamp <- tryCatch({
        # Try different timestamp formats
        if (all(grepl("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$", history$timestamp))) {
          as.POSIXct(history$timestamp, format = "%Y-%m-%d %H:%M:%S", tz = 'UTC')
        } else if (all(grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}", history$timestamp))) {
          as.POSIXct(history$timestamp, tz = 'UTC')
        } else {
          # Fallback to as.POSIXct with default parsing
          as.POSIXct(history$timestamp, tz = 'UTC')
        }
      }, error = function(e) {
        cat("Warning: Error parsing timestamps:", e$message, "\n")
        # Return original timestamps as character if parsing fails
        history$timestamp
      })
      
      # Format timestamps for display
      history$formatted_time <- tryCatch({
        if (inherits(history$timestamp, "POSIXct")) {
          format(history$timestamp, tz = tz, usetz = TRUE, '%d-%b-%Y %H:%M:%S')
        } else {
          as.character(history$timestamp)
        }
      }, error = function(e) {
        cat("Warning: Error formatting timestamps:", e$message, "\n")
        as.character(history$timestamp)
      })
      
      # Parse JSON values more robustly
      history$old_values_parsed <- sapply(history$old_values, function(x) {
        if (x != "" && !is.na(x)) {
          tryCatch({
            parsed <- jsonlite::fromJSON(x)
            if (is.list(parsed) && length(parsed) > 0) {
              paste(names(parsed), "=", unlist(parsed), collapse = ", ")
            } else {
              x
            }
          }, error = function(e) {
            cat("Warning: Error parsing old_values JSON:", e$message, "\n")
            x
          })
        } else {
          ""
        }
      })
      
      history$new_values_parsed <- sapply(history$new_values, function(x) {
        if (x != "" && !is.na(x)) {
          tryCatch({
            parsed <- jsonlite::fromJSON(x)
            if (is.list(parsed) && length(parsed) > 0) {
              paste(names(parsed), "=", unlist(parsed), collapse = ", ")
            } else {
              x
            }
          }, error = function(e) {
            cat("Warning: Error parsing new_values JSON:", e$message, "\n")
            x
          })
        } else {
          ""
        }
      })
    }
    
    return(history)
  }, error = function(e) {
    cat("Error in get_plugging_modification_history:", e$message, "\n")
    # Return empty data frame with correct structure
    data.frame(
      id = integer(0),
      table_name = character(0),
      record_id = character(0),
      action = character(0),
      old_values = character(0),
      new_values = character(0),
      timestamp = character(0),
      user_id = character(0),
      formatted_time = character(0),
      old_values_parsed = character(0),
      new_values_parsed = character(0),
      stringsAsFactors = FALSE
    )
  })
} 