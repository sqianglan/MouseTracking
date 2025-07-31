# validation.R - Comprehensive data validation for Mouse Management System

suppressPackageStartupMessages({
  library(stringr)
  library(lubridate)
})




# Enhanced validation functions
validate_asu_id <- function(asu_id) {
  if (is.null(asu_id) || asu_id == "") {
    return(list(valid = FALSE, message = "ASU ID is required"))
  }
  
  # Remove any whitespace
  asu_id <- trimws(asu_id)
  
  # Check length (typically 4-10 characters)
  if (nchar(asu_id) < 3 || nchar(asu_id) > 15) {
    return(list(valid = FALSE, message = "ASU ID must be between 3 and 15 characters"))
  }
  
  # Check for valid characters (alphanumeric and common separators)
  if (!grepl("^[A-Za-z0-9_-]+$", asu_id)) {
    return(list(valid = FALSE, message = "ASU ID can only contain letters, numbers, hyphens, and underscores"))
  }
  

  
  return(list(valid = TRUE, message = "Valid ASU ID"))
}

validate_animal_id <- function(animal_id) {
  if (is.null(animal_id) || animal_id == "") {
    return(list(valid = TRUE, message = "Animal ID is optional")) # Optional field
  }
  
  animal_id <- trimws(animal_id)
  
  # Check length
  if (nchar(animal_id) > 50) {
    return(list(valid = FALSE, message = "Animal ID must be 50 characters or less"))
  }
  

  
  return(list(valid = TRUE, message = "Valid Animal ID"))
}

validate_gender <- function(gender) {
  if (is.null(gender) || gender == "") {
    return(list(valid = FALSE, message = "Gender is required"))
  }
  
  valid_genders <- c("Male", "Female", "male", "female", "M", "F", "m", "f")
  if (!gender %in% valid_genders) {
    return(list(valid = FALSE, message = "Gender must be 'Male' or 'Female'"))
  }
  
  # Standardize to proper case
  if (tolower(gender) %in% c("male", "m")) {
    return(list(valid = TRUE, message = "Valid gender", standardized = "Male"))
  } else {
    return(list(valid = TRUE, message = "Valid gender", standardized = "Female"))
  }
}

validate_date_of_birth <- function(dob) {
  # Handle all possible empty/null cases first
  if (is.null(dob) || length(dob) == 0) {
    return(list(valid = FALSE, message = "Date of Birth is required"))
  }
  
  # Check for NA values safely
  if (length(dob) > 0 && all(is.na(dob))) {
    return(list(valid = FALSE, message = "Date of Birth is required"))
  }
  
  # Convert to character to handle various input types
  dob_char <- as.character(dob)
  if (dob_char == "" || dob_char == "NA") {
    return(list(valid = FALSE, message = "Date of Birth is required"))
  }
  
  # Try to parse the date
  parsed_date <- tryCatch({
    as.Date(dob)
  }, error = function(e) {
    return(as.Date(NA))
  })
  
  if (length(parsed_date) == 0 || all(is.na(parsed_date))) {
    return(list(valid = FALSE, message = "Invalid date format. Use YYYY-MM-DD"))
  }
  
  # Check if date is in the future (safely)
  current_date <- Sys.Date()
  if (any(is.na(parsed_date)) || any(parsed_date > current_date, na.rm = TRUE)) {
    return(list(valid = FALSE, message = "Date of Birth cannot be in the future"))
  }
  
  # Check if date is too far in the past (more than 5 years)
  past_limit <- current_date - years(5)
  if (any(is.na(parsed_date)) || any(parsed_date < past_limit, na.rm = TRUE)) {
    return(list(valid = FALSE, message = "Date of Birth seems too far in the past (more than 5 years)"))
  }
  
  return(list(valid = TRUE, message = "Valid date", standardized = as.character(parsed_date)))
}

validate_breeding_line <- function(breeding_line) {
  if (is.null(breeding_line) || breeding_line == "") {
    return(list(valid = TRUE, message = "Breeding line is optional"))
  }
  
  breeding_line <- trimws(breeding_line)
  
  # Check length
  if (nchar(breeding_line) > 100) {
    return(list(valid = FALSE, message = "Breeding line must be 100 characters or less"))
  }
  

  
  return(list(valid = TRUE, message = "Valid breeding line"))
}

validate_genotype <- function(genotype) {
  if (is.null(genotype) || genotype == "") {
    return(list(valid = TRUE, message = "Genotype is optional"))
  }
  
  genotype <- trimws(genotype)
  
  # Check length
  if (nchar(genotype) > 100) {
    return(list(valid = FALSE, message = "Genotype must be 100 characters or less"))
  }
  

  
  return(list(valid = TRUE, message = "Valid genotype"))
}

validate_responsible_person <- function(responsible_person) {
  if (is.null(responsible_person) || responsible_person == "") {
    return(list(valid = TRUE, message = "Responsible person is optional"))
  }
  
  responsible_person <- trimws(responsible_person)
  
  # Check length
  if (nchar(responsible_person) > 100) {
    return(list(valid = FALSE, message = "Responsible person must be 100 characters or less"))
  }
  

  
  return(list(valid = TRUE, message = "Valid responsible person"))
}

validate_project_code <- function(project_code) {
  if (is.null(project_code) || project_code == "") {
    return(list(valid = TRUE, message = "Project code is optional"))
  }
  
  project_code <- trimws(project_code)
  
  # Check length
  if (nchar(project_code) > 50) {
    return(list(valid = FALSE, message = "Project code must be 50 characters or less"))
  }
  

  
  return(list(valid = TRUE, message = "Valid project code"))
}

validate_protocol <- function(protocol) {
  if (is.null(protocol) || protocol == "") {
    return(list(valid = TRUE, message = "Protocol is optional"))
  }
  
  valid_protocols <- c(
    "1 (Breeding and maintenance of genetically altered animals)",
    "2 (Epithelial stem cell fate and dynamics during tissue development and regeneration)",
    "3 (Mouse tumor model)"
  )
  
  if (!protocol %in% valid_protocols) {
    return(list(valid = FALSE, message = "Please select a valid protocol"))
  }
  
  return(list(valid = TRUE, message = "Valid protocol"))
}

validate_stock_category <- function(stock_category) {
  if (is.null(stock_category) || stock_category == "") {
    return(list(valid = FALSE, message = "Stock category is required"))
  }
  
  valid_categories <- c("Experiment", "Breeding", "Charles River")
  
  if (!stock_category %in% valid_categories) {
    return(list(valid = FALSE, message = "Stock category must be 'Experiment', 'Breeding', or 'Charles River'"))
  }
  
  return(list(valid = TRUE, message = "Valid stock category"))
}

validate_status <- function(status) {
  if (is.null(status) || status == "") {
    return(list(valid = FALSE, message = "Status is required"))
  }
  
  valid_statuses <- c("Alive", "Deceased", "Deleted")
  
  if (!status %in% valid_statuses) {
    return(list(valid = FALSE, message = "Status must be 'Alive', 'Deceased', or 'Deleted'"))
  }
  
  return(list(valid = TRUE, message = "Valid status"))
}

validate_cage_id <- function(cage_id) {
  if (is.null(cage_id) || cage_id == "") {
    return(list(valid = TRUE, message = "Cage ID is optional"))
  }
  
  cage_id <- trimws(cage_id)
  
  # Check length
  if (nchar(cage_id) > 20) {
    return(list(valid = FALSE, message = "Cage ID must be 20 characters or less"))
  }
  

  
  return(list(valid = TRUE, message = "Valid cage ID"))
}

validate_room <- function(room) {
  if (is.null(room) || room == "") {
    return(list(valid = TRUE, message = "Room is optional"))
  }
  
  room <- trimws(room)
  
  # Check length
  if (nchar(room) > 20) {
    return(list(valid = FALSE, message = "Room must be 20 characters or less"))
  }
  

  
  return(list(valid = TRUE, message = "Valid room"))
}

validate_notes <- function(notes) {
  if (is.null(notes) || notes == "") {
    return(list(valid = TRUE, message = "Notes are optional"))
  }
  
  notes <- trimws(notes)
  
  # Check length
  if (nchar(notes) > 500) {
    return(list(valid = FALSE, message = "Notes must be 500 characters or less"))
  }
  

  
  return(list(valid = TRUE, message = "Valid notes"))
}

validate_study_plan <- function(study_plan) {
  if (is.null(study_plan) || study_plan == "") {
    return(list(valid = TRUE, message = "Study plan is optional"))
  }
  
  study_plan <- trimws(study_plan)
  
  # Check for valid study plan values
  valid_study_plans <- c("SP2500090", "SP2500083", "SP2500082", "SP2500081")
  if (!study_plan %in% valid_study_plans) {
    return(list(valid = FALSE, message = paste("Study plan must be one of:", paste(valid_study_plans, collapse = ", "))))
  }
  
  return(list(valid = TRUE, message = "Valid study plan"))
}

# Comprehensive validation function for mouse data
validate_mouse_data <- function(data, require_all_fields = TRUE) {
  errors <- list()
  warnings <- list()
  standardized_data <- list()
  
  # Validate required fields
  required_fields <- c("asu_id", "gender", "dob", "stock_category", "status")
  
  for (field in required_fields) {
    if (field %in% names(data)) {
      validation_result <- switch(field,
        "asu_id" = validate_asu_id(data[[field]]),
        "gender" = validate_gender(data[[field]]),
        "dob" = validate_date_of_birth(data[[field]]),
        "stock_category" = validate_stock_category(data[[field]]),
        "status" = validate_status(data[[field]])
      )
      
      if (!validation_result$valid) {
        errors[[field]] <- validation_result$message
      } else if (!is.null(validation_result$standardized)) {
        standardized_data[[field]] <- validation_result$standardized
      }
    } else if (require_all_fields) {
      errors[[field]] <- paste("Required field", field, "is missing")
    }
  }
  
  # Validate optional fields
  optional_fields <- c("animal_id", "ear_mark", "breeding_line", "genotype", 
                      "responsible_person", "project_code", 
                      "protocol", "study_plan", "cage_id", "room", "notes")
  
  for (field in optional_fields) {
    if (field %in% names(data) && !is.null(data[[field]]) && data[[field]] != "") {
      validation_result <- switch(field,
        "animal_id" = validate_animal_id(data[[field]]),
        "ear_mark" = validate_animal_id(data[[field]]), # Same validation as animal_id
        "breeding_line" = validate_breeding_line(data[[field]]),
        "genotype" = validate_genotype(data[[field]]),
        "responsible_person" = validate_responsible_person(data[[field]]),
        "project_code" = validate_project_code(data[[field]]),
        "protocol" = validate_protocol(data[[field]]),
        "study_plan" = validate_study_plan(data[[field]]),
        "cage_id" = validate_cage_id(data[[field]]),
        "room" = validate_room(data[[field]]),
        "notes" = validate_notes(data[[field]])
      )
      
      if (!validation_result$valid) {
        errors[[field]] <- validation_result$message
      } else if (!is.null(validation_result$standardized)) {
        standardized_data[[field]] <- validation_result$standardized
      }
    }
  }
  
  # Note: ASU ID uniqueness is now checked dynamically in the UI, so no warning needed here
  
  return(list(
    valid = length(errors) == 0,
    errors = errors,
    warnings = warnings,
    standardized_data = standardized_data
  ))
}

# Validation function for breeding data
validate_breeding_data <- function(data) {
  errors <- list()
  warnings <- list()
  
  # Validate required fields
  if (is.null(data$male_id) || data$male_id == "") {
    errors[["male_id"]] <- "Male ID is required"
  } else {
    male_validation <- validate_asu_id(data$male_id)
    if (!male_validation$valid) {
      errors[["male_id"]] <- male_validation$message
    }
  }
  
  if (is.null(data$female1_id) || data$female1_id == "") {
    errors[["female1_id"]] <- "Female 1 ID is required"
  } else {
    female_validation <- validate_asu_id(data$female1_id)
    if (!female_validation$valid) {
      errors[["female1_id"]] <- female_validation$message
    }
  }
  
  # Validate dates
  if (!is.null(data$start_date) && data$start_date != "") {
    date_validation <- validate_date_of_birth(data$start_date)
    if (!date_validation$valid) {
      errors[["start_date"]] <- date_validation$message
    }
  }
  
  if (!is.null(data$end_date) && data$end_date != "") {
    date_validation <- validate_date_of_birth(data$end_date)
    if (!date_validation$valid) {
      errors[["end_date"]] <- date_validation$message
    }
  }
  
  # Check if end_date is after start_date
  if (!is.null(data$start_date) && !is.null(data$end_date) && 
      data$start_date != "" && data$end_date != "") {
    start_date <- as.Date(data$start_date)
    end_date <- as.Date(data$end_date)
    if (end_date < start_date) {
      errors[["date_range"]] <- "End date cannot be before start date"
    }
  }
  
  return(list(
    valid = length(errors) == 0,
    errors = errors,
    warnings = warnings
  ))
}

# Validation function for plugging data
validate_plugging_data <- function(data) {
  errors <- list()
  warnings <- list()
  
  # Validate required fields
  if (is.null(data$male_id) || data$male_id == "") {
    errors[["male_id"]] <- "Male ID is required"
  } else {
    male_validation <- validate_asu_id(data$male_id)
    if (!male_validation$valid) {
      errors[["male_id"]] <- male_validation$message
    }
  }
  
  if (is.null(data$female_id) || data$female_id == "") {
    errors[["female_id"]] <- "Female ID is required"
  } else {
    female_validation <- validate_asu_id(data$female_id)
    if (!female_validation$valid) {
      errors[["female_id"]] <- female_validation$message
    }
  }
  
  # Validate dates
  if (!is.null(data$pairing_start_date) && data$pairing_start_date != "") {
    date_validation <- validate_date_of_birth(data$pairing_start_date)
    if (!date_validation$valid) {
      errors[["pairing_start_date"]] <- date_validation$message
    }
  }
  
  if (!is.null(data$pairing_end_date) && data$pairing_end_date != "") {
    date_validation <- validate_date_of_birth(data$pairing_end_date)
    if (!date_validation$valid) {
      errors[["pairing_end_date"]] <- date_validation$message
    }
  }
  
  if (!is.null(data$plug_observed_date) && data$plug_observed_date != "") {
    # Allow "Unknown" as a valid value
    if (data$plug_observed_date == "Unknown") {
      # Skip date validation for "Unknown"
    } else {
      date_validation <- validate_date_of_birth(data$plug_observed_date)
      if (!date_validation$valid) {
        errors[["plug_observed_date"]] <- date_validation$message
      }
    }
  }
  
  # Validate plugging status
  if (!is.null(data$plugging_status) && data$plugging_status != "") {
    valid_statuses <- c("Ongoing", "Plugged", "Empty", "Deleted", "Plug Confirmed", "Not Pregnant", "Not Observed (Waiting for confirmation)", "Not Observed (Confirmed)", "Surprising Plug!!", "Collected")
    if (!data$plugging_status %in% valid_statuses) {
      errors[["plugging_status"]] <- "Invalid plugging status"
    }
  }
  
  return(list(
    valid = length(errors) == 0,
    errors = errors,
    warnings = warnings
  ))
}

# Helper function to display validation errors in Shiny
display_validation_errors <- function(validation_result) {
  if (validation_result$valid) {
    return(NULL)
  }
  
  error_messages <- sapply(validation_result$errors, function(error) {
    paste0("• ", error)
  })
  
  error_html <- paste(error_messages, collapse = "<br>")
  
  return(div(
    style = "color: #d32f2f; background-color: #ffebee; padding: 10px; border-radius: 4px; margin: 10px 0;",
    tags$strong("Validation Errors:"),
    tags$br(),
    HTML(error_html)
  ))
}

# Helper function to display validation warnings in Shiny
display_validation_warnings <- function(validation_result) {
  if (length(validation_result$warnings) == 0) {
    return(NULL)
  }
  
  warning_messages <- sapply(validation_result$warnings, function(warning) {
    paste0("• ", warning)
  })
  
  warning_html <- paste(warning_messages, collapse = "<br>")
  
  return(div(
    style = "color: #f57c00; background-color: #fff3e0; padding: 10px; border-radius: 4px; margin: 10px 0;",
    tags$strong("Warnings:"),
    tags$br(),
    HTML(warning_html)
  ))
}

# validation function for mice status in plugging_history
validate_mice_active_status <- function(mice_id, table_name, status_column, mouse_role = NULL) {
  # input validation is not necessary here because the input is already validated in the calling function
  
  # Connect to database
  con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  
  tryCatch({
    # Determine the mouse ID column based on table name and mouse role
    mouse_id_column <- switch(table_name,
      "plugging_history" = {
        # For plugging_history, check mouse_role to determine column
        if (!is.null(mouse_role) && mouse_role == "male") {
          "male_id"  # For males, check male_id column
        } else {
          "female_id"  # For females or default, check female_id column
        }
      },
      "breeding_history" = "female1_id", # For breeding_history, we check female1_id
      "mice_stock" = "asu_id",           # For mice_stock, we check asu_id
      "asu_id"  # Default fallback
    )
    
    # Query to get all records for this mouse
    all_records_query <- paste0(
      "SELECT ", status_column, " as status ",
      "FROM ", table_name, " ",
      "WHERE ", mouse_id_column, " = ?"
    )
    
    all_records <- DBI::dbGetQuery(con, all_records_query, params = list(mice_id))
    
    # Check if mouse exists in the table at all
    if (nrow(all_records) == 0) {
      return(list(
        valid = FALSE, 
        message = paste("Mouse", mice_id, "not found in table", table_name),
        record_count = 0,
        all_statuses = character(0),
        status_summary = "No records found"
      ))
    }
    
    # Get all statuses and create summary
    all_statuses <- all_records$status
    status_summary <- table(all_statuses)
    status_details <- paste(names(status_summary), "(", status_summary, ")", collapse = ", ")
    
    return(list(
      valid = TRUE,
      message = paste("Mouse", mice_id, "found in", table_name, "with statuses:", status_details),
      record_count = nrow(all_records),
      all_statuses = all_statuses,
      status_summary = status_details
    ))
    
  }, error = function(e) {
    return(list(
      valid = FALSE, 
      message = paste("Database error:", e$message),
      record_count = 0,
      all_statuses = character(0),
      status_summary = "Error occurred"
    ))
  }, finally = {
    DBI::dbDisconnect(con)
  })
}

# Global function to get live mice by gender (reactive version)
get_live_mice_by_gender <- function(gender = NULL, global_refresh_trigger = NULL) {
  if (is.null(global_refresh_trigger)) {
    # Non-reactive version - direct database call
    con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
    
    tryCatch({
      if (is.null(gender)) {
        # Return all live mice (both genders)
        mice <- DBI::dbGetQuery(con, 
          "SELECT asu_id, animal_id, gender, breeding_line, genotype 
           FROM mice_stock 
           WHERE status = 'Alive' 
           ORDER BY asu_id")
        
        mice
      } else {
        # Filter by specific gender
        mice <- DBI::dbGetQuery(con, 
          "SELECT asu_id, animal_id, gender, breeding_line, genotype 
           FROM mice_stock 
           WHERE status = 'Alive' AND gender = ?
           ORDER BY asu_id",
          params = list(gender))
        
        mice
      }
    }, finally = {
      DBI::dbDisconnect(con)
    })
  } else {
    # Reactive version - responds to global refresh trigger
    reactive({
      # Add dependency on global refresh trigger
      global_refresh_trigger()
      
      con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
      tryCatch({
        if (is.null(gender)) {
          # Return all live mice (both genders)
          mice <- DBI::dbGetQuery(con, 
            "SELECT asu_id, animal_id, gender, breeding_line, genotype 
             FROM mice_stock 
             WHERE status = 'Alive' 
             ORDER BY asu_id")
          
          mice
        } else {
          # Filter by specific gender
          mice <- DBI::dbGetQuery(con, 
            "SELECT asu_id, animal_id, gender, breeding_line, genotype 
             FROM mice_stock 
             WHERE status = 'Alive' AND gender = ?
             ORDER BY asu_id",
            params = list(gender))
          
          mice
        }
      }, finally = {
        DBI::dbDisconnect(con)
      })
    })
  }
}

# Global function to get mouse info (unified version)
get_mouse_info <- function(asu_id, include_status = FALSE) {
  if (is.null(asu_id) || asu_id == "") return(NULL)
  
  con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  tryCatch({
    # Build query based on whether status is needed
    if (include_status) {
      query <- "SELECT asu_id, dob, breeding_line, genotype, gender, status
                FROM mice_stock 
                WHERE asu_id = ? 
                LIMIT 1"
    } else {
      query <- "SELECT asu_id, dob, breeding_line, genotype
                FROM mice_stock 
                WHERE asu_id = ? 
                LIMIT 1"
    }
    
    mouse <- DBI::dbGetQuery(con, query, params = list(asu_id))
    
    if (nrow(mouse) == 0) return(NULL)
    
    # Calculate age in weeks
    mouse$age_weeks <- round(as.numeric(Sys.Date() - as.Date(mouse$dob)) / 7, 1)
    return(mouse)
  }, finally = {
    DBI::dbDisconnect(con)
  })
}

# Function to check if mouse has body weight records
has_body_weight_records <- function(asu_id) {
  if (is.null(asu_id) || asu_id == "") return(FALSE)
  
  con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  tryCatch({
    # Check if body_weight_history table exists
    tables <- DBI::dbListTables(con)
    if (!"body_weight_history" %in% tables) {
      return(FALSE)
    }
    
    # Check if mouse has any body weight records
    query <- "SELECT COUNT(*) as count FROM body_weight_history WHERE asu_id = ?"
    result <- DBI::dbGetQuery(con, query, params = list(asu_id))
    
    return(result$count > 0)
  }, error = function(e) {
    return(FALSE)
  }, finally = {
    DBI::dbDisconnect(con)
  })
}

# Function to determine mouse status tag for all mice table
mice_status_tag_all_mice <- function(asu_id) {
  if (is.null(asu_id) || asu_id == "") return("Unknown")
  
  con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  tryCatch({
    # First check if mouse exists and get basic status
    mouse_info <- DBI::dbGetQuery(con, 
      "SELECT asu_id, gender, status FROM mice_stock WHERE asu_id = ? LIMIT 1", 
      params = list(asu_id))
    
    if (nrow(mouse_info) == 0) {
      return("Unknown")
    }
    
    # If mouse is not alive, return "Deceased"
    if (mouse_info$status != "Alive") {
      return("Deceased")
    }
    
    # Check active plugging records
    mouse_role <- ifelse(mouse_info$gender == "Male", "male", "female")
    
    # Determine status based on active plugging records
    if (mouse_role == "male") {
      # Males are "Busy" if they have 2 or more "Ongoing" pairings.
      active_statuses <- c("Ongoing")
      active_plugging_query <- paste0(
        "SELECT COUNT(*) as active_count 
         FROM plugging_history 
         WHERE male_id = ? AND plugging_status IN ('", paste(active_statuses, collapse = "', '"), "')"
      )
      active_count <- DBI::dbGetQuery(con, active_plugging_query, params = list(asu_id))$active_count
      
      if (active_count >= 2) {
        return("Busy")
      } else {
        return("Free")
      }
      
    } else { # female
      # Females are "Busy" if they have any active plugging/confirmation event.
      active_statuses <- c("Ongoing", "Plugged", "Plug Confirmed", "Not Observed (Waiting for confirmation)", "Surprising Plug!!")
      active_plugging_query <- paste0(
        "SELECT COUNT(*) as active_count 
         FROM plugging_history 
         WHERE female_id = ? AND plugging_status IN ('", paste(active_statuses, collapse = "', '"), "')"
      )
      active_count <- DBI::dbGetQuery(con, active_plugging_query, params = list(asu_id))$active_count
      
      if (active_count >= 1) {
        return("Busy")
      } else {
        return("Free")
      }
    }
    
  }, error = function(e) {
    # Return "Unknown" if there's an error
    return("Unknown")
  }, finally = {
    DBI::dbDisconnect(con)
  })
}


