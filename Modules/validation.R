# validation.R - Comprehensive data validation for Mouse Management System

library(stringr)
library(lubridate)




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
  
  # Check for SQL injection patterns
  if (grepl("['\"\\-;]", asu_id)) {
    return(list(valid = FALSE, message = "ASU ID contains invalid characters"))
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
  
  # Check for SQL injection patterns
  if (grepl("['\"\\-;]", animal_id)) {
    return(list(valid = FALSE, message = "Animal ID contains invalid characters"))
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
  if (is.null(dob) || dob == "") {
    return(list(valid = FALSE, message = "Date of Birth is required"))
  }
  
  # Try to parse the date
  parsed_date <- tryCatch({
    as.Date(dob)
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(parsed_date)) {
    return(list(valid = FALSE, message = "Invalid date format. Use YYYY-MM-DD"))
  }
  
  # Check if date is in the future
  if (parsed_date > Sys.Date()) {
    return(list(valid = FALSE, message = "Date of Birth cannot be in the future"))
  }
  
  # Check if date is too far in the past (more than 5 years)
  if (parsed_date < Sys.Date() - years(5)) {
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
  
  # Check for SQL injection patterns
  if (grepl("['\"\\-;]", breeding_line)) {
    return(list(valid = FALSE, message = "Breeding line contains invalid characters"))
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
  
  # Check for SQL injection patterns
  if (grepl("['\"\\-;]", genotype)) {
    return(list(valid = FALSE, message = "Genotype contains invalid characters"))
  }
  
  return(list(valid = TRUE, message = "Valid genotype"))
}

validate_transgenes <- function(transgenes) {
  if (is.null(transgenes) || transgenes == "") {
    return(list(valid = TRUE, message = "Transgenes is optional"))
  }
  
  transgenes <- trimws(transgenes)
  
  # Check length
  if (nchar(transgenes) > 200) {
    return(list(valid = FALSE, message = "Transgenes must be 200 characters or less"))
  }
  
  # Check for SQL injection patterns
  if (grepl("['\"\\-;]", transgenes)) {
    return(list(valid = FALSE, message = "Transgenes contains invalid characters"))
  }
  
  return(list(valid = TRUE, message = "Valid transgenes"))
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
  
  # Check for SQL injection patterns
  if (grepl("['\"\\-;]", responsible_person)) {
    return(list(valid = FALSE, message = "Responsible person contains invalid characters"))
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
  
  # Check for SQL injection patterns
  if (grepl("['\"\\-;]", project_code)) {
    return(list(valid = FALSE, message = "Project code contains invalid characters"))
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
  
  # Check for SQL injection patterns
  if (grepl("['\"\\-;]", cage_id)) {
    return(list(valid = FALSE, message = "Cage ID contains invalid characters"))
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
  
  # Check for SQL injection patterns
  if (grepl("['\"\\-;]", room)) {
    return(list(valid = FALSE, message = "Room contains invalid characters"))
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
  
  # Check for SQL injection patterns
  if (grepl("['\"\\-;]", notes)) {
    return(list(valid = FALSE, message = "Notes contain invalid characters"))
  }
  
  return(list(valid = TRUE, message = "Valid notes"))
}

# Comprehensive validation function for mouse data
validate_mouse_data <- function(data) {
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
    } else {
      errors[[field]] <- paste("Required field", field, "is missing")
    }
  }
  
  # Validate optional fields
  optional_fields <- c("animal_id", "ear_mark", "breeding_line", "genotype", 
                      "transgenes", "responsible_person", "project_code", 
                      "protocol", "cage_id", "room", "notes")
  
  for (field in optional_fields) {
    if (field %in% names(data) && !is.null(data[[field]]) && data[[field]] != "") {
      validation_result <- switch(field,
        "animal_id" = validate_animal_id(data[[field]]),
        "ear_mark" = validate_animal_id(data[[field]]), # Same validation as animal_id
        "breeding_line" = validate_breeding_line(data[[field]]),
        "genotype" = validate_genotype(data[[field]]),
        "transgenes" = validate_transgenes(data[[field]]),
        "responsible_person" = validate_responsible_person(data[[field]]),
        "project_code" = validate_project_code(data[[field]]),
        "protocol" = validate_protocol(data[[field]]),
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
  
  # Check for duplicate ASU ID if this is a new record
  if ("asu_id" %in% names(data) && !is.null(data$asu_id) && data$asu_id != "") {
    # This check should be done at the database level, but we can add a warning here
    warnings[["asu_id"]] <- "Please ensure ASU ID is unique in the database"
  }
  
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
    valid_statuses <- c("Ongoing", "Plugged", "Empty", "Deleted", "Confirmed", "Not Observed (Waiting for confirmation)", "Not Observed (Confirmed)")
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