# modal_add_animal.R
# Functions related to Add Animals modal and functionality

# Source required modules
source(file.path("Modules", "validation.R"))
source(file.path("Modules", "db_check.R"))
source(file.path("Modules", "audit_trail.R"))

suppressPackageStartupMessages({
  library(shiny)
  library(DT)
  library(DBI)
  library(RSQLite)
  library(readxl)
})

# Function to show the main Add Animals modal
show_add_animals_modal <- function() {
  showModal(modalDialog(
    title = "Add Animals",
    div(
      actionButton("add_single_entry_btn", "Single Entry", style = "margin-right: 16px; background-color: #90caf9; color: #222; border: none; font-size: 1.1em; padding: 8px 24px;"),
      actionButton("add_import_excel_btn", "Import from Excel", style = "background-color: #a5d6a7; color: #222; border: none; font-size: 1.1em; padding: 8px 24px;"),
      style = "display: flex; justify-content: center; gap: 16px; margin-top: 12px; margin-bottom: 12px;"
    ),
    footer = modalButton("Close")
  ))
}

# Function to show the single entry form
show_single_entry_form <- function(DB_PATH, TABLE_NAME) {
  # Get existing values from database for dropdowns
  con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  breeding_lines <- unique(DBI::dbGetQuery(con, paste0("SELECT DISTINCT breeding_line FROM ", TABLE_NAME, " WHERE breeding_line IS NOT NULL"))$breeding_line)
  genotypes <- unique(DBI::dbGetQuery(con, paste0("SELECT DISTINCT genotype FROM ", TABLE_NAME, " WHERE genotype IS NOT NULL"))$genotype)
  responsible_persons <- unique(DBI::dbGetQuery(con, paste0("SELECT DISTINCT responsible_person FROM ", TABLE_NAME, " WHERE responsible_person IS NOT NULL"))$responsible_person)
  project_codes <- unique(DBI::dbGetQuery(con, paste0("SELECT DISTINCT project_code FROM ", TABLE_NAME, " WHERE project_code IS NOT NULL"))$project_code)
  DBI::dbDisconnect(con)
  
  showModal(modalDialog(
    title = "Add Single Animal",
    size = "l",
    fluidRow(
      column(6, textInput("single_entry_asu_id", "ASU ID *", placeholder = "Enter ASU ID")),
      column(6, textInput("single_entry_animal_id", "Animal ID", placeholder = "Enter Animal ID"))
    ),
    fluidRow(
      column(6, textInput("single_entry_ear_mark", "Ear Mark", placeholder = "Enter ear mark")),
      column(6, selectInput("single_entry_gender", "Gender *", choices = c("", "Male", "Female"), selected = ""))
    ),
    fluidRow(
      column(6, dateInput("single_entry_dob", "Date of Birth *", value = NULL)),
      column(6, selectizeInput("single_entry_breeding_line", "Breeding Line", 
                              choices = c("", breeding_lines), 
                              options = list(create = TRUE, placeholder = "Select or type new")))
    ),
    fluidRow(
      column(6, selectizeInput("single_entry_genotype", "Genotype", 
                              choices = c("", genotypes), 
                              options = list(create = TRUE, placeholder = "Select or type new"))),
      column(6, selectizeInput("single_entry_breeding_line", "Breeding Line", 
                              choices = c("", breeding_lines), 
                              options = list(create = TRUE, placeholder = "Select or type new")))
    ),
   
    fluidRow(
      column(6, selectInput("single_entry_status", "Status", 
                           choices = c("Alive", "Deceased"), 
                           selected = "Alive")),
      column(6, selectizeInput("single_entry_responsible_person", "Responsible Person", 
                              choices = c("", responsible_persons), 
                              options = list(create = TRUE, placeholder = "Select or type new")))
    ),
    fluidRow(
      column(6, selectInput("single_entry_protocol", "Protocol", 
                            choices = c("", 
                                        "1 (Breeding and maintenance of genetically altered animals)",
                                        "2 (Epithelial stem cell fate and dynamics during tissue development and regeneration)",
                                        "3 (Mouse tumor model)"), 
                            selected = "")),
      column(6, selectizeInput("single_entry_project_code", "Project Code", 
                              choices = c("", project_codes), 
                              options = list(create = TRUE, placeholder = "Select or type new"))),
    ),
    fluidRow(
      column(6, selectInput("single_entry_study_plan", "Study Plan", 
                            choices = c("", "SP2500090", "SP2500083", "SP2500082", "SP2500081"), 
                            selected = "SP2500090")),
      column(6, selectInput("single_entry_stock_category", "Stock Category", 
                            choices = c("Experiment", "Breeding", "Charles River"), 
                            selected = "Experiment")),
    ),
    div(
      style = "margin-top: 15px; font-size: 12px; color: #666;",
      "* Required fields"
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("submit_single_entry_btn", "Add Animal", style = "background-color: #1976d2; color: white; border: none;")
    )
  ))
}

# Function to show the import from Excel modal
show_import_excel_modal <- function() {
  showModal(modalDialog(
    title = "Import Animals from Excel",
    size = "s",
    fileInput("import_excel_file", "Choose Excel File", accept = c(".xls", ".xlsx")),
    selectInput("import_stock_category", "Stock Category for Imported Records", 
                choices = c("Experiment", "Breeding", "Charles River"), 
                selected = "Experiment"),
    div(
      style = "margin-top: 10px; font-size: 12px; color: #666;",
      "Note: This stock category will be applied to all imported records."
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("submit_import_excel_btn", "Import", style = "background-color: #1976d2; color: white; border: none;")
    )
  ))
}

# Function to handle Excel import processing
handle_excel_import <- function(input, import_data) {
  req(input$import_excel_file)
  file_path <- input$import_excel_file$datapath
  library(readxl)
  df <- tryCatch(readxl::read_excel(file_path), error = function(e) NULL)
  if (is.null(df)) {
    showModal(modalDialog(title = "Import Error", "Failed to read Excel file.", easyClose = TRUE))
    return(NULL)
  }
  
  # Get column mappings with suggestions
  mapping_result <- tryCatch(parse_excel_to_mice_stock(df), error = function(e) NULL)
  if (is.null(mapping_result)) {
    showModal(modalDialog(title = "Import Error", "Failed to analyze Excel file columns.", easyClose = TRUE))
    return(NULL)
  }
  
  # Filter out excluded columns before storing
  excluded_columns <- c(
    "Age", "age", "AGE",
    "No. of animals", "No of animals", "Number of animals", "Count", "Num", "Animals",
    "Team", "TEAM", "team", 
    "Cage Type", "Cage type", "cage type", "CageType", "CAGE TYPE"
  )
  
  df_columns <- names(df)
  columns_to_keep <- df_columns[!sapply(df_columns, function(col) {
    col_lower <- tolower(col)
    any(sapply(excluded_columns, function(excl) {
      excl_lower <- tolower(excl)
      grepl(excl_lower, col_lower, fixed = TRUE) || grepl(col_lower, excl_lower, fixed = TRUE)
    }))
  })]
  
  # Store the filtered data for later use
  import_data$df <- df[, columns_to_keep, drop = FALSE]
  import_data$mappings <- mapping_result$mappings
  import_data$available_columns <- mapping_result$available_columns
  import_data$field_suggestions <- mapping_result$field_suggestions
  
  # Create column mapping UI using function from db_check.R
  mapping_ui <- create_column_mapping_ui(mapping_result, import_data$df)
  
  showModal(modalDialog(
    title = "Confirm Column Mappings",
    size = "xl",
    div(
      style = "width: 100%; max-width: none;",
      mapping_ui
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirm_mappings_btn", "Confirm and Import", style = "background-color: #1976d2; color: white; border: none;")
    ),
    # Add custom CSS to make modal extra wide
    tags$head(tags$style(HTML("
      .modal-xl {
        max-width: 98% !important;
        width: 98% !important;
      }
      .modal-xl .modal-content {
        width: 100% !important;
      }
      .modal-xl .modal-body {
        padding: 15px !important;
      }
    ")))
  ))
  
  return(import_data)
}

# Function to handle mapping confirmation
handle_mapping_confirmation <- function(input, import_data) {
  # Get the column names from the imported data
  col_names <- names(import_data$df)
  
  # First, validate for duplicate mappings
  validation_result <- validate_column_mappings(input, length(col_names))
  
  if (!validation_result$is_valid) {
    # Show warning modal for duplicate mappings with option to go back
    showModal(modalDialog(
      title = "Duplicate Column Mappings",
      create_duplicate_mapping_warning(validation_result$duplicate_fields),
      easyClose = FALSE,
      footer = tagList(
        actionButton("go_back_to_mapping", "Go Back to Fix Mappings", 
                    style = "background-color: #1976d2; color: white; border: none;"),
        modalButton("Cancel Import")
      )
    ))
    return(NULL)
  }
  
  # Collect all mappings from the new column-based UI
  confirmed_mappings <- list()
  
  # Check required fields first
  required_fields <- names(import_data$field_suggestions$required)
  # Remove asu_id from required fields since it's automatically extracted
  required_fields <- required_fields[required_fields != "asu_id"]
  missing_required <- c()
  
  # Collect mappings from column dropdowns
  for (i in seq_along(col_names)) {
    input_id <- paste0("mapping_col_", i)
    selected_value <- input[[input_id]]
    
    if (!is.null(selected_value) && selected_value != "NA") {
      # Map the database field to the Excel column name
      confirmed_mappings[[selected_value]] <- col_names[i]
    }
  }
  
  # Check if required fields are mapped
  for (field in required_fields) {
    if (!field %in% names(confirmed_mappings)) {
      missing_required <- c(missing_required, field)
    }
  }
  
  # Check if required fields are missing
  if (length(missing_required) > 0) {
    showModal(modalDialog(
      title = "Missing Required Fields",
      paste("The following required fields must be mapped:", paste(missing_required, collapse = ", ")),
      easyClose = TRUE
    ))
    return(NULL)
  }
  
  # Apply mappings and create data frame
  parsed_df <- tryCatch(
    apply_mappings_and_create_df(import_data$df, confirmed_mappings, input$import_stock_category), 
    error = function(e) NULL
  )
  
  if (is.null(parsed_df)) {
    showModal(modalDialog(title = "Import Error", "Failed to parse Excel data with confirmed mappings.", easyClose = TRUE))
    return(NULL)
  }
  
  return(parsed_df)
}

# Function to handle single entry submission
handle_single_entry_submission <- function(input, DB_PATH, TABLE_NAME) {
  # Collect all input data
  input_data <- list(
    asu_id = input$single_entry_asu_id,
    animal_id = input$single_entry_animal_id,
    ear_mark = input$single_entry_ear_mark,
    gender = input$single_entry_gender,
    dob = input$single_entry_dob,
    genotype = input$single_entry_genotype,
    breeding_line = input$single_entry_breeding_line,
    project_code = input$single_entry_project_code,
    responsible_person = input$single_entry_responsible_person,
    protocol = input$single_entry_protocol,
    study_plan = input$single_entry_study_plan,
    stock_category = input$single_entry_stock_category,
    status = input$single_entry_status
  )
  
  # Validate the input data
  validation_result <- validate_mouse_data(input_data)
  
  if (!validation_result$valid) {
    # Show validation errors
    error_html <- display_validation_errors(validation_result)
    showModal(modalDialog(
      title = "Validation Errors",
      error_html,
      easyClose = TRUE
    ))
    return(FALSE)
  }
  
  # Show warnings if any
  if (length(validation_result$warnings) > 0) {
    warning_html <- display_validation_warnings(validation_result)
    showModal(modalDialog(
      title = "Validation Warnings",
      warning_html,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("proceed_with_warnings", "Proceed Anyway", 
                    style = "background-color: #ff9800; color: white; border: none;")
      )
    ))
    return(FALSE)
  }
  
  # Apply standardized values from validation
  if (length(validation_result$standardized_data) > 0) {
    for (field in names(validation_result$standardized_data)) {
      input_data[[field]] <- validation_result$standardized_data[[field]]
    }
  }
  
  # Check if ASU ID already exists
  con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  existing_asu_id <- DBI::dbGetQuery(con, paste0("SELECT asu_id FROM ", TABLE_NAME, " WHERE asu_id = ?"), params = list(input_data$asu_id))
  
  if (nrow(existing_asu_id) > 0) {
    DBI::dbDisconnect(con)
    showModal(modalDialog(title = "Error", "ASU ID already exists in the database.", easyClose = TRUE))
    return(FALSE)
  }
  
  # Insert into database with audit trail logging
  result <- tryCatch({
    # Use direct SQL INSERT to properly set timestamps
    DBI::dbExecute(con, 
      "INSERT INTO mice_stock (
        asu_id, animal_id, ear_mark, gender, dob, genotype, transgenes, strain, 
        breeding_line, dam, sire, cage_id, room, project_code, responsible_person, 
        protocol, study_plan, stock_category, status, date_of_death, age_at_death_weeks, 
        max_severity, procedure, stage, deceased_timestamp, notes, imported_from_excel, 
        date_created, last_updated
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, DATETIME('now'), DATETIME('now'))",
      params = list(
        input_data$asu_id,
        if (is.null(input_data$animal_id) || input_data$animal_id == "") NA else input_data$animal_id,
        NA, # ear_mark
        input_data$gender,
        as.character(input_data$dob),
        if (is.null(input_data$genotype) || input_data$genotype == "") NA else input_data$genotype,
        NA, # transgenes
        'C57BL/6J', # strain
        if (is.null(input_data$breeding_line) || input_data$breeding_line == "") NA else input_data$breeding_line,
        NA, # dam
        NA, # sire
        NA, # cage_id
        NA, # room
        if (is.null(input_data$project_code) || input_data$project_code == "") NA else input_data$project_code,
        if (is.null(input_data$responsible_person) || input_data$responsible_person == "") NA else input_data$responsible_person,
        if (is.null(input_data$protocol) || input_data$protocol == "") NA else input_data$protocol,
        if (is.null(input_data$study_plan) || input_data$study_plan == "") 'SP2500090' else input_data$study_plan,
        input_data$stock_category,
        input_data$status,
        NA, # date_of_death
        NA, # age_at_death_weeks
        NA, # max_severity
        NA, # procedure
        NA, # stage
        NA, # deceased_timestamp
        NA, # notes
        FALSE # imported_from_excel
      )
    )
    
    # Log the audit trail
    log_audit_action(con, TABLE_NAME, "INSERT", input_data$asu_id, 
                    list(
                      asu_id = input_data$asu_id,
                      animal_id = if (is.null(input_data$animal_id) || input_data$animal_id == "") NA else input_data$animal_id,
                      gender = input_data$gender,
                      dob = as.character(input_data$dob),
                      genotype = if (is.null(input_data$genotype) || input_data$genotype == "") NA else input_data$genotype,
                      breeding_line = if (is.null(input_data$breeding_line) || input_data$breeding_line == "") NA else input_data$breeding_line,
                      project_code = if (is.null(input_data$project_code) || input_data$project_code == "") NA else input_data$project_code,
                      responsible_person = if (is.null(input_data$responsible_person) || input_data$responsible_person == "") NA else input_data$responsible_person,
                      protocol = if (is.null(input_data$protocol) || input_data$protocol == "") NA else input_data$protocol,
                      study_plan = if (is.null(input_data$study_plan) || input_data$study_plan == "") 'SP2500090' else input_data$study_plan,
                      stock_category = input_data$stock_category,
                      status = input_data$status
                    ), 
                    user = 'system', 
                    operation_details = "Single entry via UI")
    
    TRUE
  }, error = function(e) {
    FALSE
  })
  DBI::dbDisconnect(con)
  
  if (result) {
    showModal(modalDialog(title = "Success", "Animal added successfully!", easyClose = TRUE))
    removeModal() # Close the single entry modal
    return(TRUE)
  } else {
    showModal(modalDialog(title = "Error", "Failed to add animal to database.", easyClose = TRUE))
    return(FALSE)
  }
}

# Function to show duplicates modal with detailed comparison
show_duplicates_modal <- function(duplicate_check, import_data) {
  
  # Create the duplicate conflicts table if there are comparison records
  duplicate_table_ui <- NULL
  if (nrow(duplicate_check$comparison_data) > 0) {
    duplicate_table_ui <- create_duplicate_conflicts_html_table(import_data)
  }
  
  showModal(modalDialog(
    title = "Duplicate Records Found - Detailed Comparison",
    size = "l",
    div(
      h4("Please review and choose action for each duplicate:"),
      div(
        "Actions: Skip = Don't import, Modify = Generate new ASU ID, Overwrite = Replace DB record, Keep Both = Import with new ASU ID",
        style = "margin-bottom: 10px; font-size: 12px; color: #666;"
      ),
      # Show exact matches first
      if (nrow(duplicate_check$exact_matches) > 0) {
        div(
          h5("Identical Records (will be skipped):"),
          renderTable(duplicate_check$exact_matches, striped = TRUE, bordered = TRUE),
          style = "margin-bottom: 20px;"
        )
      },
      # Show differences if any
      if (nrow(duplicate_check$comparison_data) > 0) {
        div(
          h5("Records with Differences:"),
          duplicate_table_ui
        )
      } else {
        div(
          h5("All duplicates are exact matches. No action needed."),
          style = "color: green; font-weight: bold;"
        )
      },
      style = "max-height: 400px; overflow-y: auto;"
    ),
    footer = div(
      style = "display: flex; justify-content: space-between; align-items: center; padding: 15px 20px; background-color: #f8f9fa; border-top: 1px solid #dee2e6;",
      div(
        style = "color: #6c757d; font-size: 14px;",
        if (nrow(duplicate_check$comparison_data) > 0) {
          paste("Please select actions for", nrow(duplicate_check$comparison_data), "conflict(s)")
        } else {
          "All records are identical and will be skipped"
        }
      ),
      div(
        style = "display: flex; gap: 10px;",
        modalButton("Cancel"),
        if (nrow(duplicate_check$comparison_data) > 0) {
          actionButton("process_duplicates", "Process Selected Actions", class = "btn-primary", style = "font-weight: 600;")
        } else NULL
      )
    )
  ))
}

# Function to create duplicate conflicts HTML table for modal display
create_duplicate_conflicts_html_table <- function(import_data) {
  req(import_data$comparison_data)
  
  # Helper function to create comparison cell with highlighting
  create_comparison_cell <- function(import_val, db_val) {
    import_str <- if (is.na(import_val) || import_val == "") "NA" else as.character(import_val)
    db_str <- if (is.na(db_val) || db_val == "") "NA" else as.character(db_val)
    
    if (import_str != db_str) {
      # Values are different - highlight import value in red
      paste0(
        "<span style='color: #dc3545; font-weight: bold; background-color: #f8d7da; padding: 2px 6px; border-radius: 3px;'>", 
        import_str, 
        "</span><br/><span style='color: #6c757d; font-size: 0.9em;'>Current: ", 
        db_str, 
        "</span>"
      )
    } else {
      # Values are same - show normally
      import_str
    }
  }
  
  # Build HTML table
  table_rows <- ""
  
  for (i in seq_len(nrow(import_data$comparison_data))) {
    row_data <- import_data$comparison_data[i, ]
    
    # Create action dropdown
    action_dropdown <- as.character(selectInput(
      paste0("action_", i),
      label = NULL,
      choices = list(
        "Skip (don't import)" = "Skip",
        "Modify ASU ID" = "Modify",
        "Overwrite DB record" = "Overwrite",
        "Keep both records" = "Keep Both"
      ),
      selected = "Skip",
      width = "160px"
    ))
    
    table_rows <- paste0(table_rows, 
      "<tr>",
      "<td style='padding: 8px; border: 1px solid #ddd;'>", row_data$ASU_ID, "</td>",
      "<td style='padding: 8px; border: 1px solid #ddd;'>", 
      create_comparison_cell(row_data$Import_Animal_ID, row_data$DB_Animal_ID), "</td>",
      "<td style='padding: 8px; border: 1px solid #ddd;'>", 
      create_comparison_cell(row_data$Import_Gender, row_data$DB_Gender), "</td>",
      "<td style='padding: 8px; border: 1px solid #ddd;'>", 
      create_comparison_cell(row_data$Import_DoB, row_data$DB_DoB), "</td>",
      "<td style='padding: 8px; border: 1px solid #ddd;'>", 
      create_comparison_cell(row_data$Import_Breeding_Line, row_data$DB_Breeding_Line), "</td>",
      "<td style='padding: 8px; border: 1px solid #ddd;'>", 
      create_comparison_cell(row_data$Import_Genotype, row_data$DB_Genotype), "</td>",
      "<td style='padding: 8px; border: 1px solid #ddd;'>", action_dropdown, "</td>",
      "</tr>"
    )
  }
  
  # Create complete HTML table
  html_table <- HTML(paste0(
    "<table style='width: 100%; border-collapse: collapse; margin-top: 10px;'>",
    "<thead>",
    "<tr style='background-color: #f8f9fa;'>",
    "<th style='padding: 10px; border: 1px solid #ddd; font-weight: bold;'>ASU ID</th>",
    "<th style='padding: 10px; border: 1px solid #ddd; font-weight: bold;'>Animal ID</th>",
    "<th style='padding: 10px; border: 1px solid #ddd; font-weight: bold;'>Gender</th>",
    "<th style='padding: 10px; border: 1px solid #ddd; font-weight: bold;'>Date of Birth</th>",
    "<th style='padding: 10px; border: 1px solid #ddd; font-weight: bold;'>Breeding Line</th>",
    "<th style='padding: 10px; border: 1px solid #ddd; font-weight: bold;'>Genotype</th>",
    "<th style='padding: 10px; border: 1px solid #ddd; font-weight: bold;'>Action</th>",
    "</tr>",
    "</thead>",
    "<tbody>",
    table_rows,
    "</tbody>",
    "</table>"
  ))
  
  return(html_table)
}

# Function to check if an ASU ID already exists
check_asu_id_availability <- function(asu_id, import_data = NULL) {
  # Check database
  con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  existing_count <- DBI::dbGetQuery(con, 
    paste0("SELECT COUNT(*) as count FROM ", TABLE_NAME, " WHERE asu_id = ?"), 
    params = list(asu_id))$count
  DBI::dbDisconnect(con)
  
  # Check current import data if provided
  import_conflict <- FALSE
  if (!is.null(import_data) && !is.null(import_data$parsed_df)) {
    import_conflict <- asu_id %in% import_data$parsed_df$asu_id
  }
  
  return(list(
    available = existing_count == 0 && !import_conflict,
    in_database = existing_count > 0,
    in_import = import_conflict
  ))
}


# Function to show custom ASU ID modal for Modify and Keep Both actions
show_custom_asu_modal <- function(modify_records, keep_both_records) {
  # Create UI elements for each record that needs a custom ASU ID
  custom_asu_inputs <- list()
  
  # Add inputs for Modify records
  if (length(modify_records) > 0) {
    custom_asu_inputs <- append(custom_asu_inputs, list(
      h4("Records to Modify (generate new ASU ID):", style = "color: #ff9800; margin-bottom: 10px;")
    ))
    
    for (record in modify_records) {
      custom_asu_inputs <- append(custom_asu_inputs, list(
        div(
          style = "margin-bottom: 15px; padding: 10px; border: 1px solid #ddd; border-radius: 5px; background-color: #fff3e0;",
          fluidRow(
            column(4, 
              div(style = "font-weight: bold; margin-bottom: 5px;", paste("Original ASU ID:", record$asu_id)),
              div(style = "font-size: 0.9em; color: #666;", paste("Action: Modify"))
            ),
            column(6,
              textInput(
                paste0("custom_asu_modify_", record$index),
                "New ASU ID:",
                value = record$default_new_asu_id,
                placeholder = "Enter new ASU ID"
              )
            ),
            column(2,
              div(style = "margin-top: 25px;",
                actionButton(
                  paste0("check_asu_modify_", record$index),
                  "Check",
                  style = "background-color: #17a2b8; color: white; border: none; width: 100%; font-size: 0.9em;",
                  onclick = paste0("checkAsuAvailability('custom_asu_modify_", record$index, "', 'modify_", record$index, "')")
                ),
                div(
                  id = paste0("asu_status_modify_", record$index),
                  style = "margin-top: 5px; font-size: 0.8em; text-align: center;",
                  ""
                )
              )
            )
          )
        )
      ))
    }
  }
  
  # Add inputs for Keep Both records
  if (length(keep_both_records) > 0) {
    if (length(modify_records) > 0) {
      custom_asu_inputs <- append(custom_asu_inputs, list(hr()))
    }
    
    custom_asu_inputs <- append(custom_asu_inputs, list(
      h4("Records to Keep Both (generate new ASU ID for import):", style = "color: #2196f3; margin-bottom: 10px;")
    ))
    
    for (record in keep_both_records) {
      custom_asu_inputs <- append(custom_asu_inputs, list(
        div(
          style = "margin-bottom: 15px; padding: 10px; border: 1px solid #ddd; border-radius: 5px; background-color: #e3f2fd;",
          fluidRow(
            column(4, 
              div(style = "font-weight: bold; margin-bottom: 5px;", paste("Original ASU ID:", record$asu_id)),
              div(style = "font-size: 0.9em; color: #666;", paste("Action: Keep Both"))
            ),
            column(6,
              textInput(
                paste0("custom_asu_keep_both_", record$index),
                "New ASU ID for import:",
                value = record$default_new_asu_id,
                placeholder = "Enter new ASU ID for imported record"
              )
            ),
            column(2,
              div(style = "margin-top: 25px;",
                actionButton(
                  paste0("check_asu_keep_both_", record$index),
                  "Check",
                  style = "background-color: #17a2b8; color: white; border: none; width: 100%; font-size: 0.9em;",
                  onclick = paste0("checkAsuAvailability('custom_asu_keep_both_", record$index, "', 'keep_both_", record$index, "')")
                ),
                div(
                  id = paste0("asu_status_keep_both_", record$index),
                  style = "margin-top: 5px; font-size: 0.8em; text-align: center;",
                  ""
                )
              )
            )
          )
        )
      ))
    }
  }
  
  showModal(modalDialog(
    title = "Customize ASU IDs",
    size = "l",
    tags$head(tags$script(HTML("
      // Function to check ASU availability via AJAX
      function checkAsuAvailability(inputId, statusId) {
        var asuId = $('#' + inputId).val();
        if (!asuId || asuId.trim() === '') {
          $('#asu_status_' + statusId).html('<span style=\"color: #dc3545;\">❌ Please enter an ASU ID</span>');
          return;
        }
        
        // Show loading state
        $('#asu_status_' + statusId).html('<span style=\"color: #6c757d;\">⏳ Checking...</span>');
        
        // Send request to Shiny server
        Shiny.setInputValue('check_asu_id', {
          asu_id: asuId,
          status_element: 'asu_status_' + statusId,
          timestamp: new Date().getTime()
        });
      }
      
      // Function to check all ASU IDs at once
      function checkAllAsuIds() {
        // Find all ASU ID inputs and their corresponding status elements
        $('input[id^=\"custom_asu_\"]').each(function() {
          var inputId = $(this).attr('id');
          var statusId = inputId.replace('custom_asu_', '');
          checkAsuAvailability(inputId, statusId);
        });
      }
      
      // Custom message handler to update ASU status
      Shiny.addCustomMessageHandler('updateAsuStatus', function(message) {
        $('#' + message.element_id).html(message.html);
      });
      
      // Bind the check all button
      $(document).on('click', '#check_all_asu_ids', function() {
        checkAllAsuIds();
      });
    "))),
    div(
      style = "max-height: 500px; overflow-y: auto;",
      div(
        style = "margin-bottom: 15px; padding: 10px; background-color: #f0f0f0; border-radius: 5px;",
        HTML("<strong>Instructions:</strong><br/>
             • <span style='color: #ff9800;'>Modify</span>: The imported record will get the new ASU ID<br/>
             • <span style='color: #2196f3;'>Keep Both</span>: Both database and imported records will exist, imported record gets the new ASU ID<br/>
             • Click <strong>Check</strong> button to verify if ASU ID is available")
      ),
      custom_asu_inputs
    ),
    footer = tagList(
      div(style = "display: flex; gap: 10px; align-items: center;",
        actionButton("check_all_asu_ids", "Check All", 
                    style = "background-color: #28a745; color: white; border: none; font-size: 0.9em;"),
        span(style = "color: #6c757d; font-size: 0.85em;", "Check all ASU IDs at once")
      ),
      div(style = "margin-left: auto; display: flex; gap: 10px;",
        actionButton("go_back_to_duplicates", "← Back to Duplicates", 
                    style = "background-color: #6c757d; color: white; border: none;"),
        modalButton("Cancel Import"),
        actionButton("confirm_custom_asu", "Confirm and Import", 
                    class = "btn-primary", style = "font-weight: 600;")
      )
    )
  ))
}


# Function to handle processing duplicates
handle_process_duplicates <- function(input, import_data) {
  req(import_data)
  
  # Get user actions from the selectInputs
  user_actions <- list()
  modify_records <- list()
  keep_both_records <- list()
  skip_records <- list()
  overwrite_records <- list()
  
  # Debug: Show all available inputs that match our pattern
  input_names <- names(reactiveValuesToList(input))
  action_inputs <- input_names[grepl("^action_", input_names)]
  cat("DEBUG: Available action inputs:", paste(action_inputs, collapse = ", "), "\n")
  
  for (i in seq_len(nrow(import_data$comparison_data))) {
    action_input_id <- paste0("action_", i)
    action_input <- input[[action_input_id]]
    action <- if (is.null(action_input)) "Skip" else action_input
    user_actions[[action_input_id]] <- action

    # Debug: log what actions we're getting
    cat("DEBUG: Row", i, "Input ID:", action_input_id, "Value:", 
        if (is.null(action_input)) "NULL" else action_input, "Final action:", action, "\n")
    
    row_data <- import_data$comparison_data[i, ]
    record_info <- list(
      index = i,
      asu_id = row_data$ASU_ID,
      action = action,
      import_data = row_data,
      default_new_asu_id = paste0(row_data$ASU_ID, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    )
    
    # Collect records based on action type
    if (action == "Modify") {
      modify_records[[length(modify_records) + 1]] <- record_info
    } else if (action == "Keep Both") {
      keep_both_records[[length(keep_both_records) + 1]] <- record_info
    } else if (action == "Skip") {
      skip_records[[length(skip_records) + 1]] <- record_info
    } else if (action == "Overwrite") {
      overwrite_records[[length(overwrite_records) + 1]] <- record_info
    }
  }
  
  # Check if we need to show custom ASU ID modal
  needs_custom_asu <- length(modify_records) > 0 || length(keep_both_records) > 0
  
  return(list(
    user_actions = user_actions,
    modify_records = modify_records,
    keep_both_records = keep_both_records,
    skip_records = skip_records,
    overwrite_records = overwrite_records,
    needs_custom_asu = needs_custom_asu
  ))
}


# Function to collect custom ASU IDs from the modal
collect_custom_asu_ids <- function(input, modify_records, keep_both_records) {
  custom_asu_map <- list()
  
  # Collect custom ASU IDs for Modify records
  for (record in modify_records) {
    input_id <- paste0("custom_asu_modify_", record$index)
    custom_asu <- input[[input_id]]
    if (!is.null(custom_asu) && custom_asu != "") {
      custom_asu_map[[record$asu_id]] <- custom_asu
    }
  }
  
  # Collect custom ASU IDs for Keep Both records
  for (record in keep_both_records) {
    input_id <- paste0("custom_asu_keep_both_", record$index)
    custom_asu <- input[[input_id]]
    if (!is.null(custom_asu) && custom_asu != "") {
      custom_asu_map[[record$asu_id]] <- custom_asu
    }
  }
  
  return(custom_asu_map)
}


# Function to handle skip duplicates import
handle_skip_duplicates_import <- function(import_data, DB_PATH) {
  req(import_data)
  removeModal()
  
  # Remove duplicates from parsed_df
  clean_df <- import_data$parsed_df
  if (length(import_data$import_duplicates) > 0) {
    clean_df <- clean_df[!duplicated(clean_df$asu_id), ]
  }
  if (length(import_data$db_conflicts) > 0) {
    clean_df <- clean_df[!clean_df$asu_id %in% import_data$db_conflicts, ]
  }
  
  # Insert into DB using function from db_check.R
  result <- import_data_to_db(clean_df)
  
  if (result) {
    showModal(modalDialog(title = "Import Success", "Animals imported successfully (duplicates skipped)!", easyClose = TRUE))
    return(TRUE)
  } else {
    showModal(modalDialog(title = "Import Error", "Failed to import animals.", easyClose = TRUE))
    return(FALSE)
  }
}


# Main server module for animal modal functionality
modal_add_animal_server <- function(input, output, session, import_data, all_mice_table, global_refresh_trigger, db_path, table_name) {
  
  # Helper function to refresh all_mice_table
  refresh_all_mice_table <- function() {
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    all_data <- DBI::dbGetQuery(con, paste0("SELECT * FROM ", table_name, " ORDER BY asu_id"))
    DBI::dbDisconnect(con)
    all_mice_table(all_data)
    global_refresh_trigger(Sys.time())
  }
  
  # Handle mapping confirmation
  observeEvent(input$confirm_mappings_btn, {
    # Process mapping confirmation using the module function
    parsed_df <- handle_mapping_confirmation(input, import_data)
    
    # If successful, parsed_df will be returned, otherwise NULL
    if (!is.null(parsed_df)) {
      # Check for duplicates and handle import
      duplicate_check <- check_duplicates_and_conflicts(parsed_df)
      
      if (duplicate_check$has_duplicates) {
        # Store data for duplicate processing
        import_data$parsed_df <- parsed_df
        import_data$comparison_data <- duplicate_check$comparison_data
        import_data$exact_matches <- duplicate_check$exact_matches
        import_data$import_duplicates <- duplicate_check$import_duplicates
        import_data$db_conflicts <- duplicate_check$db_conflicts
        
        # Show duplicates modal using module function
        show_duplicates_modal(duplicate_check, import_data)
      } else {
        # No duplicates, import directly
        result <- import_data_to_db(parsed_df)
        if (result) {
          showModal(modalDialog(title = "Import Success", "Animals imported successfully!", easyClose = TRUE))
          refresh_all_mice_table()
        } else {
          showModal(modalDialog(title = "Import Error", "Failed to import animals.", easyClose = TRUE))
        }
      }
    }
  })

  # Handle Process Duplicates
  observeEvent(input$process_duplicates, {
    # Use the function from modal_add_animal.R to handle duplicate processing
    duplicate_actions <- handle_process_duplicates(input, import_data)
    
    # Check if we need to show custom ASU ID modal
    if (duplicate_actions$needs_custom_asu) {
      # Store the duplicate actions for later use
      import_data$duplicate_actions <- duplicate_actions
      
      # Show custom ASU ID modal
      show_custom_asu_modal(duplicate_actions$modify_records, duplicate_actions$keep_both_records)
    } else {
      # No custom ASU IDs needed, process directly
      result <- process_duplicates(
        import_data$parsed_df, 
        import_data$comparison_data, 
        import_data$import_duplicates, 
        import_data$db_conflicts, 
        duplicate_actions$user_actions
      )
      
      # Insert into DB
      if (nrow(result$final_df) > 0) {
        db_result <- import_data_to_db(result$final_df)
        
        if (db_result) {
          msg <- paste0("Import completed! ", nrow(result$final_df), " records imported. ")
          if (result$skipped_count > 0) msg <- paste0(msg, result$skipped_count, " skipped. ")
          if (result$modified_count > 0) msg <- paste0(msg, result$modified_count, " modified. ")
          if (result$overwritten_count > 0) msg <- paste0(msg, result$overwritten_count, " overwritten.")
          
          showModal(modalDialog(title = "Import Success", msg, easyClose = TRUE))
          removeModal()
          refresh_all_mice_table()
        } else {
          showModal(modalDialog(title = "Import Error", "Failed to import animals.", easyClose = TRUE))
        }
      } else {
        showModal(modalDialog(title = "Import Cancelled", "No records to import after processing duplicates.", easyClose = TRUE))
      }
    }
  })
  
  # Handle ASU ID availability checking
  observeEvent(input$check_asu_id, {
    req(input$check_asu_id)
    
    asu_id <- input$check_asu_id$asu_id
    status_element <- input$check_asu_id$status_element
    
    # Check availability using the function from modal_add_animal.R
    availability <- check_asu_id_availability(asu_id, import_data)
    
    # Create status message
    if (availability$available) {
      status_html <- '<span style="color: #28a745;">✅ Available</span>'
    } else {
      reasons <- c()
      if (availability$in_database) {
        reasons <- c(reasons, "exists in database")
      }
      if (availability$in_import) {
        reasons <- c(reasons, "exists in current import")
      }
      status_html <- paste0('<span style="color: #dc3545;">❌ ', paste(reasons, collapse = ", "), '</span>')
    }
    
    # Send message to client to update the status
    session$sendCustomMessage("updateAsuStatus", list(
      element_id = status_element,
      html = status_html
    ))
  })
  
  # Handle Custom ASU ID Confirmation
  observeEvent(input$confirm_custom_asu, {
    req(import_data$duplicate_actions)
    
    # Collect custom ASU IDs from the modal
    custom_asu_map <- collect_custom_asu_ids(input, 
                                           import_data$duplicate_actions$modify_records, 
                                           import_data$duplicate_actions$keep_both_records)
    
    # Validate custom ASU IDs (check for duplicates with existing database and import)
    validation_errors <- c()
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    
    # Check each custom ASU ID
    for (asu_id in names(custom_asu_map)) {
      new_asu_id <- custom_asu_map[[asu_id]]
      
      # Skip empty or null values
      if (is.null(new_asu_id) || new_asu_id == "") {
        validation_errors <- c(validation_errors, 
                              paste0("ASU ID for '", asu_id, "' cannot be empty"))
        next
      }
      
      # Check if new ASU ID already exists in database
      existing_count <- DBI::dbGetQuery(con, 
        paste0("SELECT COUNT(*) as count FROM ", table_name, " WHERE asu_id = ?"), 
        params = list(new_asu_id))$count
      
      if (existing_count > 0) {
        validation_errors <- c(validation_errors, 
                              paste0("ASU ID '", new_asu_id, "' already exists in database"))
      }
      
      # Check if new ASU ID conflicts with current import data
      if (!is.null(import_data$parsed_df) && new_asu_id %in% import_data$parsed_df$asu_id) {
        validation_errors <- c(validation_errors, 
                              paste0("ASU ID '", new_asu_id, "' conflicts with current import data"))
      }
      
      # Check for duplicates within the custom ASU IDs
      if (sum(unlist(custom_asu_map) == new_asu_id) > 1) {
        validation_errors <- c(validation_errors, 
                              paste0("Duplicate custom ASU ID '", new_asu_id, "' found"))
      }
      
      # Basic format validation (optional - only if you have specific format requirements)
      if (!grepl("^[A-Za-z0-9_-]+$", new_asu_id)) {
        validation_errors <- c(validation_errors, 
                              paste0("ASU ID '", new_asu_id, "' contains invalid characters (only letters, numbers, underscore, and hyphen allowed)"))
      }
    }
    
    DBI::dbDisconnect(con)
    
    # If validation errors, show them with more detailed information
    if (length(validation_errors) > 0) {
      showModal(modalDialog(
        title = "ASU ID Validation Errors",
        div(
          style = "margin-bottom: 15px;",
          "The following errors were found with your custom ASU IDs:"
        ),
        div(
          style = "background-color: #f8d7da; border: 1px solid #f5c6cb; padding: 15px; border-radius: 5px; max-height: 300px; overflow-y: auto;",
          HTML(paste("• ", validation_errors, collapse = "<br/>"))
        ),
        div(
          style = "margin-top: 15px; font-size: 0.9em; color: #6c757d;",
          "💡 Tip: Use the 'Check' buttons next to each ASU ID field to verify availability before confirming."
        ),
        footer = tagList(
          actionButton("fix_custom_asu", "← Fix ASU IDs", 
                      style = "background-color: #6c757d; color: white; border: none;"),
          modalButton("Cancel Import")
        )
      ))
      return()
    }
    
    # All validations passed, process duplicates with custom ASU IDs
    result <- process_duplicates_with_custom(
      import_data$parsed_df, 
      import_data$comparison_data, 
      import_data$import_duplicates, 
      import_data$db_conflicts, 
      import_data$duplicate_actions$user_actions,
      custom_asu_map
    )
    
    # Insert into DB
    if (nrow(result$final_df) > 0) {
      db_result <- import_data_to_db(result$final_df)
      
      if (db_result) {
        msg <- paste0("Import completed! ", nrow(result$final_df), " records imported. ")
        if (result$skipped_count > 0) msg <- paste0(msg, result$skipped_count, " skipped. ")
        if (result$modified_count > 0) msg <- paste0(msg, result$modified_count, " modified. ")
        if (result$overwritten_count > 0) msg <- paste0(msg, result$overwritten_count, " overwritten.")
        
        showModal(modalDialog(title = "Import Success", msg, easyClose = TRUE))
        removeModal()
        refresh_all_mice_table()
      } else {
        showModal(modalDialog(title = "Import Error", "Failed to import animals.", easyClose = TRUE))
      }
    } else {
      showModal(modalDialog(title = "Import Cancelled", "No records to import after processing duplicates.", easyClose = TRUE))
    }
  })
  
  # Handle going back to fix custom ASU IDs
  observeEvent(input$fix_custom_asu, {
    req(import_data$duplicate_actions)
    removeModal()
    
    # Show the custom ASU modal again
    show_custom_asu_modal(import_data$duplicate_actions$modify_records, 
                         import_data$duplicate_actions$keep_both_records)
  })
  
  # Handle going back to duplicates modal from custom ASU modal
  observeEvent(input$go_back_to_duplicates, {
    req(import_data$comparison_data, import_data$exact_matches)
    removeModal()
    
    # Recreate the duplicate check structure
    duplicate_check <- list(
      comparison_data = import_data$comparison_data,
      exact_matches = import_data$exact_matches,
      has_duplicates = TRUE
    )
    
    # Show the duplicates modal again
    show_duplicates_modal(duplicate_check, import_data)
  })

  # Handle Single Entry Submission
  observeEvent(input$submit_single_entry_btn, {
    # Use the module function to handle submission
    result <- handle_single_entry_submission(input, db_path, table_name)
    
    # If successful, refresh the all_mice_table
    if (result) {
      refresh_all_mice_table()
    }
  })
  
  # Handle proceeding with warnings
  observeEvent(input$proceed_with_warnings, {
    removeModal()
    # Trigger the submission again
    input$submit_single_entry_btn
  })
  
  # Handle Excel Import
  observeEvent(input$submit_import_excel_btn, {
    import_data <- handle_excel_import(input, import_data)
  })
}