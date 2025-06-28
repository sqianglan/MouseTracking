all_mice_tab_ui <- function() {
  fluidPage(
    fluidRow(
      column(12, 
        div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
          h3("All Mice", style = "margin: 0;"),
          div(
            actionButton("clear_search_btn", "Clear Search", 
                        style = "background-color: #f44336; color: white; border: none; margin-right: 10px;"),
            actionButton("bulk_edit_btn", "Edit Selected", 
                        style = "background-color: #ff9800; color: white; border: none; margin-right: 10px;"),
            add_plugging_modal_ui("add_plugging_modal_all_mice"),
            uiOutput("bulk_delete_btn_ui")
          )
        )
      )
    ),
    fluidRow(
      column(3,
        wellPanel(
          h4("Search Animals", style = "margin-top: 0;"),
          textInput("all_mice_search_asu_id", "ASU ID", placeholder = "Enter ASU ID (supports * and ? wildcards)"),
          textInput("all_mice_search_animal_id", "Animal ID", placeholder = "Enter Animal ID (supports * and ? wildcards)"),
          selectInput("all_mice_search_gender", "Gender", choices = c("", "Male", "Female"), selected = ""),
          textInput("all_mice_search_breeding_line", "Breeding Line", placeholder = "Enter Breeding Line (supports * and ? wildcards)"),
          selectizeInput("all_mice_search_responsible_person", "Responsible Person", 
                        choices = c(""), 
                        options = list(placeholder = "Select responsible person")),
          selectInput("all_mice_search_stock_category", "Stock Category", choices = c("", "Experiment", "Breeding", "Charles River"), selected = ""),
          selectInput("all_mice_search_status", "Status", choices = c("Both", "Live", "Deceased"), selected = "Live"),
          div(
            style = "margin-top: 15px; font-size: 12px; color: #666;",
            "Use * for multiple characters and ? for single character wildcards"
          ),
          actionButton("all_mice_execute_search_btn", "Search", 
                      style = "background-color: #1976d2; color: white; border: none; width: 100%; margin-top: 10px;")
        )
      ),
      column(9,
        div(
          style = "margin-bottom: 10px; font-size: 12px; color: #666;",
          "💡 Double-click on any row to view Mouse History Tracing"
        ),
        DT::dataTableOutput("all_mice_table")
      )
    )
  )
}

all_mice_tab_server <- function(input, output, session, all_mice_table, is_system_locked = NULL, global_refresh_trigger = NULL) {
  # Source the modal module
  source("Modules/modal_add_plugging_event.R", local = TRUE)
  source("Modules/validation.R", local = TRUE)
  
  # Default lock function if not provided
  if (is.null(is_system_locked)) {
    is_system_locked <- function() FALSE
  }
  
  # Default refresh trigger if not provided
  if (is.null(global_refresh_trigger)) {
    global_refresh_trigger <- reactiveVal(Sys.time())
  }

  # Create reactive value to store filtered data
  filtered_data <- reactiveVal(NULL)

  # On app start, load filtered data with default status of Live  
  observe({
    # Build SQL query for default filter (status = 'Alive')
    query <- paste0("SELECT * FROM ", TABLE_NAME, " WHERE status = 'Alive' ORDER BY asu_id")
    con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
    filtered <- DBI::dbGetQuery(con, query)
    DBI::dbDisconnect(con)
    filtered_data(filtered)
    all_mice_table(filtered)
  })
  
  # React to global refresh trigger for automatic updates
  observe({
    global_refresh_trigger()
    
    # Get current search status to maintain filter
    current_status_filter <- input$all_mice_search_status
    if (is.null(current_status_filter)) current_status_filter <- "Live"
    
    # Rebuild query based on current filters
    where_conditions <- c()
    
    # Add status filter
    if (current_status_filter != "Both") {
      if (current_status_filter == "Live") {
        where_conditions <- c(where_conditions, "status == 'Alive'")
      } else if (current_status_filter == "Deceased") {
        where_conditions <- c(where_conditions, "status == 'Deceased'")
      }
    }
    
    # Add other current filters if they exist
    if (!is.null(input$all_mice_search_asu_id) && input$all_mice_search_asu_id != "") {
      asu_pattern <- gsub("\\*", "%", gsub("\\?", "_", input$all_mice_search_asu_id))
      where_conditions <- c(where_conditions, paste0("asu_id LIKE '", asu_pattern, "'"))
    }
    if (!is.null(input$all_mice_search_animal_id) && input$all_mice_search_animal_id != "") {
      animal_pattern <- gsub("\\*", "%", gsub("\\?", "_", input$all_mice_search_animal_id))
      where_conditions <- c(where_conditions, paste0("animal_id LIKE '", animal_pattern, "'"))
    }
    if (!is.null(input$all_mice_search_gender) && input$all_mice_search_gender != "") {
      where_conditions <- c(where_conditions, paste0("gender = '", input$all_mice_search_gender, "'"))
    }
    if (!is.null(input$all_mice_search_breeding_line) && input$all_mice_search_breeding_line != "") {
      breeding_pattern <- gsub("\\*", "%", gsub("\\?", "_", input$all_mice_search_breeding_line))
      where_conditions <- c(where_conditions, paste0("breeding_line LIKE '", breeding_pattern, "'"))
    }
    if (!is.null(input$all_mice_search_responsible_person) && input$all_mice_search_responsible_person != "") {
      where_conditions <- c(where_conditions, paste0("responsible_person = '", input$all_mice_search_responsible_person, "'"))
    }
    if (!is.null(input$all_mice_search_stock_category) && input$all_mice_search_stock_category != "") {
      where_conditions <- c(where_conditions, paste0("stock_category = '", input$all_mice_search_stock_category, "'"))
    }
    
    # Build and execute query
    if (length(where_conditions) == 0) {
      query <- paste0("SELECT * FROM ", TABLE_NAME, " ORDER BY asu_id")
    } else {
      query <- paste0("SELECT * FROM ", TABLE_NAME, " WHERE ", paste(where_conditions, collapse = " AND "), " ORDER BY asu_id")
    }
    
    con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
    tryCatch({
      refreshed_data <- DBI::dbGetQuery(con, query)
      filtered_data(refreshed_data)
      all_mice_table(refreshed_data)
    }, finally = {
      DBI::dbDisconnect(con)
    })
  })

  # Render all mice table
  output$all_mice_table <- DT::renderDataTable({
    req(filtered_data())
    data <- filtered_data()
    
    # Function to format timestamps
    format_timestamp <- function(timestamp) {
      if (is.null(timestamp) || is.na(timestamp) || timestamp == "") {
        return("")
      }
      # Handle Unix timestamp (seconds since epoch)
      if (is.numeric(timestamp) && timestamp > 1000000000) {
        return(format(as.POSIXct(timestamp, origin = "1970-01-01"), "%d-%b-%Y %H:%M"))
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
          return(format(parsed, "%d-%b-%Y %H:%M"))
        }
      }
      return(as.character(timestamp))
    }
    
    # Select columns including timestamps
    display_data <- data[, c("asu_id", "animal_id", "gender", "dob", "breeding_line", "genotype", "responsible_person", "stock_category", "status", "last_updated")]
    
    # Calculate age in weeks
    display_data$age_weeks <- round(as.numeric(Sys.Date() - as.Date(display_data$dob)) / 7, 1)
    
    # Reorder columns for display
    col_order <- c("asu_id", "animal_id", "gender", "dob", "age_weeks", "breeding_line", "genotype", "responsible_person", "stock_category", "status", "last_updated")
    display_data <- display_data[, col_order]
    
    # Format dates
    if ("dob" %in% colnames(display_data)) {
      display_data$dob <- format(as.Date(display_data$dob), "%d-%b-%Y")
    }
    
    # Format timestamps
    if ("last_updated" %in% colnames(display_data)) {
      display_data$last_updated <- sapply(display_data$last_updated, format_timestamp)
    }
    
    colnames(display_data) <- c(
      "ASU ID",
      "Animal ID", 
      "Gender",
      "Date of Birth",
      "Age (weeks)",
      "Breeding Line",
      "Genotype",
      "Responsible Person",
      "Stock Category",
      "Status",
      "Last Updated"
    )
    DT::datatable(
      display_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 'tip'
      ),
      filter = 'none',
      selection = 'multiple',
      callback = JS("
        table.on('dblclick', 'tr', function() {
          var rowIndex = table.row(this).index();
          var data = table.row(this).data();
          console.log('Row index:', rowIndex);
          console.log('Double-click data:', data);
          if (data && data.length > 0) {
            var asuId = data[0];
            console.log('ASU ID captured:', asuId);
            Shiny.setInputValue('mouse_double_click', asuId, {priority: 'event'});
            Shiny.setInputValue('mouse_double_click_row', rowIndex, {priority: 'event'});
          } else {
            console.log('No data found in row');
          }
        });
      "),
      rownames = FALSE
    )
  })
  
  # Handle Clear Search button
  observeEvent(input$clear_search_btn, {
    con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
    all_data <- DBI::dbGetQuery(con, paste0("SELECT * FROM ", TABLE_NAME, " ORDER BY asu_id"))
    DBI::dbDisconnect(con)
    filtered_data(all_data)
    all_mice_table(all_data)
    showNotification("Search cleared. Showing all animals.", type = "message", duration = 3)
  })
  
  # Populate responsible person dropdown for All Mice tab search
  observe({
    con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
    responsible_persons <- unique(DBI::dbGetQuery(con, paste0("SELECT DISTINCT responsible_person FROM ", TABLE_NAME, " WHERE responsible_person IS NOT NULL"))$responsible_person)
    DBI::dbDisconnect(con)
    
    updateSelectizeInput(session, "all_mice_search_responsible_person", 
                        choices = c("", responsible_persons))
  })
  
  # Handle All Mice tab search execution
  observeEvent(input$all_mice_execute_search_btn, {
    where_conditions <- c()
    if (!is.null(input$all_mice_search_asu_id) && input$all_mice_search_asu_id != "") {
      asu_pattern <- gsub("\\*", "%", gsub("\\?", "_", input$all_mice_search_asu_id))
      where_conditions <- c(where_conditions, paste0("asu_id LIKE '", asu_pattern, "'"))
    }
    if (!is.null(input$all_mice_search_animal_id) && input$all_mice_search_animal_id != "") {
      animal_pattern <- gsub("\\*", "%", gsub("\\?", "_", input$all_mice_search_animal_id))
      where_conditions <- c(where_conditions, paste0("animal_id LIKE '", animal_pattern, "'"))
    }
    if (!is.null(input$all_mice_search_gender) && input$all_mice_search_gender != "") {
      where_conditions <- c(where_conditions, paste0("gender = '", input$all_mice_search_gender, "'"))
    }
    if (!is.null(input$all_mice_search_breeding_line) && input$all_mice_search_breeding_line != "") {
      breeding_pattern <- gsub("\\*", "%", gsub("\\?", "_", input$all_mice_search_breeding_line))
      where_conditions <- c(where_conditions, paste0("breeding_line LIKE '", breeding_pattern, "'"))
    }
    if (!is.null(input$all_mice_search_responsible_person) && input$all_mice_search_responsible_person != "") {
      where_conditions <- c(where_conditions, paste0("responsible_person = '", input$all_mice_search_responsible_person, "'"))
    }
    if (!is.null(input$all_mice_search_stock_category) && input$all_mice_search_stock_category != "") {
      where_conditions <- c(where_conditions, paste0("stock_category = '", input$all_mice_search_stock_category, "'"))
    }
    if (!is.null(input$all_mice_search_status) && input$all_mice_search_status != "Both") {
      if (input$all_mice_search_status == "Live") {
        where_conditions <- c(where_conditions, "status == 'Alive'")
      } else if (input$all_mice_search_status == "Deceased") {
        where_conditions <- c(where_conditions, "status == 'Deceased'")
      }
    }
    if (length(where_conditions) == 0) {
      query <- paste0("SELECT * FROM ", TABLE_NAME, " ORDER BY asu_id")
    } else {
      query <- paste0("SELECT * FROM ", TABLE_NAME, " WHERE ", paste(where_conditions, collapse = " AND "), " ORDER BY asu_id")
    }
    con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
    search_results <- tryCatch({
      DBI::dbGetQuery(con, query)
    }, error = function(e) {
      data.frame()
    })
    DBI::dbDisconnect(con)
    filtered_data(search_results)
    all_mice_table(search_results)
    if (nrow(search_results) == 0) {
      showNotification("No animals found matching your search criteria.", type = "warning", duration = 3)
    } else {
      showNotification(paste("Found", nrow(search_results), "animals matching your search criteria."), type = "message", duration = 3)
    }
  })
  
  # Handle Bulk Edit button
  observeEvent(input$bulk_edit_btn, {
    # Use filtered_data for both display and selection mapping
    data <- filtered_data()
    selected_visible_rows <- input$all_mice_table_rows_selected
    if (is.null(selected_visible_rows) || length(selected_visible_rows) == 0) {
      showNotification("Please select at least one animal to edit.", type = "warning", duration = 3)
      return()
    }
    # Get the displayed table as shown to the user (with only columns that exist in data)
    display_data <- data[, c("asu_id", "animal_id", "gender", "dob", "breeding_line", "genotype", "responsible_person", "stock_category", "status")]
    selected_asu_ids <- display_data[selected_visible_rows, "asu_id"]
    selected_data <- data[data$asu_id %in% selected_asu_ids, , drop = FALSE]
    # Get existing values from database for dropdowns (keep connection open)
    con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
    breeding_lines <- unique(DBI::dbGetQuery(con, paste0("SELECT DISTINCT breeding_line FROM ", TABLE_NAME, " WHERE breeding_line IS NOT NULL"))$breeding_line)
    genotypes <- unique(DBI::dbGetQuery(con, paste0("SELECT DISTINCT genotype FROM ", TABLE_NAME, " WHERE genotype IS NOT NULL"))$genotype)
    responsible_persons <- unique(DBI::dbGetQuery(con, paste0("SELECT DISTINCT responsible_person FROM ", TABLE_NAME, " WHERE responsible_person IS NOT NULL"))$responsible_person)
    project_codes <- unique(DBI::dbGetQuery(con, paste0("SELECT DISTINCT project_code FROM ", TABLE_NAME, " WHERE project_code IS NOT NULL"))$project_code)
    DBI::dbDisconnect(con)
    
    # Helper to get common value, treating NA as "", trimming whitespace, and coercing to character
    get_common_value <- function(x) {
      x <- as.character(x)
      x[is.na(x)] <- ""
      x <- trimws(x)
      ux <- unique(x)
      ux <- ux[ux != "NA"] # Remove literal "NA" string if present
      if (length(ux) == 1) ux else ""
    }
    common_gender <- get_common_value(selected_data$gender)
    common_status <- get_common_value(selected_data$status)
    common_breeding_line <- get_common_value(selected_data$breeding_line)
    common_genotype <- get_common_value(selected_data$genotype)
    common_responsible_person <- get_common_value(selected_data$responsible_person)
    common_protocol <- get_common_value(selected_data$protocol)
    common_study_plan <- get_common_value(selected_data$study_plan)
    common_project_code <- get_common_value(selected_data$project_code)
    common_stock_category <- get_common_value(selected_data$stock_category)

    # Ensure common values are included in choices for each field, and remove NA
    breeding_line_choices <- unique(c("", na.omit(as.character(breeding_lines)), common_breeding_line))
    genotype_choices <- unique(c("", na.omit(as.character(genotypes)), common_genotype))
    responsible_person_choices <- unique(c("", na.omit(as.character(responsible_persons)), common_responsible_person))
    protocol_choices <- unique(c("", 
      "1 (Breeding and maintenance of genetically altered animals)",
      "2 (Epithelial stem cell fate and dynamics during tissue development and regeneration)",
      "3 (Mouse tumor model)",
      common_protocol))
    study_plan_choices <- c("", "SP2500090", "SP2500083", "SP2500082", "SP2500081")
    project_code_choices <- unique(c("", na.omit(as.character(project_codes)), common_project_code))
    stock_category_choices <- unique(c("", "Experiment", "Breeding", "Charles River", common_stock_category))

    # Debug: Print selected mouse asu_id(s)
    cat('DEBUG: selected asu_id(s):', paste(selected_data$asu_id, collapse=','), '\n')
    # Debug: Print selected value and choices for each field
    cat('DEBUG: breeding_line - selected:', common_breeding_line, 'choices:', paste(breeding_line_choices, collapse=','), '\n')
    cat('DEBUG: genotype - selected:', common_genotype, 'choices:', paste(genotype_choices, collapse=','), '\n')
    cat('DEBUG: responsible_person - selected:', common_responsible_person, 'choices:', paste(responsible_person_choices, collapse=','), '\n')
    cat('DEBUG: protocol - selected:', common_protocol, 'choices:', paste(protocol_choices, collapse=','), '\n')
    cat('DEBUG: project_code - selected:', common_project_code, 'choices:', paste(project_code_choices, collapse=','), '\n')
    cat('DEBUG: stock_category - selected:', common_stock_category, 'choices:', paste(stock_category_choices, collapse=','), '\n')

    showModal(modalDialog(
      title = paste("Bulk Edit", length(selected_asu_ids), "Selected Animals"),
      size = "l",
      div(
        style = "margin-bottom: 15px;",
        paste("You are editing", length(selected_asu_ids), "animals. Leave fields empty to keep current values."),
        br(),
        tags$div(
          style = "font-size: 16px; color: #333; margin-top: 8px; font-weight: bold;",
          HTML(paste0("ASU IDs: <span style='font-weight:bold;'>", paste(selected_asu_ids, collapse = ", "), "</span>"))
        )
      ),
      fluidRow(
        column(6, selectInput("bulk_edit_gender", "Gender", choices = c("", "Male", "Female"), selected = common_gender)),
        column(6, selectInput("bulk_edit_status", "Status", choices = c("", "Alive", "Deceased"), selected = common_status))
      ),
      fluidRow(
        column(6, selectizeInput("bulk_edit_breeding_line", "Breeding Line", 
                                choices = breeding_line_choices, 
                                selected = common_breeding_line,
                                options = list(create = TRUE, placeholder = "Select or type new"))),
        column(6, selectizeInput("bulk_edit_genotype", "Genotype", 
                                choices = genotype_choices, 
                                selected = common_genotype,
                                options = list(create = TRUE, placeholder = "Select or type new")))
      ),
      fluidRow(
        column(6, selectizeInput("bulk_edit_responsible_person", "Responsible Person", 
                                choices = responsible_person_choices, 
                                selected = common_responsible_person,
                                options = list(create = TRUE, placeholder = "Select or type new")))
      ),
      fluidRow(
        column(6, selectInput("bulk_edit_protocol", "Protocol", 
                              choices = protocol_choices, 
                              selected = common_protocol)),
        column(6, selectizeInput("bulk_edit_project_code", "Project Code", 
                                choices = project_code_choices, 
                                selected = common_project_code,
                                options = list(create = TRUE, placeholder = "Select or type new")))
      ),
      fluidRow(
        column(6, selectInput("bulk_edit_study_plan", "Study Plan", 
                              choices = study_plan_choices, 
                              selected = common_study_plan)),
        column(6, selectInput("bulk_edit_stock_category", "Stock Category", 
                              choices = stock_category_choices, 
                              selected = common_stock_category))
      ),
      div(
        style = "margin-top: 15px; font-size: 12px; color: #666;",
        "Empty fields will not change the current values"
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("submit_bulk_edit_btn", "Update Animals", style = "background-color: #1976d2; color: white; border: none;")
      )
    ))
  })
  
  # Handle Bulk Edit submission
  observeEvent(input$submit_bulk_edit_btn, {
    # Use filtered_data for both display and selection mapping
    data <- filtered_data()
    selected_visible_rows <- input$all_mice_table_rows_selected
    if (is.null(selected_visible_rows) || length(selected_visible_rows) == 0) {
      showNotification("Please select at least one animal to edit.", type = "warning", duration = 3)
      return()
    }
    # Get the displayed table as shown to the user (with only columns that exist in data)
    display_data <- data[, c("asu_id", "animal_id", "gender", "dob", "breeding_line", "genotype", "responsible_person", "stock_category", "status")]
    selected_asu_ids <- display_data[selected_visible_rows, "asu_id"]
    selected_data <- data[data$asu_id %in% selected_asu_ids, , drop = FALSE]
    # Get existing values from database for dropdowns (keep connection open)
    con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
    breeding_lines <- unique(DBI::dbGetQuery(con, paste0("SELECT DISTINCT breeding_line FROM ", TABLE_NAME, " WHERE breeding_line IS NOT NULL"))$breeding_line)
    genotypes <- unique(DBI::dbGetQuery(con, paste0("SELECT DISTINCT genotype FROM ", TABLE_NAME, " WHERE genotype IS NOT NULL"))$genotype)
    responsible_persons <- unique(DBI::dbGetQuery(con, paste0("SELECT DISTINCT responsible_person FROM ", TABLE_NAME, " WHERE responsible_person IS NOT NULL"))$responsible_person)
    project_codes <- unique(DBI::dbGetQuery(con, paste0("SELECT DISTINCT project_code FROM ", TABLE_NAME, " WHERE project_code IS NOT NULL"))$project_code)
    DBI::dbDisconnect(con)
    
    # Collect bulk edit data
    bulk_edit_data <- list()
    if (!is.null(input$bulk_edit_gender) && input$bulk_edit_gender != "") {
      bulk_edit_data$gender <- input$bulk_edit_gender
    }
    if (!is.null(input$bulk_edit_status) && input$bulk_edit_status != "") {
      bulk_edit_data$status <- input$bulk_edit_status
    }
    if (!is.null(input$bulk_edit_breeding_line) && input$bulk_edit_breeding_line != "") {
      bulk_edit_data$breeding_line <- input$bulk_edit_breeding_line
    }
    if (!is.null(input$bulk_edit_genotype) && input$bulk_edit_genotype != "") {
      bulk_edit_data$genotype <- input$bulk_edit_genotype
    }
    if (!is.null(input$bulk_edit_responsible_person) && input$bulk_edit_responsible_person != "") {
      bulk_edit_data$responsible_person <- input$bulk_edit_responsible_person
    }
    if (!is.null(input$bulk_edit_protocol) && input$bulk_edit_protocol != "") {
      bulk_edit_data$protocol <- input$bulk_edit_protocol
    }
    if (!is.null(input$bulk_edit_study_plan) && input$bulk_edit_study_plan != "") {
      bulk_edit_data$study_plan <- input$bulk_edit_study_plan
    }
    if (!is.null(input$bulk_edit_project_code) && input$bulk_edit_project_code != "") {
      bulk_edit_data$project_code <- input$bulk_edit_project_code
    }
    if (!is.null(input$bulk_edit_stock_category) && input$bulk_edit_stock_category != "") {
      bulk_edit_data$stock_category <- input$bulk_edit_stock_category
    }
    
    # Validate bulk edit data
    if (length(bulk_edit_data) > 0) {
      validation_result <- validate_mouse_data(bulk_edit_data)
      
      if (!validation_result$valid) {
        error_html <- display_validation_errors(validation_result)
        showModal(modalDialog(
          title = "Validation Errors",
          error_html,
          easyClose = TRUE
        ))
        return()
      }
    }
    
    # Build update queries for each selected animal
    update_count <- 0
    record_ids <- c()
    
    for (i in 1:nrow(selected_data)) {
      asu_id <- selected_data$asu_id[i]
      update_fields <- c()
      old_values <- list()
      new_values <- list()
      
      # Check each field and add to update if not empty
      if (!is.null(input$bulk_edit_gender) && input$bulk_edit_gender != "") {
        update_fields <- c(update_fields, paste0("gender = '", input$bulk_edit_gender, "'"))
        old_values$gender <- selected_data$gender[i]
        new_values$gender <- input$bulk_edit_gender
      }
      if (!is.null(input$bulk_edit_status) && input$bulk_edit_status != "") {
        update_fields <- c(update_fields, paste0("status = '", input$bulk_edit_status, "'"))
        old_values$status <- selected_data$status[i]
        new_values$status <- input$bulk_edit_status
      }
      if (!is.null(input$bulk_edit_breeding_line) && input$bulk_edit_breeding_line != "") {
        update_fields <- c(update_fields, paste0("breeding_line = '", input$bulk_edit_breeding_line, "'"))
        old_values$breeding_line <- selected_data$breeding_line[i]
        new_values$breeding_line <- input$bulk_edit_breeding_line
      }
      if (!is.null(input$bulk_edit_genotype) && input$bulk_edit_genotype != "") {
        update_fields <- c(update_fields, paste0("genotype = '", input$bulk_edit_genotype, "'"))
        old_values$genotype <- selected_data$genotype[i]
        new_values$genotype <- input$bulk_edit_genotype
      }
      if (!is.null(input$bulk_edit_responsible_person) && input$bulk_edit_responsible_person != "") {
        update_fields <- c(update_fields, paste0("responsible_person = '", input$bulk_edit_responsible_person, "'"))
        old_values$responsible_person <- selected_data$responsible_person[i]
        new_values$responsible_person <- input$bulk_edit_responsible_person
      }
      if (!is.null(input$bulk_edit_protocol) && input$bulk_edit_protocol != "") {
        update_fields <- c(update_fields, paste0("protocol = '", input$bulk_edit_protocol, "'"))
        old_values$protocol <- selected_data$protocol[i]
        new_values$protocol <- input$bulk_edit_protocol
      }
      if (!is.null(input$bulk_edit_study_plan) && input$bulk_edit_study_plan != "") {
        update_fields <- c(update_fields, paste0("study_plan = '", input$bulk_edit_study_plan, "'"))
        old_values$study_plan <- selected_data$study_plan[i]
        new_values$study_plan <- input$bulk_edit_study_plan
      }
      if (!is.null(input$bulk_edit_project_code) && input$bulk_edit_project_code != "") {
        update_fields <- c(update_fields, paste0("project_code = '", input$bulk_edit_project_code, "'"))
        old_values$project_code <- selected_data$project_code[i]
        new_values$project_code <- input$bulk_edit_project_code
      }
      if (!is.null(input$bulk_edit_stock_category) && input$bulk_edit_stock_category != "") {
        update_fields <- c(update_fields, paste0("stock_category = '", input$bulk_edit_stock_category, "'"))
        old_values$stock_category <- selected_data$stock_category[i]
        new_values$stock_category <- input$bulk_edit_stock_category
      }
      
      # Add last_updated timestamp
      update_fields <- c(update_fields, "last_updated = CURRENT_TIMESTAMP")
      
      if (length(update_fields) > 1) { # More than just last_updated
        query <- paste0("UPDATE ", TABLE_NAME, " SET ", paste(update_fields, collapse = ", "), " WHERE asu_id = '", asu_id, "'")
        tryCatch({
          DBI::dbExecute(con, query)
          update_count <- update_count + 1
          record_ids <- c(record_ids, asu_id)
          
          # Log individual field changes for detailed audit trail
          for (field_name in names(new_values)) {
            log_field_change(con, TABLE_NAME, asu_id, field_name, 
                           old_values[[field_name]], new_values[[field_name]], 
                           user = 'system', operation_details = "Bulk edit via UI")
          }
        }, error = function(e) {
          # Handle error silently for now
        })
      }
    }
    
    # Log bulk operation summary
    if (length(record_ids) > 0) {
      log_bulk_audit_action(con, TABLE_NAME, "UPDATE", record_ids, 
                           list(old = old_values, new = new_values), 
                           user = 'system', operation_details = "Bulk edit operation")
    }
    
    DBI::dbDisconnect(con)
    
    # Refresh the table data
    con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
    all_data <- DBI::dbGetQuery(con, paste0("SELECT * FROM ", TABLE_NAME, " ORDER BY asu_id"))
    DBI::dbDisconnect(con)
    filtered_data(all_data)
    all_mice_table(all_data)
    
    removeModal()
    
    if (update_count > 0) {
      showNotification(paste("Successfully updated", update_count, "animals."), type = "message", duration = 3)
    } else {
      showNotification("No changes were made. Please fill in at least one field.", type = "warning", duration = 3)
    }
  })
  
  # Handle Bulk Delete button
  observeEvent(input$bulk_delete_btn, {
    selected_rows <- input$all_mice_table_rows_selected
    
    if (length(selected_rows) == 0) {
      showNotification("Please select at least one animal to delete.", type = "warning", duration = 3)
      return()
    }
    
    # Get the selected data
    current_data <- filtered_data()
    selected_data <- current_data[selected_rows, ]
    
    showModal(modalDialog(
      title = "Manage Selected Animals",
      size = "m",
      div(
        style = "margin-bottom: 15px;",
        paste("You have selected", length(selected_rows), "animals. Choose an action:")
      ),
      div(
        style = "max-height: 200px; overflow-y: auto; margin-bottom: 15px;",
        tags$strong("Selected animals:"),
        tags$ul(
          lapply(1:min(10, nrow(selected_data)), function(i) {
            tags$li(paste(selected_data$asu_id[i], "-", selected_data$animal_id[i]))
          })
        ),
        if (nrow(selected_data) > 10) {
          tags$p(paste("... and", nrow(selected_data) - 10, "more"))
        }
      ),
      div(
        style = "margin-bottom: 15px;",
        tags$strong("Available actions:")
      ),
      div(
        style = "margin-bottom: 10px;",
        tags$span("🗑️ ", style = "color: #d32f2f; font-weight: bold;"),
        tags$span("Delete: Mark animals as 'Deleted' and add deletion timestamp")
      ),
      div(
        style = "margin-bottom: 15px;",
        tags$span("💀 ", style = "color: #ff5722; font-weight: bold;"),
        tags$span("Deceased: Mark animals as 'Deceased' and add death timestamp")
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("mark_deleted_btn", "🗑️ Delete", style = "background-color: #d32f2f; color: white; border: none; margin-right: 10px;"),
        actionButton("mark_deceased_btn", "💀 Deceased", style = "background-color: #ff5722; color: white; border: none;")
      )
    ))
  })
  
  # Handle Mark as Deleted
  observeEvent(input$mark_deleted_btn, {
    selected_rows <- input$all_mice_table_rows_selected
    
    if (length(selected_rows) == 0) {
      showNotification("Please select at least one animal to edit.", type = "warning", duration = 3)
      return()
    }
    
    # Get the selected data
    current_data <- filtered_data()
    selected_data <- current_data[selected_rows, ]
    
    # Update selected animals to status 'Deleted' and add deletion timestamp
    con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
    
    update_count <- 0
    record_ids <- c()
    
    for (i in 1:nrow(selected_data)) {
      asu_id <- selected_data$asu_id[i]
      old_status <- selected_data$status[i]
      
      query <- paste0("UPDATE ", TABLE_NAME, " SET status = 'Deleted', last_updated = CURRENT_TIMESTAMP WHERE asu_id = '", asu_id, "'")
      
      tryCatch({
        result <- DBI::dbExecute(con, query)
        if (result > 0) {
          update_count <- update_count + 1
          record_ids <- c(record_ids, asu_id)
          
          # Log the status change
          log_field_change(con, TABLE_NAME, asu_id, "status", 
                          old_status, "Deleted", 
                          user = 'system', operation_details = "Bulk delete via UI")
        }
      }, error = function(e) {
        print(paste("Error updating", asu_id, ":", e$message))
      })
    }
    
    # Log bulk operation summary
    if (length(record_ids) > 0) {
      log_bulk_audit_action(con, TABLE_NAME, "UPDATE", record_ids, 
                           list(status = "Deleted"), 
                           user = 'system', operation_details = "Bulk delete operation")
    }
    
    DBI::dbDisconnect(con)
    
    # Refresh the table data
    con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
    all_data <- DBI::dbGetQuery(con, paste0("SELECT * FROM ", TABLE_NAME, " ORDER BY asu_id"))
    DBI::dbDisconnect(con)
    filtered_data(all_data)
    all_mice_table(all_data)
    
    removeModal()
    
    if (update_count > 0) {
      showNotification(paste("Successfully marked", update_count, "animals as Deleted."), type = "message", duration = 3)
    } else {
      showNotification("No animals were updated.", type = "warning", duration = 3)
    }
  })
  
  # Handle Mark as Deceased
  observeEvent(input$mark_deceased_btn, {
    selected_rows <- input$all_mice_table_rows_selected
    current_data <- filtered_data()
    selected_data <- current_data[selected_rows, ]
    
    # Update selected animals to status 'Deceased' and add death timestamp
    con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
    
    update_count <- 0
    record_ids <- c()
    
    for (i in 1:nrow(selected_data)) {
      asu_id <- selected_data$asu_id[i]
      old_status <- selected_data$status[i]
      
      query <- paste0("UPDATE ", TABLE_NAME, " SET status = 'Deceased', last_updated = CURRENT_TIMESTAMP WHERE asu_id = '", asu_id, "'")
      
      tryCatch({
        result <- DBI::dbExecute(con, query)
        if (result > 0) {
          update_count <- update_count + 1
          record_ids <- c(record_ids, asu_id)
          
          # Log the status change
          log_field_change(con, TABLE_NAME, asu_id, "status", 
                          old_status, "Deceased", 
                          user = 'system', operation_details = "Bulk deceased via UI")
        }
      }, error = function(e) {
        print(paste("Error updating deceased", asu_id, ":", e$message))
      })
    }
    
    # Log bulk operation summary
    if (length(record_ids) > 0) {
      log_bulk_audit_action(con, TABLE_NAME, "UPDATE", record_ids, 
                           list(status = "Deceased"), 
                           user = 'system', operation_details = "Bulk deceased operation")
    }
    
    DBI::dbDisconnect(con)
    
    # Refresh the table data
    con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
    all_data <- DBI::dbGetQuery(con, paste0("SELECT * FROM ", TABLE_NAME, " ORDER BY asu_id"))
    DBI::dbDisconnect(con)
    filtered_data(all_data)
    all_mice_table(all_data)
    
    removeModal()
    
    if (update_count > 0) {
      showNotification(paste("Successfully marked", update_count, "animals as Deceased."), type = "message", duration = 3)
    } else {
      showNotification("No animals were updated.", type = "warning", duration = 3)
    }
  })
  
  # Handle double-click on mouse row to show history tracing
  observeEvent(input$mouse_double_click, {
    req(input$mouse_double_click)
    asu_id <- input$mouse_double_click
    row_index <- input$mouse_double_click_row
    
    # Debug: Print the received ASU ID and row index
    cat("Received ASU ID:", asu_id, "\n")
    cat("Received row index:", row_index, "\n")
    
    # If ASU ID is empty or invalid, try to get it from the row index
    if (is.null(asu_id) || asu_id == "" || asu_id == "NA") {
      display_data <- filtered_data()
      if (!is.null(display_data) && !is.null(row_index) && row_index >= 0 && row_index < nrow(display_data)) {
        # Get the ASU ID from the original data using the row index
        original_data <- all_mice_table()
        if (!is.null(original_data) && row_index < nrow(original_data)) {
          asu_id <- original_data$asu_id[row_index + 1]  # +1 because R is 1-indexed
          cat("Retrieved ASU ID from row index:", asu_id, "\n")
        }
      }
    }
    
    # Call the mouse history tracing function from the separate modal file
    if (!is.null(asu_id) && asu_id != "" && asu_id != "NA") {
      show_mouse_history_tracing(input, output, session, asu_id, all_mice_table)
    } else {
      showNotification("Could not retrieve mouse information. Please try again.", type = "error", duration = 3)
    }
  })

  # Render bulk delete button based on lock state
  output$bulk_delete_btn_ui <- renderUI({
    if (!is_system_locked()) {
      actionButton("bulk_delete_btn", "Delete Selected", 
                  style = "background-color: #d32f2f; color: white; border: none;")
    } else {
      NULL
    }
  })

  # Validation workflow function to decide if modal should be activated
  validate_selection_for_plugging <- function() {
    # Get selected rows from the table
    selected_rows <- input$all_mice_table_rows_selected
    print(selected_rows) ##debugging
    
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showModal(modalDialog(
        title = "No Mice Selected",
        "Please select mice from the table first before adding a plugging event.",
        footer = modalButton("OK"),
        size = "s"
      ))
      return(FALSE)
    }
    
    # Check maximum 3 mice condition
    if (length(selected_rows) > 3) {
      showModal(modalDialog(
        title = "Too Many Mice Selected",
        paste("You have selected", length(selected_rows), "mice. Maximum allowed is 3 mice for plugging."),
        footer = modalButton("OK"),
        size = "s"
      ))
      return(FALSE)
    }
    
    # Get the current filtered data
    current_data <- filtered_data()
    if (is.null(current_data) || nrow(current_data) == 0) {
      showModal(modalDialog(
        title = "No Data Available",
        "No data is currently available in the table.",
        footer = modalButton("OK"),
        size = "s"
      ))
      return(FALSE)
    }
    
    # Get selected mice ASU IDs
    selected_asu_ids <- current_data$asu_id[selected_rows]
    
    # Use get_mouse_info to check each selected mouse
    warning_messages <- list()
    alive_mice <- list()
    male_count <- 0
    female_count <- 0
    
    for (asu_id in selected_asu_ids) {
      # Function 3: Get mouse info (unified version with status)
      mouse_info <- get_mouse_info(asu_id, include_status = TRUE)
      
      if (is.null(mouse_info)) {
        warning_messages[[paste0("not_found_", asu_id)]] <- paste("Mouse", asu_id, "not found in database")
        next
      }
      
      # Check if mouse is alive
      if (mouse_info$status != "Alive") {
        warning_messages[[paste0("not_alive_", asu_id)]] <- paste("Mouse", asu_id, "is not alive (status:", mouse_info$status, ")")
        next
      }
      
      # Function 4: Validate mice active status for plugging
      plugging_check <- validate_mice_active_status(
        mice_id = asu_id,
        table_name = "plugging_history", 
        status_column = "plugging_status",
        mouse_role = ifelse(mouse_info$gender == "Male", "male", "female")
      )
      
      # Check stock status
      stock_check <- validate_mice_active_status(
        mice_id = asu_id,
        table_name = "mice_stock",
        status_column = "status"
      )
      
      # Define active plugging statuses
      active_plugging_statuses <- c("Ongoing", "Plugged", "Not Observed (Waiting for confirmation)")
      active_plugging_count <- sum(plugging_check$all_statuses %in% active_plugging_statuses)
      
      # Check for warnings based on gender
      if (mouse_info$gender == "Male") {
        male_count <- male_count + 1
        
        if (active_plugging_count >= 2) {
          warning_messages[[paste0("male_plugging_", asu_id)]] <- paste("Male", asu_id, "has", active_plugging_count, "active plugging records")
        }
        
        if (mouse_info$age_weeks < 7) {
          warning_messages[[paste0("male_age_", asu_id)]] <- paste("Male", asu_id, "is only", mouse_info$age_weeks, "weeks old (minimum: 7 weeks)")
        }
      } else if (mouse_info$gender == "Female") {
        female_count <- female_count + 1
        
        if (active_plugging_count >= 1) {
          warning_messages[[paste0("female_plugging_", asu_id)]] <- paste("Female", asu_id, "has", active_plugging_count, "active plugging records")
        }
        
        if (mouse_info$age_weeks < 7) {
          warning_messages[[paste0("female_age_", asu_id)]] <- paste("Female", asu_id, "is only", mouse_info$age_weeks, "weeks old (minimum: 7 weeks)")
        }
      }
      
      # Check stock status
      if (!any(stock_check$all_statuses == "Alive")) {
        warning_messages[[paste0("stock_status_", asu_id)]] <- paste("Mouse", asu_id, "stock status issue:", stock_check$status_summary)
      }
      
      # Add to alive mice list if passed all checks
      alive_mice[[asu_id]] <- mouse_info
    }
    
    # Check gender distribution
    if (male_count == 0) {
      showModal(modalDialog(
        title = "⚠️ No Male Selected",
        div(
          style = "color: #d32f2f;",
          tags$strong("Cannot add plugging event:"),
          tags$br(), tags$br(),
          "You must select at least one male mouse for plugging.",
          tags$br(), tags$br(),
          tags$div(
            style = "background-color: #fff3e0; padding: 10px; border-radius: 4px; border-left: 4px solid #ff9800;",
            tags$strong("How to fix:"),
            tags$br(),
            "• Select one male mouse from the table",
            tags$br(),
            "• Ensure the male is alive and at least 7 weeks old"
          )
        ),
        footer = modalButton("OK"),
        size = "m"
      ))
      return(FALSE)
    }
    
    if (male_count > 1) {
      showModal(modalDialog(
        title = "⚠️ Too Many Males Selected",
        div(
          style = "color: #d32f2f;",
          tags$strong("Cannot add plugging event:"),
          tags$br(), tags$br(),
          paste("You have selected", male_count, "males. Maximum allowed is 1 male for plugging."),
          tags$br(), tags$br(),
          tags$div(
            style = "background-color: #fff3e0; padding: 10px; border-radius: 4px; border-left: 4px solid #ff9800;",
            tags$strong("How to fix:"),
            tags$br(),
            "• Select only one male mouse",
            tags$br(),
            "• You can select 1-2 female mice to pair with the male"
          )
        ),
        footer = modalButton("OK"),
        size = "m"
      ))
      return(FALSE)
    }
    
    if (female_count == 0) {
      showModal(modalDialog(
        title = "⚠️ No Female Selected",
        div(
          style = "color: #d32f2f;",
          tags$strong("Cannot add plugging event:"),
          tags$br(), tags$br(),
          "You must select at least one female mouse for plugging.",
          tags$br(), tags$br(),
          tags$div(
            style = "background-color: #fff3e0; padding: 10px; border-radius: 4px; border-left: 4px solid #ff9800;",
            tags$strong("How to fix:"),
            tags$br(),
            "• Select one or two female mice from the table",
            tags$br(),
            "• Ensure the females are alive and at least 7 weeks old"
          )
        ),
        footer = modalButton("OK"),
        size = "m"
      ))
      return(FALSE)
    }
    
    # If there are warnings, show them and prevent modal opening
    if (length(warning_messages) > 0) {
      showModal(modalDialog(
        title = "⚠️ Validation Failed - Cannot Add Plugging Event",
        div(
          style = "color: #d32f2f;",
          tags$strong("The following issues prevent adding a plugging event:"),
          tags$br(), tags$br(),
          tags$ul(
            lapply(warning_messages, function(msg) tags$li(msg))
          ),
          tags$br(),
          tags$div(
            style = "background-color: #fff3e0; padding: 10px; border-radius: 4px; border-left: 4px solid #ff9800;",
            tags$strong("How to fix:"),
            tags$br(),
            "• Select different mice that meet the requirements",
            tags$br(),
            "• Ensure mice are alive and at least 7 weeks old",
            tags$br(),
            "• Check that mice don't have too many active plugging records",
            tags$br(),
            "• Select exactly 1 male and 1-2 females"
          )
        ),
        footer = modalButton("OK"),
        size = "l"
      ))
      return(FALSE)
    }
    
    # All validations passed - show success message
    return(TRUE)
  }

  # Handle Add Plugging Event button from All Mice tab - use modular modal
  plugging_modal_in_all_mice_tab <- add_plugging_modal_server(
    "add_plugging_modal_all_mice",
    get_live_mice = function() {
      # Function 2: Get live mice - but only if validation passes
      if (!validate_selection_for_plugging()) {
        return(NULL)  # Return NULL to indicate validation failed
      }
      
      # Get selected rows from the table
      selected_rows <- input$all_mice_table_rows_selected
      current_data <- filtered_data()
      selected_asu_ids <- current_data$asu_id[selected_rows]
      
      # Query database to get detailed information for selected mice
      con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
      tryCatch({
        # Build query with placeholders for multiple ASU IDs
        placeholders <- paste(rep("?", length(selected_asu_ids)), collapse = ",")
        query <- paste0(
          "SELECT asu_id, animal_id, gender, breeding_line, genotype 
           FROM mice_stock 
           WHERE asu_id IN (", placeholders, ") 
           AND status = 'Alive'
           ORDER BY asu_id"
        )
        
        selected_mice <- DBI::dbGetQuery(con, query, params = as.list(selected_asu_ids))
        
        # Return in the expected format for the modal
        list(
          males = selected_mice[selected_mice$gender == "Male", ],
          females = selected_mice[selected_mice$gender == "Female", ]
        )
        
      }, error = function(e) {
        showModal(modalDialog(
          title = "Database Error",
          paste("Error retrieving mouse data:", e$message),
          footer = modalButton("OK"),
          size = "s"
        ))
        NULL  # Return NULL to indicate error
      }, finally = {
        DBI::dbDisconnect(con)
      })
    },
    get_mouse_info = get_mouse_info,
    validate_mice_active_status = validate_mice_active_status,
    db_connect = function() DBI::dbConnect(RSQLite::SQLite(), DB_PATH),
    db_disconnect = function(con) DBI::dbDisconnect(con),
    log_audit_trail = log_audit_trail,
    auto_update_plugging_status_to_unknown = function() {
      # Auto-update function for plugging status
      con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
      tryCatch({
        today <- as.character(Sys.Date())
        # Find all ongoing records where pairing_end_date is before today
        records <- DBI::dbGetQuery(con, 
          "SELECT * FROM plugging_history 
           WHERE plugging_status = 'Ongoing' 
           AND pairing_end_date IS NOT NULL 
           AND pairing_end_date < ?", 
          params = list(today))
        
        for (i in seq_len(nrow(records))) {
          rec <- records[i, ]
          # Update status to Not Observed (Waiting for confirmation)
          DBI::dbExecute(con, 
            "UPDATE plugging_history 
             SET plugging_status = 'Not Observed (Waiting for confirmation)', 
                 updated_at = DATETIME('now') 
             WHERE id = ?", 
            params = list(rec$id))
          
          # Log to audit trail
          log_audit_trail(
            "plugging_history",
            rec$id,
            "UPDATE",
            as.list(rec),
            list(
              plugging_status = "Not Observed (Waiting for confirmation)",
              confirmation_date = as.character(Sys.Date()),
              notes = "Auto-update: pairing period ended, status set to Not Observed (Waiting for confirmation)"
            ),
            user_id = "system(auto)"
          )
        }
      }, finally = {
        DBI::dbDisconnect(con)
      })
    },
    plugging_state = reactiveValues(reload = NULL),
    is_system_locked = is_system_locked,
    global_refresh_trigger = global_refresh_trigger
  )
} 