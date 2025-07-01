all_mice_tab_ui <- function() {
  fluidPage(
    fluidRow(
      column(12, 
        div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 16px;",
          h3("🐭 All Mice", style = "margin: 0; font-size: 1.8em; color: #2c3e50; font-weight: 700;"),
          div(
            style = "display: flex; align-items: center; gap: 8px;",
            div(
              style = "font-size: 12px; color: #6c757d; background: #e9ecef; padding: 4px 8px; border-radius: 4px;",
              "💡 Double-click any row to view Mouse History"
            )
          )
        )
      )
    ),
    fluidRow(
      column(2,
        div(
          class = "search-panel",
          style = "padding: 8px 4px 8px 4px; min-width: 150px; max-width: 220px;",
          h4("🔍 Search & Filter", style = "margin-top: 0; margin-bottom: 10px; font-size: 1.05em; color: #2c3e50;"),
          textInput("all_mice_search_asu_id", "ASU ID", placeholder = "Enter ASU ID", width = "100%"),
          textInput("all_mice_search_animal_id", "Animal ID", placeholder = "Enter Animal ID", width = "100%"),
          selectInput("all_mice_search_gender", "Gender", 
                     choices = c("All" = "", "Male" = "Male", "Female" = "Female"), 
                     selected = "", width = "100%"),
          textInput("all_mice_search_breeding_line", "Breeding Line", placeholder = "Enter Breeding Line", width = "100%"),
          selectizeInput("all_mice_search_responsible_person", "Responsible Person", 
                        choices = c("All" = ""), 
                        options = list(placeholder = "Select responsible person")),
          selectInput("all_mice_search_stock_category", "Stock Category", 
                     choices = c("All" = "", "Experiment" = "Experiment", "Breeding" = "Breeding", "Charles River" = "Charles River"), 
                     selected = "", width = "100%"),
          selectInput("all_mice_search_status", "Status", 
                     choices = c("Live" = "Live", "Deceased" = "Deceased", "Both" = "Both"), 
                     selected = "Live", width = "100%"),
          div(
            style = "margin-top: 6px; padding: 4px; background: #f8f9fa; border-radius: 4px; font-size: 9.5px; color: #6c757d; border-left: 3px solid #17a2b8;",
            "💡 Use * for multiple characters and ? for single character wildcards"
          ),
          actionButton("all_mice_execute_search_btn", "🔍 Search", 
                      style = "background: linear-gradient(135deg, #1976d2 0%, #1565c0 100%); color: white; border: none; width: 100%; margin-top: 6px; font-size: 12px; padding: 6px 8px; border-radius: 6px; font-weight: 500;")
        )
      ),
      column(10,
        div(
          style = "background: white; border-radius: 8px; padding: 10px; box-shadow: 0 2px 4px rgba(0,0,0,0.05);",
          div(
            class = "action-buttons",
            style = "margin-bottom: 10px;",
            actionButton("clear_search_btn", "🗑️ Clear Search", 
                        style = "background: linear-gradient(135deg, #6c757d 0%, #5a6268 100%); color: white; border: none; padding: 5px 10px; font-size: 11px; border-radius: 6px;"),
            actionButton("clear_selection_btn", "🔲 Clear Selection", 
                        style = "background: linear-gradient(135deg, #17a2b8 0%, #138496 100%); color: white; border: none; padding: 5px 10px; font-size: 11px; border-radius: 6px;"),
            actionButton("bulk_edit_btn", "✏️ Edit Selected", 
                        style = "background: linear-gradient(135deg, #ff9800 0%, #f57c00 100%); color: white; border: none; padding: 5px 10px; font-size: 11px; border-radius: 6px;"),
            add_plugging_modal_ui("add_plugging_modal_all_mice"),
            uiOutput("bulk_delete_btn_ui")
          ),
          div(
            style = "margin-bottom: 6px; padding: 5px 8px; background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%); border-radius: 6px; border-left: 4px solid #2196f3; font-size: 11px;",
            "📊 Showing filtered results. Use the search panel to refine your query."
          ),
          div(
            style = "position: relative;",
            uiOutput("loading_overlay"),
            DT::dataTableOutput("all_mice_table")
          )
        )
      )
    )
  )
}

all_mice_tab_server <- function(input, output, session, all_mice_table, is_system_locked = NULL, global_refresh_trigger = NULL, shared_plugging_state = NULL) {
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
  
  # Default shared plugging state if not provided
  if (is.null(shared_plugging_state)) {
    shared_plugging_state <- reactiveValues(
      reload = NULL,
      viewing_id = NULL,
      editing_id = NULL,
      confirming_id = NULL
    )
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
    
    # Add reactive dependency on global refresh trigger to ensure status lights update
    global_refresh_trigger()
    
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
    display_data <- data[, c("asu_id", "animal_id", "gender", "breeding_line", "genotype", "responsible_person", "stock_category", "status")]
    
    # Calculate age in weeks
    display_data$age_weeks <- floor(as.numeric(Sys.Date() - as.Date(data$dob)) / 7) 
    
    # Add status light before ASU ID with improved styling
    display_data$asu_id_with_light <- sapply(display_data$asu_id, function(asu_id) {
      status_tag <- mice_status_tag_all_mice(asu_id)
      
      # Define light colors based on status with better contrast
      light_color <- switch(status_tag,
        "Free" = "#4CAF50",      # Green
        "Busy" = "#FF9800",      # Orange
        "Deceased" = "#F44336",  # Red
        "Unknown" = "#9E9E9E"    # Gray
      )
      
      # Create HTML for the light and ASU ID with improved styling
      paste0(
        '<span class="status-indicator status-', tolower(status_tag), '" style="background-color: ', light_color, ';" title="Status: ', status_tag, '"></span>',
        '<span style="font-weight: 500; color: #2c3e50;">', asu_id, '</span>'
      )
    })
    
    # Reorder columns for display (replace asu_id with asu_id_with_light, exclude dob and last_updated)
    col_order <- c("asu_id_with_light", "animal_id", "gender", "age_weeks", "breeding_line", "genotype", "responsible_person", "stock_category", "status")
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
      "Age (wks)",
      "Breeding Line",
      "Genotype",
      "Responsible Person",
      "Stock Category",
      "Status"
    )
    
    # Enhanced DataTable with better styling and functionality
    DT::datatable(
      display_data,
      options = list(
        pageLength = 100,
        scrollX = TRUE,
        dom = '<"top"lf>rt<"bottom"ip><"clear">',
        columnDefs = list(
          list(width = '120px', targets = 0),  # ASU ID column - give it more width
          list(width = '80px', targets = 3)    # Age column - make it smaller
        ),
        language = list(
          search = "🔍 Search:",
          lengthMenu = "Show _MENU_ entries per page",
          info = "Showing _START_ to _END_ of _TOTAL_ entries"
        )
      ),
      filter = 'none',
      selection = 'multiple',
      escape = FALSE,  # Allow HTML in the ASU ID column
      rownames = FALSE,
      class = 'table table-striped table-hover',
      callback = JS("
        table.on('dblclick', 'tr', function() {
          var data = table.row(this).data();
          if (data && data.length > 0) {
            var asuIdCell = data[0];
            var asuId = asuIdCell.replace(/<[^>]*>/g, '').trim();
            Shiny.setInputValue('mouse_double_click', asuId, {priority: 'event'});
          }
        });
      ")
    )
  })
  
  # Loading overlay for table operations
  output$loading_overlay <- renderUI({
    # This will be shown/hidden based on reactive triggers
    NULL
  })
  
  # Handle All Mice tab search execution with loading states
  observeEvent(input$all_mice_execute_search_btn, {
    # Show loading notification
    showNotification("🔍 Searching...", type = "default", duration = NULL, id = "search_loading")
    
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
    
    # Remove loading notification and show results
    removeNotification("search_loading")
    
    if (nrow(search_results) == 0) {
      showNotification("❌ No animals found matching your search criteria.", type = "warning", duration = 3)
    } else {
      showNotification(paste("✅ Found", nrow(search_results), "animals matching your search criteria."), type = "message", duration = 3)
    }
  })
  
  # Handle Clear Search button
  observeEvent(input$clear_search_btn, {
    # Clear all search form fields
    updateTextInput(session, "all_mice_search_asu_id", value = "")
    updateTextInput(session, "all_mice_search_animal_id", value = "")
    updateSelectInput(session, "all_mice_search_gender", selected = "")
    updateTextInput(session, "all_mice_search_breeding_line", value = "")
    updateSelectizeInput(session, "all_mice_search_responsible_person", selected = "")
    updateSelectInput(session, "all_mice_search_stock_category", selected = "")
    updateSelectInput(session, "all_mice_search_status", selected = "Live")
    
    # Reset to default filter (show only alive mice)
    con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
    alive_data <- DBI::dbGetQuery(con, paste0("SELECT * FROM ", TABLE_NAME, " WHERE status = 'Alive' ORDER BY asu_id"))
    DBI::dbDisconnect(con)
    filtered_data(alive_data)
    all_mice_table(alive_data)
  })
  
  # Handle Clear Selection button
  observeEvent(input$clear_selection_btn, {
    # Clear all selected rows in the DataTable
    # This will trigger the table to re-render with no selections
    dataTableProxy("all_mice_table") %>% selectRows(NULL)
    
    # Show notification
    showNotification("Selection cleared", type = "message", duration = 2)
  })
  
  # Populate responsible person dropdown for All Mice tab search
  observe({
    con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
    responsible_persons <- unique(DBI::dbGetQuery(con, paste0("SELECT DISTINCT responsible_person FROM ", TABLE_NAME, " WHERE responsible_person IS NOT NULL"))$responsible_person)
    DBI::dbDisconnect(con)
    
    updateSelectizeInput(session, "all_mice_search_responsible_person", 
                        choices = c("All" = "", responsible_persons))
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
    display_data <- data[, c("asu_id", "animal_id", "gender", "breeding_line", "genotype", "responsible_person", "stock_category", "status")]
    selected_asu_ids <- display_data[selected_visible_rows, "asu_id"]
    
    # Get complete data for selected animals from database to ensure all columns are available
    con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
    selected_data <- DBI::dbGetQuery(con, paste0("SELECT * FROM ", TABLE_NAME, " WHERE asu_id IN (", 
                                                 paste(paste0("'", selected_asu_ids, "'"), collapse = ","), 
                                                 ") ORDER BY asu_id"))
    # Get existing values from database for dropdowns (keep connection open)
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
    display_data <- data[, c("asu_id", "animal_id", "gender", "breeding_line", "genotype", "responsible_person", "stock_category", "status")]
    selected_asu_ids <- display_data[selected_visible_rows, "asu_id"]
    
    # Get complete data for selected animals from database to ensure all columns are available
    con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
    selected_data <- DBI::dbGetQuery(con, paste0("SELECT * FROM ", TABLE_NAME, " WHERE asu_id IN (", 
                                                 paste(paste0("'", selected_asu_ids, "'"), collapse = ","), 
                                                 ") ORDER BY asu_id"))
    # Get existing values from database for dropdowns (keep connection open)
    breeding_lines <- unique(DBI::dbGetQuery(con, paste0("SELECT DISTINCT breeding_line FROM ", TABLE_NAME, " WHERE breeding_line IS NOT NULL"))$breeding_line)
    genotypes <- unique(DBI::dbGetQuery(con, paste0("SELECT DISTINCT genotype FROM ", TABLE_NAME, " WHERE genotype IS NOT NULL"))$genotype)
    responsible_persons <- unique(DBI::dbGetQuery(con, paste0("SELECT DISTINCT responsible_person FROM ", TABLE_NAME, " WHERE responsible_person IS NOT NULL"))$responsible_person)
    project_codes <- unique(DBI::dbGetQuery(con, paste0("SELECT DISTINCT project_code FROM ", TABLE_NAME, " WHERE project_code IS NOT NULL"))$project_code)
    DBI::dbDisconnect(con)
    
    # Collect bulk edit data for validation (only fields that are actually being changed)
    validation_data <- list()
    
    # Check if any fields have actually changed by comparing with the first selected record
    # (assuming all selected records have the same current values for common fields)
    if (nrow(selected_data) > 0) {
      first_record <- selected_data[1, ]
      
      # Gender
      current_gender <- ifelse(is.na(first_record$gender), "", first_record$gender)
      form_gender <- ifelse(is.null(input$bulk_edit_gender), "", input$bulk_edit_gender)
      if (form_gender != "" && form_gender != current_gender) {
        validation_data$gender <- form_gender
      }
      
      # Status
      current_status <- ifelse(is.na(first_record$status), "", first_record$status)
      form_status <- ifelse(is.null(input$bulk_edit_status), "", input$bulk_edit_status)
      if (form_status != "" && form_status != current_status) {
        validation_data$status <- form_status
      }
      
      # Breeding Line
      current_breeding_line <- ifelse(is.na(first_record$breeding_line), "", first_record$breeding_line)
      form_breeding_line <- ifelse(is.null(input$bulk_edit_breeding_line), "", input$bulk_edit_breeding_line)
      if (form_breeding_line != "" && form_breeding_line != current_breeding_line) {
        validation_data$breeding_line <- form_breeding_line
      }
      
      # Genotype
      current_genotype <- ifelse(is.na(first_record$genotype), "", first_record$genotype)
      form_genotype <- ifelse(is.null(input$bulk_edit_genotype), "", input$bulk_edit_genotype)
      if (form_genotype != "" && form_genotype != current_genotype) {
        validation_data$genotype <- form_genotype
      }
      
      # Responsible Person
      current_responsible_person <- ifelse(is.na(first_record$responsible_person), "", first_record$responsible_person)
      form_responsible_person <- ifelse(is.null(input$bulk_edit_responsible_person), "", input$bulk_edit_responsible_person)
      if (form_responsible_person != "" && form_responsible_person != current_responsible_person) {
        validation_data$responsible_person <- form_responsible_person
      }
      
      # Protocol
      current_protocol <- ifelse(is.na(first_record$protocol), "", first_record$protocol)
      form_protocol <- ifelse(is.null(input$bulk_edit_protocol), "", input$bulk_edit_protocol)
      if (form_protocol != "" && form_protocol != current_protocol) {
        validation_data$protocol <- form_protocol
      }
      
      # Study Plan
      current_study_plan <- ifelse(is.na(first_record$study_plan), "", first_record$study_plan)
      form_study_plan <- ifelse(is.null(input$bulk_edit_study_plan), "", input$bulk_edit_study_plan)
      if (form_study_plan != "" && form_study_plan != current_study_plan) {
        validation_data$study_plan <- form_study_plan
      }
      
      # Project Code
      current_project_code <- ifelse(is.na(first_record$project_code), "", first_record$project_code)
      form_project_code <- ifelse(is.null(input$bulk_edit_project_code), "", input$bulk_edit_project_code)
      if (form_project_code != "" && form_project_code != current_project_code) {
        validation_data$project_code <- form_project_code
      }
      
      # Stock Category
      current_stock_category <- ifelse(is.na(first_record$stock_category), "", first_record$stock_category)
      form_stock_category <- ifelse(is.null(input$bulk_edit_stock_category), "", input$bulk_edit_stock_category)
      if (form_stock_category != "" && form_stock_category != current_stock_category) {
        validation_data$stock_category <- form_stock_category
      }
    }
    
    # Validate bulk edit data
    if (length(validation_data) > 0) {
      validation_result <- validate_mouse_data(validation_data, require_all_fields = FALSE)
      
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
    
    # Create a new connection for the update operations
    con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
    
    for (i in 1:nrow(selected_data)) {
      asu_id <- selected_data$asu_id[i]
      update_fields <- c()
      old_values <- list()
      new_values <- list()
      
      # Check each field and add to update if it has actually changed
      # Gender
      current_gender <- ifelse(is.na(selected_data$gender[i]), "", selected_data$gender[i])
      form_gender <- ifelse(is.null(input$bulk_edit_gender), "", input$bulk_edit_gender)
      if (form_gender != "" && form_gender != current_gender) {
        update_fields <- c(update_fields, paste0("gender = '", form_gender, "'"))
        old_values$gender <- current_gender
        new_values$gender <- form_gender
      }
      
      # Status
      current_status <- ifelse(is.na(selected_data$status[i]), "", selected_data$status[i])
      form_status <- ifelse(is.null(input$bulk_edit_status), "", input$bulk_edit_status)
      if (form_status != "" && form_status != current_status) {
        update_fields <- c(update_fields, paste0("status = '", form_status, "'"))
        old_values$status <- current_status
        new_values$status <- form_status
      }
      
      # Breeding Line
      current_breeding_line <- ifelse(is.na(selected_data$breeding_line[i]), "", selected_data$breeding_line[i])
      form_breeding_line <- ifelse(is.null(input$bulk_edit_breeding_line), "", input$bulk_edit_breeding_line)
      if (form_breeding_line != "" && form_breeding_line != current_breeding_line) {
        update_fields <- c(update_fields, paste0("breeding_line = '", form_breeding_line, "'"))
        old_values$breeding_line <- current_breeding_line
        new_values$breeding_line <- form_breeding_line
      }
      
      # Genotype
      current_genotype <- ifelse(is.na(selected_data$genotype[i]), "", selected_data$genotype[i])
      form_genotype <- ifelse(is.null(input$bulk_edit_genotype), "", input$bulk_edit_genotype)
      if (form_genotype != "" && form_genotype != current_genotype) {
        update_fields <- c(update_fields, paste0("genotype = '", form_genotype, "'"))
        old_values$genotype <- current_genotype
        new_values$genotype <- form_genotype
      }
      
      # Responsible Person
      current_responsible_person <- ifelse(is.na(selected_data$responsible_person[i]), "", selected_data$responsible_person[i])
      form_responsible_person <- ifelse(is.null(input$bulk_edit_responsible_person), "", input$bulk_edit_responsible_person)
      if (form_responsible_person != "" && form_responsible_person != current_responsible_person) {
        update_fields <- c(update_fields, paste0("responsible_person = '", form_responsible_person, "'"))
        old_values$responsible_person <- current_responsible_person
        new_values$responsible_person <- form_responsible_person
      }
      
      # Protocol
      current_protocol <- ifelse(is.na(selected_data$protocol[i]), "", selected_data$protocol[i])
      form_protocol <- ifelse(is.null(input$bulk_edit_protocol), "", input$bulk_edit_protocol)
      if (form_protocol != "" && form_protocol != current_protocol) {
        update_fields <- c(update_fields, paste0("protocol = '", form_protocol, "'"))
        old_values$protocol <- current_protocol
        new_values$protocol <- form_protocol
      }
      
      # Study Plan
      current_study_plan <- ifelse(is.na(selected_data$study_plan[i]), "", selected_data$study_plan[i])
      form_study_plan <- ifelse(is.null(input$bulk_edit_study_plan), "", input$bulk_edit_study_plan)
      if (form_study_plan != "" && form_study_plan != current_study_plan) {
        update_fields <- c(update_fields, paste0("study_plan = '", form_study_plan, "'"))
        old_values$study_plan <- current_study_plan
        new_values$study_plan <- form_study_plan
      }
      
      # Project Code
      current_project_code <- ifelse(is.na(selected_data$project_code[i]), "", selected_data$project_code[i])
      form_project_code <- ifelse(is.null(input$bulk_edit_project_code), "", input$bulk_edit_project_code)
      if (form_project_code != "" && form_project_code != current_project_code) {
        update_fields <- c(update_fields, paste0("project_code = '", form_project_code, "'"))
        old_values$project_code <- current_project_code
        new_values$project_code <- form_project_code
      }
      
      # Stock Category
      current_stock_category <- ifelse(is.na(selected_data$stock_category[i]), "", selected_data$stock_category[i])
      form_stock_category <- ifelse(is.null(input$bulk_edit_stock_category), "", input$bulk_edit_stock_category)
      if (form_stock_category != "" && form_stock_category != current_stock_category) {
        update_fields <- c(update_fields, paste0("stock_category = '", form_stock_category, "'"))
        old_values$stock_category <- current_stock_category
        new_values$stock_category <- form_stock_category
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
          warning(paste("Error updating", asu_id, ":", e$message))
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
      
              query <- paste0("UPDATE ", TABLE_NAME, " SET status = 'Deleted', last_updated = CURRENT_TIMESTAMP WHERE asu_id = ?")
        result <- DBI::dbExecute(con, query, params = list(asu_id))
      
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
        warning(paste("Error updating", asu_id, ":", e$message))
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
      
              query <- paste0("UPDATE ", TABLE_NAME, " SET status = 'Deceased', last_updated = CURRENT_TIMESTAMP WHERE asu_id = ?")
        result <- DBI::dbExecute(con, query, params = list(asu_id))
      
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
        warning(paste("Error updating deceased", asu_id, ":", e$message))
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
    
    # If ASU ID is empty or invalid, try to get it from the row index
    if (is.null(asu_id) || asu_id == "" || asu_id == "NA") {
      display_data <- filtered_data()
      if (!is.null(display_data) && !is.null(row_index) && row_index >= 0 && row_index < nrow(display_data)) {
        # Get the ASU ID from the original data using the row index
        original_data <- all_mice_table()
        if (!is.null(original_data) && row_index < nrow(original_data)) {
          asu_id <- original_data$asu_id[row_index + 1]  # +1 because R is 1-indexed
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
                  style = "background-color: #d32f2f; color: white; border: none; margin-right: 8px; padding: 6px 12px; font-size: 13px;")
    } else {
      NULL
    }
  })

  # Validation workflow function to decide if modal should be activated
  validate_selection_for_plugging <- function() {
    # Get selected rows from the table
    selected_rows <- input$all_mice_table_rows_selected
    
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
      active_plugging_statuses <- c("Ongoing", "Plugged", "Not Observed (Waiting for confirmation)", "Surprising Plug!!")
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
    plugging_state = shared_plugging_state,
    is_system_locked = is_system_locked,
    global_refresh_trigger = global_refresh_trigger,
    force_refresh_all_mice_table = function() {
      # Force refresh the all_mice table by updating filtered_data and all_mice_table
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
    }
  )
} 