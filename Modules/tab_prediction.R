suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(RSQLite)
  library(plotly)
  library(DT)
})

prediction_tab_ui <- function() {
  fluidPage(
    fluidRow(
      column(
        3,
        div(
          style = "position: sticky; top: 20px; align-self: flex-start; background: #ffffff; border: 1px solid #e5e7eb; border-radius: 10px; padding: 16px; box-shadow: 0 4px 16px rgba(15, 23, 42, 0.06);",
          h4("Prediction Setup", style = "margin-top: 0; color: #1e3a5f;"),
          uiOutput("prediction_breeding_line_filter_ui"),
          selectInput(
            "prediction_breeding_line_mode",
            "Breeding Line Modeling",
            choices = c(
              "Use breeding line as model input" = "feature",
              "Ignore breeding line" = "pooled",
              "Train only on the current breeding line" = "line_specific"
            ),
            selected = "feature"
          ),
          uiOutput("prediction_breeding_line_mode_help"),
          sliderInput("prediction_age_range", "Training Female Age (weeks)", min = 5, max = 60, value = c(7, 30), step = 1),
          div(
            style = "margin-top: 12px; color: #64748b; font-size: 0.95em;",
            "Filters limit which completed historical cases are used to build the pregnancy model curve shown on the right."
          ),
          div(
            style = "margin-top: 12px; display: flex; flex-direction: column; gap: 8px;",
            actionButton("prediction_retrain_models", "Retrain Saved Models", class = "btn-primary", style = "width: 100%; font-weight: 600;"),
            div(
              style = "color: #64748b; font-size: 0.85em; line-height: 1.35;",
              "Classifier training uses pregnant and not-pregnant events. Embryo-count fitting uses pregnant collected events only."
            ),
            uiOutput("prediction_saved_model_status_ui")
          ),
          div(
            style = "margin-top: 10px; padding-top: 10px; border-top: 1px solid #e2e8f0;",
            actionButton("prediction_browse_models", "Browse Saved Models", class = "btn-info", style = "width: 100%; font-weight: 600;")
          )
        )
      ),
      column(
        9,
        div(
          style = "background: white; border: 1px solid #e5e7eb; border-radius: 10px; padding: 18px;",
          div(
            style = "margin-bottom: 12px; padding: 12px 14px; background: linear-gradient(135deg, rgba(245, 158, 11, 0.08) 0%, rgba(59, 130, 246, 0.06) 100%); border: 1px solid #e2e8f0; border-radius: 10px; color: #475569; line-height: 1.45;",
            div(style = "font-weight: 700; color: #1e3a5f; margin-bottom: 4px;", "Prediction Overview"),
            "The prediction function uses completed plugging outcomes and body weight trends to estimate pregnancy likelihood for current events. When enough collected data is available, it also supports embryo-count estimation. The Prediction Curve tab summarizes the historical training pattern behind the model, while Training Records lets you inspect the source cases used to build that pattern."
          ),
          tabsetPanel(
            id = "prediction_main_view",
            tabPanel(
              "Prediction Curve",
              div(style = "height: 520px; margin-top: 12px;", plotlyOutput("prediction_weight_plot", height = "520px"))
            ),
            tabPanel(
              "Training Records",
              div(
                style = "margin-top: 12px; display: flex; align-items: center; justify-content: space-between; gap: 16px; color: #64748b;",
                tags$div(
                  style = "flex: 1 1 auto; min-width: 0;",
                  "Double-click a row to inspect the source record. Collected rows open the collection report editor directly."
                ),
                tags$div(
                  style = "flex: 0 0 auto; display: flex; justify-content: flex-end; align-items: center; gap: 10px; white-space: nowrap;",
                  tags$input(
                    id = "prediction_focus_pregnant",
                    type = "checkbox",
                    style = "width: 20px; height: 20px; margin: 0; accent-color: #2563eb; cursor: pointer;"
                  ),
                  tags$span("Focus pregnant events only", style = "display: inline-flex; align-items: center; line-height: 1;")
                )
              ),
              DT::dataTableOutput("prediction_training_table")
            )
          )
        )
      )
    )
  )
}

prediction_tab_server <- function(input, output, session, shared_plugging_state = NULL, global_refresh_trigger = NULL) {
  if (is.null(global_refresh_trigger)) {
    global_refresh_trigger <- reactiveVal(Sys.time())
  }

  saved_model_status <- reactiveVal(load_prediction_model_registry())
  saved_model_choices_version <- reactiveVal(Sys.time())

  prediction_state <- if (is.null(shared_plugging_state)) {
    reactiveValues(prediction_target_id = NULL, viewing_id = NULL, open_details_id = NULL, open_collection_id = NULL, open_edit_id = NULL, prediction_breeding_line_mode = "feature")
  } else {
    shared_plugging_state
  }
  
  previous_training_filter_state <- reactiveVal(list(
    age_range = NULL,
    breeding_lines = NULL,
    focus_pregnant = FALSE,
    breeding_mode = "feature"
  ))
  prediction_training_table_page <- reactiveVal(0L)
  prediction_training_table_page_length <- reactiveVal(25L)

  observe({
    prediction_state$prediction_breeding_line_mode <- normalize_prediction_breeding_line_mode(input$prediction_breeding_line_mode)
  })

  output$prediction_breeding_line_mode_help <- renderUI({
    div(
      style = "margin-top: -6px; margin-bottom: 10px; color: #64748b; font-size: 0.92em;",
      describe_prediction_breeding_line_mode(input$prediction_breeding_line_mode)
    )
  })

  prediction_dataset <- reactive({
    global_refresh_trigger()
    build_plugging_prediction_dataset(DB_PATH)
  })

  selected_prediction_training_line <- reactive({
    selected_lines <- input$prediction_breeding_lines
    if (is.null(selected_lines) || length(selected_lines) == 0 || "All" %in% selected_lines) {
      return(NA_character_)
    }

    unique_lines <- unique(stats::na.omit(selected_lines))
    if (length(unique_lines) == 1) unique_lines[[1]] else NA_character_
  })

  output$prediction_breeding_line_filter_ui <- renderUI({
    dataset <- prediction_dataset()
    lines <- sort(unique(stats::na.omit(dataset$female_breeding_line)))
    checkboxGroupInput(
      "prediction_breeding_lines",
      "Training Breeding Lines",
      choices = c("All", lines),
      selected = "All"
    )
  })

  output$prediction_saved_model_status_ui <- renderUI({
    registry <- saved_model_status()

    if (is.null(registry) || is.null(registry$metadata)) {
      return(div(
        style = "color: #64748b; font-size: 0.9em;",
        "No saved model registry yet. Use Retrain Saved Models to create one."
      ))
    }

    div(
      style = "color: #475569; font-size: 0.9em; line-height: 1.4;",
      div(strong("Saved model: "), registry$metadata$breeding_line_description),
      div(strong("Saved at: "), ifelse(is.null(registry$metadata$saved_at) || is.na(registry$metadata$saved_at) || registry$metadata$saved_at == "", "Unknown", registry$metadata$saved_at))
    )
  })

  available_saved_model_choices <- reactive({
    saved_model_choices_version()
    list_prediction_model_registry_choices()
  })

  selected_saved_model_entry <- reactive({
    selected_path <- input$prediction_saved_model_choice
    registry_choices <- available_saved_model_choices()

    if (is.null(selected_path) || !nzchar(selected_path)) {
      return(NULL)
    }

    matched <- Filter(function(entry) identical(entry$path, selected_path), registry_choices)
    if (length(matched) == 0) {
      return(NULL)
    }

    matched[[1]]
  })

  output$prediction_saved_model_selector_ui <- renderUI({
    registry_choices <- available_saved_model_choices()

    if (length(registry_choices) == 0) {
      return(NULL)
    }

    choice_values <- vapply(registry_choices, function(entry) entry$path, character(1))
    choice_labels <- vapply(registry_choices, function(entry) entry$label, character(1))
    active_choice <- choice_values[[which.max(vapply(registry_choices, function(entry) isTRUE(entry$is_active), logical(1)))]]

    div(
      style = "padding-top: 4px;",
      selectInput(
        "prediction_saved_model_choice",
        "Load / Switch Saved Model",
        choices = stats::setNames(choice_values, choice_labels),
        selected = active_choice
      ),
      div(
        style = "display: flex; gap: 8px; margin-top: 4px;",
        actionButton("prediction_activate_saved_model", "Load Selected Model", class = "btn-default btn-sm"),
        actionButton("prediction_delete_saved_model", "Delete Selected Backup", class = "btn-danger btn-sm"),
        actionButton("prediction_refresh_saved_models", "Refresh List", class = "btn-link btn-sm", style = "padding-left: 0;")
      ),
      div(
        style = "margin-top: 4px; color: #64748b; font-size: 0.85em;",
        "The active model is used by Plugging and Calendar prediction cards when it matches the current modeling mode."
      )
    )
  })

  observeEvent(input$prediction_browse_models, {
    saved_model_status(load_prediction_model_registry())
    saved_model_choices_version(Sys.time())

    showModal(modalDialog(
      title = "Saved Prediction Models",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      div(
        style = "display: flex; flex-direction: column; gap: 12px;",
        div(
          style = "background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px; padding: 12px;",
          uiOutput("prediction_saved_model_status_ui")
        ),
        div(
          style = "background: #ffffff; border: 1px solid #e2e8f0; border-radius: 8px; padding: 12px;",
          uiOutput("prediction_saved_model_selector_ui")
        ),
        div(
          style = "background: #ffffff; border: 1px solid #e2e8f0; border-radius: 8px; padding: 12px;",
          h5("Available Saved Models", style = "margin-top: 0; color: #1e3a5f;"),
          DT::dataTableOutput("prediction_saved_model_inventory")
        )
      )
    ))
  }, ignoreInit = TRUE)

  output$prediction_saved_model_inventory <- DT::renderDataTable({
    registry_choices <- available_saved_model_choices()
    req(length(registry_choices) > 0, cancelOutput = TRUE)

    display_rows <- data.frame(
      Path = vapply(registry_choices, function(entry) entry$path, character(1)),
      Status = vapply(registry_choices, function(entry) if (isTRUE(entry$is_active)) "Active" else "Backup", character(1)),
      Mode = vapply(registry_choices, function(entry) entry$description, character(1)),
      Saved = vapply(registry_choices, function(entry) entry$saved_at, character(1)),
      Events = vapply(registry_choices, function(entry) ifelse(is.na(entry$total_events), "-", as.character(entry$total_events)), character(1)),
      Pregnant = vapply(registry_choices, function(entry) ifelse(is.na(entry$pregnant_events), "-", as.character(entry$pregnant_events)), character(1)),
      NotPreg = vapply(registry_choices, function(entry) ifelse(is.na(entry$not_pregnant_events), "-", as.character(entry$not_pregnant_events)), character(1)),
      File = vapply(registry_choices, function(entry) entry$file_name, character(1)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    DT::datatable(
      display_rows,
      rownames = FALSE,
      selection = list(mode = "single", selected = which(vapply(registry_choices, function(entry) isTRUE(entry$is_active), logical(1)))),
      escape = TRUE,
      options = list(
        dom = "t",
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        ordering = FALSE,
        scrollX = TRUE,
        columnDefs = list(list(visible = FALSE, targets = 0))
      ),
      colnames = c("Path", "Status", "Model Mode", "Saved At", "Events", "Preg", "Not Preg", "File")
    )
  })

  observeEvent(input$prediction_saved_model_inventory_rows_selected, {
    selected_row <- input$prediction_saved_model_inventory_rows_selected
    req(length(selected_row) == 1)

    registry_choices <- available_saved_model_choices()
    req(selected_row >= 1, selected_row <= length(registry_choices))

    updateSelectInput(
      session,
      "prediction_saved_model_choice",
      selected = registry_choices[[selected_row]]$path
    )
  }, ignoreInit = TRUE)

  observeEvent(input$prediction_refresh_saved_models, {
    saved_model_status(load_prediction_model_registry())
    saved_model_choices_version(Sys.time())
  }, ignoreInit = TRUE)

  observeEvent(input$prediction_activate_saved_model, {
    selected_registry_path <- input$prediction_saved_model_choice
    req(!is.null(selected_registry_path), nzchar(selected_registry_path))

    activation_result <- tryCatch(
      activate_prediction_model_registry(selected_registry_path),
      error = function(e) e
    )

    if (inherits(activation_result, "error")) {
      showNotification(paste("Failed to load the selected model:", activation_result$message), type = "error")
      return()
    }

    saved_model_status(load_prediction_model_registry())
    saved_model_choices_version(Sys.time())

    if (isTRUE(activation_result$changed)) {
      showNotification("Selected prediction model is now active.", type = "message")
    } else {
      showNotification("The selected prediction model was already active.", type = "message")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$prediction_delete_saved_model, {
    selected_entry <- selected_saved_model_entry()
    req(!is.null(selected_entry))

    delete_result <- tryCatch(
      delete_prediction_model_registry(selected_entry$path),
      error = function(e) e
    )

    if (inherits(delete_result, "error")) {
      showNotification(paste("Failed to delete the selected model:", delete_result$message), type = "error")
      return()
    }

    saved_model_status(load_prediction_model_registry())
    saved_model_choices_version(Sys.time())
    showNotification("Selected backup model deleted.", type = "message")
  }, ignoreInit = TRUE)

  filtered_training_summary <- reactive({
    breeding_lines <- input$prediction_breeding_lines
    if (is.null(breeding_lines) || length(breeding_lines) == 0) {
      breeding_lines <- "All"
    }
    summarize_prediction_training_data(
      prediction_dataset(),
      breeding_lines = breeding_lines,
      min_age_weeks = input$prediction_age_range[1],
      max_age_weeks = input$prediction_age_range[2],
      breeding_line_mode = input$prediction_breeding_line_mode,
      current_breeding_line = selected_prediction_training_line()
    )
  })

  observeEvent(input$prediction_retrain_models, {
    summary_info <- filtered_training_summary()
    training_rows <- summary_info$filtered_dataset
    selected_mode <- normalize_prediction_breeding_line_mode(input$prediction_breeding_line_mode)
    current_line <- selected_prediction_training_line()

    if (identical(selected_mode, "line_specific") && (is.na(current_line) || current_line == "")) {
      showNotification("Select exactly one breeding line before retraining a line-specific model.", type = "warning")
      return()
    }

    model_bundle <- build_pregnancy_ml_models(
      training_rows,
      current_breeding_line = current_line,
      breeding_line_mode = selected_mode
    )

    if (is.null(model_bundle$classifier)) {
      showNotification("Not enough filtered training data to build and save a classifier model.", type = "error")
      return()
    }

    registry <- create_prediction_model_registry(
      model_bundle,
      breeding_line_mode = selected_mode,
      current_breeding_line = current_line,
      training_summary = list(
        total_events = summary_info$total_events,
        pregnant_events = summary_info$pregnant_events,
        not_pregnant_events = summary_info$not_pregnant_events,
        unique_females = summary_info$unique_females
      )
    )

    save_result <- tryCatch(
      save_prediction_model_registry(registry),
      error = function(e) e
    )

    if (inherits(save_result, "error")) {
      showNotification(paste("Failed to save prediction models:", save_result$message), type = "error")
      return()
    }

    saved_model_status(load_prediction_model_registry())
    saved_model_choices_version(Sys.time())
    showNotification("Prediction model registry retrained and saved.", type = "message")
  }, ignoreInit = TRUE)

  training_table_rows <- reactive({
    training_rows <- filtered_training_summary()$filtered_dataset
    if (isTRUE(input$prediction_focus_pregnant)) {
      training_rows <- training_rows[training_rows$outcome_label == "pregnant", , drop = FALSE]
    }

    training_rows
  })

  observeEvent(input$prediction_training_table_state, {
    table_state <- input$prediction_training_table_state
    if (!is.null(table_state$page) && !is.na(table_state$page)) {
      prediction_training_table_page(as.integer(table_state$page))
    }
    if (!is.null(table_state$length) && !is.na(table_state$length)) {
      prediction_training_table_page_length(as.integer(table_state$length))
    }
  }, ignoreInit = TRUE)

  output$prediction_training_table <- DT::renderDataTable({
    training_rows <- training_table_rows()
    req(nrow(training_rows) > 0, cancelOutput = TRUE)

    # Track current filter state
    current_filter_state <- list(
      age_range = if (is.null(input$prediction_age_range)) NULL else list(input$prediction_age_range[1], input$prediction_age_range[2]),
      breeding_lines = if (is.null(input$prediction_breeding_lines)) NULL else sort(as.character(input$prediction_breeding_lines)),
      focus_pregnant = isTRUE(input$prediction_focus_pregnant),
      breeding_mode = if (is.null(input$prediction_breeding_line_mode)) "feature" else as.character(input$prediction_breeding_line_mode)
    )
    
    previous_state <- previous_training_filter_state()
    filter_changed <- !identical(current_filter_state, previous_state)

    previous_training_filter_state(current_filter_state)
    if (filter_changed) {
      prediction_training_table_page(0L)
    }

    session$sendCustomMessage(type = "eval", message = "if(typeof saveScrollForAllTables === 'function') saveScrollForAllTables();")

    display_rows <- training_rows[, c(
      "plugging_id", "female_id", "male_id", "plugging_status", "outcome_label",
      "pairing_start_date", "plug_observed_date",
      "final_report_primary_age", "final_report_total_embryos",
      "final_report_male_embryos", "final_report_female_embryos",
      "parse_confidence", "notes", "final_report_notes"
    ), drop = FALSE]

    current_page_length <- prediction_training_table_page_length()
    if (is.null(current_page_length) || is.na(current_page_length) || current_page_length <= 0) {
      current_page_length <- 25L
    }
    current_page <- prediction_training_table_page()
    if (is.null(current_page) || is.na(current_page) || current_page < 0) {
      current_page <- 0L
    }
    max_page <- max(0L, ceiling(nrow(display_rows) / current_page_length) - 1L)
    current_page <- min(as.integer(current_page), as.integer(max_page))
    display_start <- current_page * current_page_length

    on.exit({
      session$sendCustomMessage(type = "eval", message = "setTimeout(function() { if(typeof restoreScrollForAllTables === 'function') restoreScrollForAllTables(); }, 100);")
    }, add = TRUE)

    DT::datatable(
      display_rows,
      rownames = FALSE,
      selection = "none",
      escape = TRUE,
      options = list(
        pageLength = current_page_length,
        displayStart = display_start,
        scrollX = TRUE,
        order = list(list(0, "desc")),
        columnDefs = list(list(visible = FALSE, targets = 0))
      ),
      colnames = c(
        "Plugging ID", "Female", "Male", "Status", "Pregnant?",
        "Pairing Start", "Plug Observed",
        "Embryo Age", "Total Embryos",
        "Male", "Female",
        "Parse", "Notes", "Parsed Report Notes"
      ),
      callback = DT::JS(
        "  function syncPredictionTrainingState() {",
        "    var info = table.page.info();",
        "    if (!info) { return; }",
        "    Shiny.setInputValue('prediction_training_table_state', {",
        "      page: info.page,",
        "      length: info.length,",
        "      nonce: Date.now()",
        "    }, {priority: 'event'});",
        "  }",
        "  table.on('page.dt length.dt order.dt search.dt', syncPredictionTrainingState);",
        "  setTimeout(syncPredictionTrainingState, 0);",
        "table.on('dblclick', 'tbody tr', function() {",
        "  var data = table.row(this).data();",
        "  if (!data || data.length === 0) { return; }",
        "  Shiny.setInputValue('prediction_training_table_row_dblclicked', { id: data[0], status: data[3], outcome: data[4] }, {priority: 'event'});",
        "});"
      )
    )
  })

  observeEvent(input$prediction_training_table_row_dblclicked, {
    selected_row <- input$prediction_training_table_row_dblclicked
    selected_id <- suppressWarnings(as.integer(selected_row$id))
    req(!is.na(selected_id))

    status_text <- if (!is.null(selected_row$status) && !is.na(selected_row$status)) as.character(selected_row$status) else ""
    outcome_text <- if (!is.null(selected_row$outcome) && !is.na(selected_row$outcome)) as.character(selected_row$outcome) else ""
    is_not_pregnant <- grepl("not\\s*preg", tolower(status_text), perl = TRUE) || identical(tolower(trimws(outcome_text)), "not_pregnant")

    if (identical(selected_row$status, "Collected")) {
      prediction_state$open_collection_id <- NULL
      prediction_state$open_collection_id <- selected_id
    } else if (is_not_pregnant) {
      prediction_state$open_edit_id <- NULL
      prediction_state$open_edit_id <- selected_id
    } else {
      prediction_state$open_details_id <- NULL
      prediction_state$open_details_id <- selected_id
    }
  }, ignoreInit = TRUE)

  output$prediction_weight_plot <- renderPlotly({
    summary_info <- filtered_training_summary()
    training_rows <- summary_info$filtered_dataset
    plot_data <- build_training_model_gain_curve(training_rows, max_days = 21)
    req(nrow(plot_data) > 0, cancelOutput = TRUE)

    annotation_text <- paste0(
      "Filtered training data<br>",
      "Completed events: ", summary_info$total_events, "<br>",
      "Unique females: ", summary_info$unique_females, "<br>",
      "Pregnant labels: ", summary_info$pregnant_events, "<br>",
      "Not pregnant labels: ", summary_info$not_pregnant_events, "<br>",
      ">= 3 body weights: ", summary_info$with_weight_ge_3, "<br>",
      "Model mode: ", describe_prediction_breeding_line_mode(input$prediction_breeding_line_mode),
      if (isTRUE(input$prediction_focus_pregnant)) "<br>Table view: pregnant events only" else ""
    )

    p <- plot_ly()
    p <- add_lines(
      p,
      data = plot_data,
      x = ~pregnancy_age,
      y = ~weight_gain,
      name = "Historical Training Gain Curve",
      line = list(color = "#f59e0b", width = 3),
      hovertemplate = "Age E%{x}<br>Predicted Gain %{y:.2f} g<extra></extra>"
    )

    p %>% layout(
      title = "Predicted Body Weight Gain by Pregnancy Time",
      xaxis = list(title = "Pregnancy Time (days)", range = c(0, 21), zeroline = TRUE),
      yaxis = list(title = "Body Weight Gain (grams)"),
      annotations = list(list(
        x = 0.02,
        y = 0.98,
        xref = "paper",
        yref = "paper",
        xanchor = "left",
        yanchor = "top",
        text = annotation_text,
        showarrow = FALSE,
        align = "left",
        font = list(size = 13, color = "#334155"),
        bgcolor = "rgba(245, 158, 11, 0.12)",
        bordercolor = "rgba(180, 83, 9, 0.45)",
        borderwidth = 1,
        borderpad = 6
      )),
      legend = list(orientation = "h", y = -0.2),
      hovermode = "closest",
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
  })
}