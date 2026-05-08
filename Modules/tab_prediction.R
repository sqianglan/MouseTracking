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
          sliderInput("prediction_age_range", "Training Female Age (weeks)", min = 5, max = 60, value = c(7, 30), step = 1),
          checkboxInput("prediction_focus_pregnant", "Focus on pregnant events only", value = FALSE),
          div(
            style = "margin-top: 12px; color: #64748b; font-size: 0.95em;",
            "Filters limit which completed historical cases are used to build the pregnancy model curve shown on the right."
          ),
          hr(),
          uiOutput("prediction_training_summary_ui")
        )
      ),
      column(
        9,
        div(
          style = "background: white; border: 1px solid #e5e7eb; border-radius: 10px; padding: 18px;",
          tabsetPanel(
            id = "prediction_main_view",
            tabPanel(
              "Prediction Curve",
              div(style = "height: 520px; margin-top: 12px;", plotlyOutput("prediction_weight_plot", height = "520px"))
            ),
            tabPanel(
              "Training Records",
              div(
                style = "margin-top: 12px; color: #64748b;",
                "Double-click a row to inspect the source record. Collected rows open the collection report editor directly."
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

  prediction_state <- if (is.null(shared_plugging_state)) {
    reactiveValues(prediction_target_id = NULL, viewing_id = NULL, open_details_id = NULL, open_collection_id = NULL)
  } else {
    shared_plugging_state
  }

  prediction_dataset <- reactive({
    global_refresh_trigger()
    build_plugging_prediction_dataset(DB_PATH)
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

  filtered_training_summary <- reactive({
    breeding_lines <- input$prediction_breeding_lines
    if (is.null(breeding_lines) || length(breeding_lines) == 0) {
      breeding_lines <- "All"
    }
    summarize_prediction_training_data(
      prediction_dataset(),
      breeding_lines = breeding_lines,
      min_age_weeks = input$prediction_age_range[1],
      max_age_weeks = input$prediction_age_range[2]
    )
  })

  output$prediction_training_summary_ui <- renderUI({
    summary_info <- filtered_training_summary()
    div(
      h5("Training Data Summary", style = "color: #1e3a5f; margin-top: 0;"),
      div(strong("Completed events: "), summary_info$total_events),
      div(strong("Unique females: "), summary_info$unique_females),
      div(strong("Pregnant labels: "), summary_info$pregnant_events),
      div(strong("Not pregnant labels: "), summary_info$not_pregnant_events),
      div(strong(">= 3 body weights: "), summary_info$with_weight_ge_3),
      if (isTRUE(input$prediction_focus_pregnant)) {
        div(style = "margin-top: 8px; color: #92400e; font-size: 0.95em;", "Training Records table is focused on pregnant events only.")
      }
    )
  })

  training_table_rows <- reactive({
    training_rows <- filtered_training_summary()$filtered_dataset
    if (isTRUE(input$prediction_focus_pregnant)) {
      training_rows <- training_rows[training_rows$outcome_label == "pregnant", , drop = FALSE]
    }

    training_rows
  })

  output$prediction_training_table <- DT::renderDataTable({
    training_rows <- training_table_rows()
    req(nrow(training_rows) > 0, cancelOutput = TRUE)

    display_rows <- training_rows[, c(
      "plugging_id", "female_id", "male_id", "plugging_status", "outcome_label",
      "pairing_start_date", "plug_observed_date",
      "final_report_primary_age", "final_report_total_embryos",
      "final_report_male_embryos", "final_report_female_embryos",
      "parse_confidence", "notes", "final_report_notes"
    ), drop = FALSE]

    DT::datatable(
      display_rows,
      rownames = FALSE,
      selection = "none",
      escape = TRUE,
      options = list(
        pageLength = 25,
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
        "table.on('dblclick', 'tbody tr', function() {",
        "  var data = table.row(this).data();",
        "  if (!data || data.length === 0) { return; }",
        "  Shiny.setInputValue('prediction_training_table_row_dblclicked', { id: data[0], status: data[3] }, {priority: 'event'});",
        "});"
      )
    )
  })

  observeEvent(input$prediction_training_table_row_dblclicked, {
    selected_row <- input$prediction_training_table_row_dblclicked
    selected_id <- suppressWarnings(as.integer(selected_row$id))
    req(!is.na(selected_id))

    if (identical(selected_row$status, "Collected")) {
      prediction_state$open_collection_id <- NULL
      prediction_state$open_collection_id <- selected_id
    } else {
      prediction_state$open_details_id <- NULL
      prediction_state$open_details_id <- selected_id
    }
  }, ignoreInit = TRUE)

  output$prediction_weight_plot <- renderPlotly({
    training_rows <- filtered_training_summary()$filtered_dataset
    plot_data <- build_training_model_gain_curve(training_rows, max_days = 21)
    req(nrow(plot_data) > 0, cancelOutput = TRUE)

    p <- plot_ly()
    p <- add_lines(
      p,
      data = plot_data,
      x = ~pregnancy_age,
      y = ~weight_gain,
      name = "Filtered Pregnancy Gain Curve",
      line = list(color = "#f59e0b", width = 3),
      hovertemplate = "Age E%{x}<br>Predicted Gain %{y:.2f} g<extra></extra>"
    )

    p %>% layout(
      title = "Filtered Pregnancy Model: Body Weight Gain vs Pregnancy Age",
      xaxis = list(title = "Pregnancy Age (days)", range = c(0, 21), zeroline = TRUE),
      yaxis = list(title = "Body Weight Gain (grams)"),
      annotations = list(list(
        x = 0.02,
        y = 0.98,
        xref = "paper",
        yref = "paper",
        xanchor = "left",
        yanchor = "top",
        text = paste0(
          "Filtered pregnancy model<br>",
          filtered_training_summary()$pregnant_events,
          " pregnant records, ",
          filtered_training_summary()$not_pregnant_events,
          " not-pregnant records"
        ),
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