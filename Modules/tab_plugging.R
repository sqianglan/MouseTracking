# Optimized Plugging Tab Module
# This module provides comprehensive plugging event management with improved performance

suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(RSQLite)
  library(DT)
  library(jsonlite)
})

# Source the new modal module
source("Modules/modal_add_plugging_event.R")
source("Modules/modal_body_weight.R")
source("Modules/pregnancy_prediction_analysis.R")

# Constants
PLUGGING_STATUSES <- c("Ongoing", "Plugged", "Plug Confirmed", "Not Pregnant", "Not Observed (Waiting for confirmation)", "Empty", "Not Observed (Confirmed)", "Surprising Plug!!", "Collected")

# UI Function
plugging_tab_ui <- function() {
  fluidPage(
    div(
      style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 16px;",
      h3("♂️🐭 x 🐭♀️ Plugging Management", style = "margin: 0; font-size: 1.8em; color: #2c3e50; font-weight: 700;"),
      div(
        style = "display: flex; align-items: center; gap: 8px;",
        div(
          style = "font-size: 12px; color: #6c757d; background: #e9ecef; padding: 4px 8px; border-radius: 4px;",
          "💡 Manage breeding pairs and track plugging events"
        )
      )
    ),
    div(
      style = "background: white; border-radius: 8px; padding: 16px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); margin-bottom: 16px; position: sticky; top: 8px; z-index: 95;",
      div(
        class = "action-buttons",
        style = "margin-bottom: 16px;",
        add_plugging_modal_ui("add_plugging_modal"),
        actionButton("show_calendar_btn", "📅 Event Calendar", 
                    class = "btn-info", style = "margin-left: 8px; padding: 8px 16px; font-size: 14px; border-radius: 6px;")
      ),
      div(
        style = "padding: 12px; background: linear-gradient(135deg, #e8f5e8 0%, #d4edda 100%); border-radius: 6px; border-left: 4px solid #28a745;",
        "📊 Record plugging events for breeding groups with one male and up to three females, and track their progress through different stages."
      )
    ),
    div(
      style = "background: white; border-radius: 8px; padding: 16px; box-shadow: 0 2px 4px rgba(0,0,0,0.05);",
      h4("📋 Plugging History", style = "margin: 8px 0 16px 0; font-size: 1.3em; color: #2c3e50;"),
      uiOutput("plugging_history_controls"),
      div(
        style = "position: relative;",
        uiOutput("plugging_loading_overlay"),
        DT::dataTableOutput("plugging_history_table")
      )
    )
  )
}

# Server Function
plugging_tab_server <- function(input, output, session, is_system_locked = NULL, global_refresh_trigger = NULL, all_mice_table = NULL, shared_plugging_state = NULL) {
  
  # Default lock function if not provided
  if (is.null(is_system_locked)) {
    is_system_locked <- function() FALSE
  }
  
  # Default refresh trigger if not provided
  if (is.null(global_refresh_trigger)) {
    global_refresh_trigger <- reactiveVal(Sys.time())
  }
  
  # Default all_mice_table if not provided
  if (is.null(all_mice_table)) {
    all_mice_table <- reactiveVal(NULL)
  }
  
  # Use shared plugging state if provided, otherwise create a new one
  if (is.null(shared_plugging_state)) {
    plugging_state <- reactiveValues(
      reload = NULL,
      viewing_id = NULL,
      viewing_refresh = 0,
      editing_id = NULL,
      confirming_id = NULL,
      prediction_breeding_line_mode = "feature"
    )
  } else {
    plugging_state <- shared_plugging_state
  }

  if (is.null(isolate(plugging_state$viewing_refresh))) {
    plugging_state$viewing_refresh <- 0
  }

  details_prediction_training_dataset <- reactive({
    global_refresh_trigger()
    tryCatch(build_plugging_prediction_dataset(DB_PATH), error = function(e) data.frame())
  })
  
  # Add a reactiveVal to store the pending delete plugging_id
  pending_delete_id <- reactiveVal(NULL)
  
  # Database connection helper
  db_connect <- function() {
    DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  }
  
  db_disconnect <- function(con) {
    if (!is.null(con)) {
      tryCatch(DBI::dbDisconnect(con), error = function(e) NULL)
    }
  }

  safe_parse_plugging_date <- function(date_string) {
    if (is.null(date_string) || is.na(date_string) || date_string == "" || date_string == "Unknown") {
      return(as.Date(NA))
    }

    date_formats <- c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%Y/%m/%d", "%m-%d-%Y", "%d-%m-%Y")

    for (date_format in date_formats) {
      parsed_date <- tryCatch(as.Date(date_string, format = date_format), error = function(e) as.Date(NA))
      if (!is.na(parsed_date)) {
        return(unname(parsed_date))
      }
    }

    as.Date(NA)
  }

  calculate_plugging_base_date <- function(status, plug_observed_date, pairing_start_date) {
    if (status %in% c("Plugged", "Plug Confirmed")) {
      parsed_plug_date <- safe_parse_plugging_date(plug_observed_date)
      if (!is.na(parsed_plug_date)) {
        return(parsed_plug_date)
      }

      parsed_pairing_start_date <- safe_parse_plugging_date(pairing_start_date)
      if (!is.na(parsed_pairing_start_date)) {
        return(parsed_pairing_start_date)
      }
    }

    if (status %in% c("Not Observed (Waiting for confirmation)", "Surprising Plug!!", "Ongoing")) {
      parsed_pairing_start_date <- safe_parse_plugging_date(pairing_start_date)
      if (!is.na(parsed_pairing_start_date)) {
        return(parsed_pairing_start_date + 1)
      }
    }

    as.Date(NA)
  }

  format_day_month <- function(date_value) {
    if (is.na(date_value)) {
      return(NULL)
    }

    paste0(as.integer(format(date_value, "%d")), ".", as.integer(format(date_value, "%m")))
  }

  format_embryonic_age_today <- function(base_date) {
    if (is.na(base_date)) {
      return(NULL)
    }

    embryonic_age <- as.numeric(Sys.Date() - base_date) + 0.5
    paste0("E", floor(embryonic_age) + 0.5, " today")
  }

  build_plugging_mouse_label <- function(mouse_id, genotype, breeding_line, sex_label) {
    descriptor <- NA_character_

    if (!is.null(genotype) && !is.na(genotype) && trimws(genotype) != "") {
      descriptor <- trimws(genotype)
    } else if (!is.null(breeding_line) && !is.na(breeding_line) && trimws(breeding_line) != "") {
      descriptor <- trimws(breeding_line)
    }

    if (is.na(descriptor)) {
      paste0("#", mouse_id, " (", sex_label, ")")
    } else {
      paste0("#", mouse_id, " (", descriptor, ", ", sex_label, ")")
    }
  }

  normalize_plugging_notes_input <- function(note_text) {
    if (is.null(note_text) || length(note_text) == 0 || is.na(note_text[1])) {
      return("")
    }

    strip_plugging_status_audit_notes(note_text[1])
  }

  write_collection_debug_log <- function(stage, details = list()) {
    invisible(NULL)
  }

  embryo_count_autofill_in_progress <- reactiveVal(FALSE)
  quick_embryo_autofill_enabled <- reactiveVal(TRUE)
  edit_embryo_autofill_enabled <- reactiveVal(TRUE)
  euthanasia_embryo_autofill_enabled <- reactiveVal(TRUE)
  quick_other_age_row_count <- reactiveVal(1)
  edit_other_age_row_count <- reactiveVal(1)
  quick_other_age_rows_seed <- reactiveVal(data.frame(stage = "", count = NA_integer_, stringsAsFactors = FALSE))
  edit_other_age_rows_seed <- reactiveVal(data.frame(stage = "", count = NA_integer_, stringsAsFactors = FALSE))
  euthanasia_other_age_row_count <- reactiveVal(1)
  euthanasia_other_age_rows_seed <- reactiveVal(data.frame(stage = "", count = NA_integer_, stringsAsFactors = FALSE))

  parse_other_age_rows_from_json <- function(age_groups_json, primary_age_text = "") {
    default_rows <- data.frame(stage = "", count = NA_integer_, stringsAsFactors = FALSE)
    if (is.null(age_groups_json) || is.na(age_groups_json) || trimws(as.character(age_groups_json)) == "") {
      return(default_rows)
    }

    age_groups <- tryCatch(jsonlite::fromJSON(age_groups_json), error = function(e) NULL)
    if (is.null(age_groups) || NROW(age_groups) == 0) {
      return(default_rows)
    }

    age_df <- as.data.frame(age_groups, stringsAsFactors = FALSE)
    if (!("age_label" %in% names(age_df))) {
      return(default_rows)
    }

    primary_label <- normalize_embryo_age_input(primary_age_text)$label
    parsed_rows <- lapply(seq_len(nrow(age_df)), function(idx) {
      stage_value <- trimws(as.character(age_df$age_label[idx]))
      if (stage_value == "") {
        return(NULL)
      }

      if (!is.na(primary_label) && identical(toupper(stage_value), toupper(primary_label))) {
        return(NULL)
      }

      count_value <- NA_integer_
      if ("count" %in% names(age_df)) {
        count_value <- suppressWarnings(as.integer(age_df$count[idx]))
      }

      data.frame(stage = stage_value, count = count_value, stringsAsFactors = FALSE)
    })

    parsed_rows <- Filter(Negate(is.null), parsed_rows)
    if (length(parsed_rows) == 0) {
      return(default_rows)
    }

    do.call(rbind, parsed_rows)
  }

  collect_other_age_rows <- function(prefix, row_count, seed_rows) {
    count <- max(1, suppressWarnings(as.integer(row_count)))
    rows <- vector("list", count)

    for (idx in seq_len(count)) {
      stage_id <- paste0(prefix, "_other_age_stage_", idx)
      count_id <- paste0(prefix, "_other_age_count_", idx)

      stage_value <- ""
      count_value <- NA_integer_

      if (!is.null(input[[stage_id]])) {
        stage_value <- trimws(as.character(input[[stage_id]]))
      } else if (idx <= nrow(seed_rows)) {
        stage_value <- trimws(as.character(seed_rows$stage[idx]))
      }

      if (!is.null(input[[count_id]])) {
        count_value <- suppressWarnings(as.integer(input[[count_id]]))
      } else if (idx <= nrow(seed_rows)) {
        count_value <- suppressWarnings(as.integer(seed_rows$count[idx]))
      }

      rows[[idx]] <- data.frame(stage = stage_value, count = count_value, stringsAsFactors = FALSE)
    }

    do.call(rbind, rows)
  }

  build_age_groups_text_from_rows <- function(rows_df) {
    if (is.null(rows_df) || nrow(rows_df) == 0) {
      return("")
    }

    valid_rows <- rows_df[
      trimws(as.character(rows_df$stage)) != "" & !is.na(suppressWarnings(as.integer(rows_df$count))),
      , drop = FALSE
    ]

    if (nrow(valid_rows) == 0) {
      return("")
    }

    valid_rows$count <- suppressWarnings(as.integer(valid_rows$count))
    valid_rows <- valid_rows[!is.na(valid_rows$count) & valid_rows$count >= 0, , drop = FALSE]
    if (nrow(valid_rows) == 0) {
      return("")
    }

    paste(apply(valid_rows, 1, function(row_value) {
      paste0(trimws(as.character(row_value[["stage"]])), " x", as.integer(row_value[["count"]]))
    }), collapse = "; ")
  }

  format_age_groups_text_from_json <- function(age_groups_json) {
    if (is.null(age_groups_json) || length(age_groups_json) == 0 || is.na(age_groups_json) || trimws(as.character(age_groups_json)[1]) == "") {
      return("")
    }

    mixed_age_groups <- tryCatch(jsonlite::fromJSON(age_groups_json), error = function(e) NULL)
    if (is.null(mixed_age_groups) || NROW(mixed_age_groups) == 0) {
      return("")
    }

    mixed_age_df <- as.data.frame(mixed_age_groups, stringsAsFactors = FALSE)
    if (!("age_label" %in% names(mixed_age_df)) || nrow(mixed_age_df) == 0) {
      return("")
    }

    paste(vapply(seq_len(nrow(mixed_age_df)), function(idx) {
      age_label <- trimws(as.character(mixed_age_df$age_label[idx]))
      if (age_label == "") {
        return("")
      }

      count_value <- if ("count" %in% names(mixed_age_df)) mixed_age_df$count[idx] else NA
      if (!is.na(count_value) && as.character(count_value) != "") {
        paste0(age_label, " x", count_value)
      } else {
        age_label
      }
    }, character(1)), collapse = "; ")
  }

  render_other_age_rows_ui <- function(prefix, row_count, seed_rows) {
    count <- max(1, suppressWarnings(as.integer(row_count)))
    rows <- seed_rows
    if (nrow(rows) < count) {
      rows <- rbind(rows, data.frame(stage = rep("", count - nrow(rows)), count = rep(NA_integer_, count - nrow(rows)), stringsAsFactors = FALSE))
    }

    tagList(lapply(seq_len(count), function(idx) {
      is_last <- idx == count
      div(
        style = "display: flex; align-items: center; gap: 6px; margin-bottom: 2px;",
        div(style = "flex: 3; min-width: 0;",
          textInput(
            paste0(prefix, "_other_age_stage_", idx),
            if (idx == 1) "Stage" else NULL,
            value = as.character(rows$stage[idx]),
            placeholder = "E15.5"
          )
        ),
        div(style = "flex: 1.5; min-width: 0;",
          numericInput(
            paste0(prefix, "_other_age_count_", idx),
            if (idx == 1) "Number" else NULL,
            value = if (is.na(rows$count[idx])) NA else suppressWarnings(as.integer(rows$count[idx])),
            min = 0,
            step = 1
          )
        ),
        div(style = "flex: 0 0 40px; display: flex; justify-content: center;",
          if (is_last) {
            actionButton(paste0(prefix, "_add_other_age_row_btn"), "+", class = "btn btn-default btn-sm", style = "min-width: 34px; padding: 2px 8px;")
          } else {
            tags$span(style = "display: inline-block; width: 34px;")
          }
        ),
        div(style = "flex: 0 0 40px; display: flex; justify-content: center;",
          if (is_last) {
            actionButton(paste0(prefix, "_remove_other_age_row_btn"), "-", class = "btn btn-default btn-sm", style = "min-width: 34px; padding: 2px 8px;")
          } else {
            tags$span(style = "display: inline-block; width: 34px;")
          }
        )
      )
    }))
  }

  initialize_quick_other_age_rows <- function(report_defaults) {
    seed_rows <- parse_other_age_rows_from_json(
      report_defaults$final_report_age_groups_json,
      report_defaults$final_report_primary_age
    )
    quick_other_age_rows_seed(seed_rows)
    quick_other_age_row_count(max(1, nrow(seed_rows)))
  }

  initialize_edit_other_age_rows <- function(report_defaults) {
    seed_rows <- parse_other_age_rows_from_json(
      report_defaults$final_report_age_groups_json,
      report_defaults$final_report_primary_age
    )
    edit_other_age_rows_seed(seed_rows)
    edit_other_age_row_count(max(1, nrow(seed_rows)))
  }

  initialize_euthanasia_other_age_rows <- function(report_defaults) {
    seed_rows <- parse_other_age_rows_from_json(
      report_defaults$final_report_age_groups_json,
      report_defaults$final_report_primary_age
    )
    euthanasia_other_age_rows_seed(seed_rows)
    euthanasia_other_age_row_count(max(1, nrow(seed_rows)))
  }

  apply_embryo_count_autofill <- function(input_values, input_ids) {
    normalized_counts <- normalize_final_report_embryo_counts(
      total_embryos = input_values$total,
      male_embryos = input_values$male,
      female_embryos = input_values$female,
      unknown_embryos = input_values$unknown
    )

    if (!is.null(normalized_counts$validation_message) || length(normalized_counts$autofilled_fields) == 0) {
      return(FALSE)
    }

    embryo_count_autofill_in_progress(TRUE)
    on.exit(embryo_count_autofill_in_progress(FALSE), add = TRUE)

    updated_any <- FALSE

    if ("total" %in% normalized_counts$autofilled_fields && !identical(input_values$total, normalized_counts$final_report_total_embryos)) {
      updateNumericInput(session, input_ids$total, value = normalized_counts$final_report_total_embryos)
      updated_any <- TRUE
    }
    if ("male" %in% normalized_counts$autofilled_fields && !identical(input_values$male, normalized_counts$final_report_male_embryos)) {
      updateNumericInput(session, input_ids$male, value = normalized_counts$final_report_male_embryos)
      updated_any <- TRUE
    }
    if ("female" %in% normalized_counts$autofilled_fields && !identical(input_values$female, normalized_counts$final_report_female_embryos)) {
      updateNumericInput(session, input_ids$female, value = normalized_counts$final_report_female_embryos)
      updated_any <- TRUE
    }
    if ("unknown" %in% normalized_counts$autofilled_fields && !identical(input_values$unknown, normalized_counts$final_report_unknown_embryos)) {
      updateNumericInput(session, input_ids$unknown, value = normalized_counts$final_report_unknown_embryos)
      updated_any <- TRUE
    }

    updated_any
  }

  observeEvent(plugging_state$confirming_id, {
    quick_embryo_autofill_enabled(TRUE)
  }, ignoreInit = TRUE)

  observeEvent(
    list(
      input$quick_total_embryos_input,
      input$quick_male_embryos_input,
      input$quick_female_embryos_input,
      input$quick_unknown_embryos_input
    ),
    {
      if (isTRUE(embryo_count_autofill_in_progress()) || is.null(plugging_state$confirming_id) || !isTRUE(quick_embryo_autofill_enabled())) {
        return()
      }

      input_values <- list(
        total = input$quick_total_embryos_input,
        male = input$quick_male_embryos_input,
        female = input$quick_female_embryos_input,
        unknown = input$quick_unknown_embryos_input
      )

      if (all(vapply(input_values, is.null, logical(1)))) {
        return()
      }

      did_autofill <- apply_embryo_count_autofill(
        input_values,
        list(
          total = "quick_total_embryos_input",
          male = "quick_male_embryos_input",
          female = "quick_female_embryos_input",
          unknown = "quick_unknown_embryos_input"
        )
      )

      if (isTRUE(did_autofill)) {
        quick_embryo_autofill_enabled(FALSE)
      }
    },
    ignoreInit = TRUE
  )

  observeEvent(
    list(
      input$edit_final_report_total_embryos,
      input$edit_final_report_male_embryos,
      input$edit_final_report_female_embryos,
      input$edit_final_report_unknown_embryos
    ),
    {
      if (isTRUE(embryo_count_autofill_in_progress()) || !isTRUE(edit_embryo_autofill_enabled())) {
        return()
      }

      input_values <- list(
        total = input$edit_final_report_total_embryos,
        male = input$edit_final_report_male_embryos,
        female = input$edit_final_report_female_embryos,
        unknown = input$edit_final_report_unknown_embryos
      )

      if (all(vapply(input_values, is.null, logical(1)))) {
        return()
      }

      did_autofill <- apply_embryo_count_autofill(
        input_values,
        list(
          total = "edit_final_report_total_embryos",
          male = "edit_final_report_male_embryos",
          female = "edit_final_report_female_embryos",
          unknown = "edit_final_report_unknown_embryos"
        )
      )

      if (isTRUE(did_autofill)) {
        edit_embryo_autofill_enabled(FALSE)
      }
    },
    ignoreInit = TRUE
  )

  observeEvent(
    list(
      input$euthanasia_total_embryos_input,
      input$euthanasia_male_embryos_input,
      input$euthanasia_female_embryos_input,
      input$euthanasia_unknown_embryos_input,
      input$euthanasia_status_choice
    ),
    {
      if (isTRUE(embryo_count_autofill_in_progress()) || !identical(input$euthanasia_status_choice, "Collected") || !isTRUE(euthanasia_embryo_autofill_enabled())) {
        return()
      }

      input_values <- list(
        total = input$euthanasia_total_embryos_input,
        male = input$euthanasia_male_embryos_input,
        female = input$euthanasia_female_embryos_input,
        unknown = input$euthanasia_unknown_embryos_input
      )

      if (all(vapply(input_values, is.null, logical(1)))) {
        return()
      }

      did_autofill <- apply_embryo_count_autofill(
        input_values,
        list(
          total = "euthanasia_total_embryos_input",
          male = "euthanasia_male_embryos_input",
          female = "euthanasia_female_embryos_input",
          unknown = "euthanasia_unknown_embryos_input"
        )
      )

      if (isTRUE(did_autofill)) {
        euthanasia_embryo_autofill_enabled(FALSE)
      }
    },
    ignoreInit = TRUE
  )

  output$quick_other_age_rows_ui <- renderUI({
    render_other_age_rows_ui("quick", quick_other_age_row_count(), quick_other_age_rows_seed())
  })

  output$edit_other_age_rows_ui <- renderUI({
    render_other_age_rows_ui("edit", edit_other_age_row_count(), edit_other_age_rows_seed())
  })

  output$euthanasia_other_age_rows_ui <- renderUI({
    render_other_age_rows_ui("euthanasia", euthanasia_other_age_row_count(), euthanasia_other_age_rows_seed())
  })

  observeEvent(input$quick_add_other_age_row_btn, {
    current_rows <- collect_other_age_rows("quick", quick_other_age_row_count(), quick_other_age_rows_seed())
    quick_other_age_rows_seed(rbind(current_rows, data.frame(stage = "", count = NA_integer_, stringsAsFactors = FALSE)))
    quick_other_age_row_count(quick_other_age_row_count() + 1)
  }, ignoreInit = TRUE)

  observeEvent(input$quick_remove_other_age_row_btn, {
    current_count <- quick_other_age_row_count()
    if (current_count <= 1) {
      return()
    }

    current_rows <- collect_other_age_rows("quick", current_count, quick_other_age_rows_seed())
    quick_other_age_rows_seed(current_rows[seq_len(current_count - 1), , drop = FALSE])
    quick_other_age_row_count(current_count - 1)
  }, ignoreInit = TRUE)

  observeEvent(input$edit_add_other_age_row_btn, {
    current_rows <- collect_other_age_rows("edit", edit_other_age_row_count(), edit_other_age_rows_seed())
    edit_other_age_rows_seed(rbind(current_rows, data.frame(stage = "", count = NA_integer_, stringsAsFactors = FALSE)))
    edit_other_age_row_count(edit_other_age_row_count() + 1)
  }, ignoreInit = TRUE)

  observeEvent(input$edit_remove_other_age_row_btn, {
    current_count <- edit_other_age_row_count()
    if (current_count <= 1) {
      return()
    }

    current_rows <- collect_other_age_rows("edit", current_count, edit_other_age_rows_seed())
    edit_other_age_rows_seed(current_rows[seq_len(current_count - 1), , drop = FALSE])
    edit_other_age_row_count(current_count - 1)
  }, ignoreInit = TRUE)

  observeEvent(input$euthanasia_add_other_age_row_btn, {
    current_rows <- collect_other_age_rows("euthanasia", euthanasia_other_age_row_count(), euthanasia_other_age_rows_seed())
    euthanasia_other_age_rows_seed(rbind(current_rows, data.frame(stage = "", count = NA_integer_, stringsAsFactors = FALSE)))
    euthanasia_other_age_row_count(euthanasia_other_age_row_count() + 1)
  }, ignoreInit = TRUE)

  observeEvent(input$euthanasia_remove_other_age_row_btn, {
    current_count <- euthanasia_other_age_row_count()
    if (current_count <= 1) {
      return()
    }
    current_rows <- collect_other_age_rows("euthanasia", current_count, euthanasia_other_age_rows_seed())
    euthanasia_other_age_rows_seed(current_rows[seq_len(current_count - 1), , drop = FALSE])
    euthanasia_other_age_row_count(current_count - 1)
  }, ignoreInit = TRUE)

  observe({
    req(euthanasia_other_age_row_count() >= 1)
    rows_df <- collect_other_age_rows("euthanasia", euthanasia_other_age_row_count(), euthanasia_other_age_rows_seed())
    age_groups_text <- build_age_groups_text_from_rows(rows_df)
    updateTextAreaInput(session, "euthanasia_embryo_age_groups_input", value = age_groups_text)
    if (age_groups_text != "" && !isTRUE(input$euthanasia_mixed_embryo_ages_input)) {
      updateCheckboxInput(session, "euthanasia_mixed_embryo_ages_input", value = TRUE)
    }
  })

  observe({
    req(quick_other_age_row_count() >= 1)
    rows_df <- collect_other_age_rows("quick", quick_other_age_row_count(), quick_other_age_rows_seed())
    age_groups_text <- build_age_groups_text_from_rows(rows_df)
    updateTextAreaInput(session, "quick_embryo_age_groups_input", value = age_groups_text)

    if (age_groups_text != "" && !isTRUE(input$quick_mixed_embryo_ages_input)) {
      updateCheckboxInput(session, "quick_mixed_embryo_ages_input", value = TRUE)
    }
  })

  observe({
    req(edit_other_age_row_count() >= 1)
    rows_df <- collect_other_age_rows("edit", edit_other_age_row_count(), edit_other_age_rows_seed())
    age_groups_text <- build_age_groups_text_from_rows(rows_df)
    updateTextAreaInput(session, "edit_final_report_age_groups", value = age_groups_text)

    if (age_groups_text != "" && !isTRUE(input$edit_final_report_mixed_age)) {
      updateCheckboxInput(session, "edit_final_report_mixed_age", value = TRUE)
    }
  })

  build_plugging_details_clipboard_text <- function(row) {
    female_label <- build_plugging_mouse_label(row$female_id, row$female_genotype, row$female_breeding_line, "female")
    male_label <- build_plugging_mouse_label(row$male_id, row$male_genotype, row$male_breeding_line, "male")

    base_text <- paste(female_label, "plugged with", male_label)
    suffix_parts <- character(0)

    if (identical(row$plugging_status, "Surprising Plug!!")) {
      suffix_parts <- c(suffix_parts, "surprising plug")
    }

    observed_date <- safe_parse_plugging_date(row$plug_observed_date)
    observed_text <- format_day_month(observed_date)
    if (!is.null(observed_text)) {
      suffix_parts <- c(suffix_parts, paste0("@", observed_text))
    }

    current_age_text <- format_embryonic_age_today(
      calculate_plugging_base_date(row$plugging_status, row$plug_observed_date, row$pairing_start_date)
    )
    if (!is.null(current_age_text)) {
      suffix_parts <- c(suffix_parts, current_age_text)
    }

    if (length(suffix_parts) == 0) {
      return(base_text)
    }

    if (suffix_parts[1] == "surprising plug") {
      if (length(suffix_parts) == 1) {
        return(paste(base_text, suffix_parts[1]))
      }

      return(paste0(base_text, " ", suffix_parts[1], ", ", paste(suffix_parts[-1], collapse = ", ")))
    }

    paste0(base_text, " ", paste(suffix_parts, collapse = ", "))
  }

  build_ongoing_plugging_summary <- function() {
    con <- db_connect()

    tryCatch({
      ongoing_records <- DBI::dbGetQuery(
        con,
        "SELECT male_id, female_id, pairing_start_date
         FROM plugging_history
         WHERE plugging_status = 'Ongoing'
         ORDER BY pairing_start_date ASC, male_id ASC, female_id ASC"
      )

      if (nrow(ongoing_records) == 0) {
        return("No ongoing plugging records.")
      }

      ongoing_records$pairing_start_date[is.na(ongoing_records$pairing_start_date) | ongoing_records$pairing_start_date == ""] <- "9999-12-31"
      grouped_records <- split(ongoing_records, ongoing_records$male_id)

      group_sort_dates <- vapply(grouped_records, function(group_df) {
        min(group_df$pairing_start_date, na.rm = TRUE)
      }, character(1))

      grouped_records <- grouped_records[order(group_sort_dates, names(grouped_records))]

      formatted_lines <- vapply(grouped_records, function(group_df) {
        female_ids <- unique(group_df$female_id)
        female_text <- paste(female_ids, collapse = " and ")
        paste0(group_df$male_id[1], " with females ", female_text, ";")
      }, character(1))

      paste(formatted_lines, collapse = "\n\n")
    }, error = function(e) {
      paste("Unable to load ongoing plugging summary:", e$message)
    }, finally = {
      db_disconnect(con)
    })
  }
  
  # Unified modal function for Empty (Alive) status
  show_empty_alive_modal <- function(female_info, female_age, default_date, default_notes, confirm_id, confirm_btn_id) {
    modalDialog(
      title = "Confirm Set Status to Empty (Alive)",
      size = "m",
      tagList(
        fluidRow(
          column(6, 
            wellPanel(
              tags$h4("Female Mouse Information"),
              tags$b("ASU ID:"), if(!is.null(female_info) && nrow(female_info) > 0) female_info$asu_id[1] else "N/A", br(),
              tags$b("Age (weeks):"), if(!is.na(female_age)) female_age else "N/A", br(),
              tags$b("Breeding Line:"), if(!is.null(female_info) && nrow(female_info) > 0) female_info$breeding_line[1] else "N/A", br(),
              tags$b("Genotype:"), if(!is.null(female_info) && nrow(female_info) > 0) female_info$genotype[1] else "N/A"
            )
          ),
          column(6,
            dateInput(confirm_id, "Date of Confirmation", value = default_date),
            textAreaInput(paste0(confirm_id, "_notes"), "Notes", value = default_notes, rows = 2)
          )
        ),
        div(
          style = "text-align: center; margin-top: 15px;",
          tags$p(tags$i("This will set the plugging status to 'Empty' but keep the female mouse alive."))
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(confirm_btn_id, "Confirm", class = "btn-success")
      )
    )
  }
  
  # Use the enhanced audit trail system from audit_trail.R
  
  # Get live mice for selection - using centralized function directly
  get_live_mice <- reactive({
    # Get all live mice directly using centralized function
    all_mice <- get_live_mice_by_gender(gender = NULL, global_refresh_trigger = global_refresh_trigger)()
    
    # Return in the expected format for the modal
    list(
      males = all_mice[all_mice$gender == "Male", ],
      females = all_mice[all_mice$gender == "Female", ]
    )
  })
  
  # Plugging history table
  output$plugging_history_controls <- renderUI({
    div(
      style = "display: flex; align-items: center; gap: 16px;",
      div(
        style = "display: flex; flex-direction: column; margin-right: 16px;",
        checkboxInput("show_finished_plugging_history", "Show Archived Records", value = FALSE),
        div(
          style = "font-size: 11px; color: #666; margin-left: 20px; margin-top: -12px;",
          "(Unsuccessful Plugs, Empty Plugs and Euthanized Mice)"
        )
      ),
      div(
        style = "display: flex; flex-direction: column;",
        checkboxInput("show_deleted_plugging_history", "Show Deleted Records", value = FALSE),
        div(
          style = "font-size: 11px; color: #666; margin-left: 20px; margin-top: -12px;",
          "(Entries by mistake)"
        )
      ),
      div(
        style = "margin-left: 8px;",
        actionButton("show_ongoing_plugging_summary_btn", "📝 Ongoing Summary",
                    class = "btn-secondary",
                    style = "padding: 6px 12px; font-size: 12px; border-radius: 4px;")
      ),
      div(
        style = "margin-left: auto;",
        actionButton("refresh_plugging_table_btn", "🔄 Refresh Table", 
                    class = "btn-secondary", 
                    style = "padding: 6px 12px; font-size: 12px; border-radius: 4px;")
      )
    )
  })
  
  output$plugging_history_table <- DT::renderDataTable({
    plugging_state$reload
    
    # Save scroll and table state before refresh
    session$sendCustomMessage(type = "eval", message = "if(typeof saveDataTableState === 'function') saveDataTableState('plugging_history_table');")
    session$sendCustomMessage(type = "eval", message = "if(typeof saveScrollForAllTables === 'function') saveScrollForAllTables();")
    
    con <- db_connect()
    tryCatch({
      # Get plugging data with mouse info in a single query
      pluggings <- DBI::dbGetQuery(con, 
        "SELECT ph.*, 
                m.dob as male_dob, m.breeding_line as male_breeding_line, m.genotype as male_genotype, m.status as male_status,
                f.dob as female_dob, f.breeding_line as female_breeding_line, f.genotype as female_genotype, f.status as female_status
         FROM plugging_history ph
         LEFT JOIN mice_stock m ON ph.male_id = m.asu_id
         LEFT JOIN mice_stock f ON ph.female_id = f.asu_id
         ORDER BY ph.id DESC")
      
      # Filtering logic
      show_deleted <- isTRUE(input$show_deleted_plugging_history)
      show_finished <- isTRUE(input$show_finished_plugging_history)
      

      ### Default values 
      filtered <- pluggings[
          pluggings$plugging_status %in% c("Ongoing", "Plugged", "Not Observed (Waiting for confirmation)", "Plug Confirmed", "Surprising Plug!!") &
          (is.na(pluggings$female_status) | pluggings$female_status == "Alive"),
        ]
      
      ###add empty plugs or euthanized mice if show_finished is true
      ### potential problem if the mice has revived for mistake, the record will take both record. but reality is if the mice has been deceased, it will not shown anymore.
      euthanized_ids <- c()
      if (show_finished) {
        audit_con <- db_connect()
        euthanized_rows <- DBI::dbGetQuery(audit_con, "SELECT record_id, new_values FROM audit_trail WHERE table_name = 'mice_stock' AND action = 'UPDATE'")
        db_disconnect(audit_con)
        if (nrow(euthanized_rows) > 0) {
        
          euthanized_ids <- sapply(seq_len(nrow(euthanized_rows)), function(i) {
            new_vals <- tryCatch(jsonlite::fromJSON(euthanized_rows$new_values[i]), error = function(e) list())
            if (!is.null(new_vals$source) && new_vals$source == 'Plugging Tab' && new_vals$status == 'Deceased') {
              female_id <- euthanized_rows$record_id[i]
              which(pluggings$female_id == female_id)
            } else {
              integer(0)
            }
          })
          euthanized_ids <- unique(unlist(euthanized_ids))
        }
        filtered <- rbind(filtered, pluggings[pluggings$plugging_status == 'Empty' | pluggings$plugging_status == 'Not Observed (Confirmed)' | pluggings$plugging_status == 'Not Pregnant' | pluggings$plugging_status == 'Collected' | seq_len(nrow(pluggings)) %in% euthanized_ids, ])
      
      }
      
      ###add deleted plugs if show_deleted is true
      if (show_deleted) {
        deleted_plugs <- pluggings[pluggings$plugging_status == 'Deleted', ]
        filtered <- rbind(filtered, deleted_plugs)
      } 
      
     
      # Calculate ages
      filtered$male_age <- floor(as.numeric(Sys.Date() - as.Date(filtered$male_dob)) / 7)
      filtered$female_age <- floor(as.numeric(Sys.Date() - as.Date(filtered$female_dob)) / 7)
      
      # Handle NULL dates
      filtered$pairing_start_date[is.na(filtered$pairing_start_date) | filtered$pairing_start_date == ""] <- "Unknown"
      filtered$pairing_end_date[is.na(filtered$pairing_end_date) | filtered$pairing_end_date == ""] <- "Unknown"
      filtered$plug_observed_date[is.na(filtered$plug_observed_date) | filtered$plug_observed_date == ""] <- "Unknown"
      
      # All missing dates are now consistently shown as "Unknown"
      
      # Create display table - show only females (one animal per row)
      display_data <- filtered[, c("id", "female_id", "female_age", "female_breeding_line", "female_genotype", 
                                   "pairing_start_date", "pairing_end_date", "plug_observed_date", 
                                   "plugging_status", "expected_age_for_harvesting", "notes")]
      display_data$notes <- vapply(display_data$notes, strip_plugging_status_audit_notes, character(1))
      
      # Add action buttons column
      display_data$actions <- sapply(seq_len(nrow(display_data)), function(i) {
        row <- display_data[i, ]
        btns <- c()
        
        # Add Update button for all active status mice
        if (is_active_status(row$plugging_status)) {
          btns <- c(btns, paste0('<button class="btn btn-sm btn-success quick-confirm-btn" data-id="', row$id, '">Update</button>'))
        }
        
        # Add Delete button for all except already deleted or active statuses, but only if system is unlocked
        if (!row$plugging_status %in% c("Deleted") && !is_active_status(row$plugging_status) && !is_system_locked()) {
          btns <- c(btns, paste0('<button class="btn btn-sm btn-danger quick-delete-plugging-btn" data-id="', row$id, '">Delete</button>'))
        }
        paste(btns, collapse = ' ')
      })
      
      # Move actions column to second position (after id)
      display_data <- display_data[, c("id", "actions", "female_id", "female_age", "female_breeding_line", "female_genotype", 
                                       "pairing_start_date", "pairing_end_date", "plug_observed_date", 
                                       "plugging_status", "expected_age_for_harvesting", "notes")]

      pairing_group_index <- integer(nrow(display_data))
      current_group_index <- 0L
      previous_pairing_start <- NULL

      for (row_idx in seq_len(nrow(display_data))) {
        current_pairing_start <- display_data$pairing_start_date[row_idx]
        if (is.null(previous_pairing_start) || !identical(current_pairing_start, previous_pairing_start)) {
          current_group_index <- current_group_index + 1L
          previous_pairing_start <- current_pairing_start
        }
        pairing_group_index[row_idx] <- current_group_index
      }

      display_data$row_background <- vapply(seq_len(nrow(display_data)), function(row_idx) {
        group_is_even <- (pairing_group_index[row_idx] %% 2L) == 0L
        row_is_even <- (row_idx %% 2L) == 0L

        if (group_is_even && row_is_even) {
          return("#f6fbff")
        }
        if (group_is_even && !row_is_even) {
          return("#edf7ff")
        }
        if (!group_is_even && row_is_even) {
          return("#fcfcff")
        }

        "#f7f8fc"
      }, character(1))
      
      # Store the IDs for double-click functionality
      row_ids <- filtered$id

      dt <- DT::datatable(
        display_data,
        options = list(
          pageLength = 100,
          scrollX = TRUE,
          order = list(list(0, 'desc')),
          columnDefs = list(
            list(visible = FALSE, targets = c(0, ncol(display_data) - 1))
          ) # hide id and background helper columns
        ),
        rownames = FALSE,
        colnames = c("ID", "Actions", "Female ID", "Age (wks)", "Breeding Line", "Genotype", 
                     "Pairing Start", "Pairing End", "Plug Observed", "Status", "Harvesting @", "Notes", "Row Background"),
        selection = "single",
        escape = FALSE,
        callback = JS(
          'table.on("dblclick", "tr", function() {',
          '  var data = table.row(this).data();',
          '  if(data !== undefined) {',
          '    Shiny.setInputValue("plugging_history_table_row_dblclicked", data[0], {priority: "event"});',
          '  }',
          '});',
          'table.on("click", ".quick-confirm-btn", function() {',
          '  var id = $(this).data("id");',
          '  Shiny.setInputValue("quick_confirm_plugging_btn", id, {priority: "event"});',
          '});',
          'table.on("click", ".quick-delete-plugging-btn", function() {',
          '  var id = $(this).data("id");',
          '  Shiny.setInputValue("quick_delete_plugging_btn", id, {priority: "event"});',
          '});'
        )
      )

      DT::formatStyle(
        dt,
        columns = c("actions", "female_id", "female_age", "female_breeding_line", "female_genotype",
                    "pairing_start_date", "pairing_end_date", "plug_observed_date",
                    "plugging_status", "expected_age_for_harvesting", "notes"),
        valueColumns = "row_background",
        backgroundColor = DT::styleEqual(unique(display_data$row_background), unique(display_data$row_background))
      )
    }, finally = {
      db_disconnect(con)
      # Restore scroll and table state after table is rendered
      session$sendCustomMessage(type = "eval", message = "setTimeout(function() { if(typeof restoreDataTableState === 'function') restoreDataTableState('plugging_history_table'); }, 100);")
      session$sendCustomMessage(type = "eval", message = "setTimeout(function() { if(typeof restoreScrollForAllTables === 'function') restoreScrollForAllTables(); }, 200);")
    })
  })

  observeEvent(input$show_ongoing_plugging_summary_btn, {
    summary_text <- build_ongoing_plugging_summary()

    showModal(modalDialog(
      title = "Ongoing Plugging Summary",
      size = "m",
      tagList(
        div(
          style = "margin-bottom: 10px; color: #6c757d; font-size: 12px;",
          "Only ongoing plugging records are included. Entries are grouped by male and sorted by pairing start date."
        ),
        tags$textarea(
          id = "ongoing_plugging_summary_text",
          readonly = "readonly",
          style = "width: 100%; min-height: 220px; font-family: monospace; font-size: 13px; white-space: pre-wrap;",
          summary_text
        )
      ),
      footer = tagList(
        modalButton("Close")
      )
    ))

    session$sendCustomMessage(
      type = "eval",
      message = "setTimeout(function(){ var el = document.getElementById('ongoing_plugging_summary_text'); if (el) { el.focus(); el.select(); } }, 100);"
    )
  })
  
  # Helper to check if status is Plugged
  is_plugged_status <- function(status) {
    is.null(status) || status %in% c("Plugged", "Plug Confirmed", "Not Observed (Waiting for confirmation)", "Surprising Plug!!")
  }
  
  # Helper to check if status allows further actions (not final states)
  is_active_status <- function(status) {
    status %in% c("Ongoing", "Plugged", "Plug Confirmed", "Not Observed (Waiting for confirmation)", "Surprising Plug!!")
  }
  
  # Helper to check if status is Not Observed (Confirmed) or Not Pregnant
  is_not_observed_confirmed <- function(status) {
    status %in% c("Not Observed (Confirmed)", "Not Pregnant")
  }
  
  # Helper to check if status allows "Plug is Empty" action
  can_mark_empty <- function(status) {
    status %in% c("Plugged", "Plug Confirmed")
  }
  
  # Helper to check if plug observed date is valid (not NA, empty, or "Unknown")
  is_valid_plug_date <- function(date_value) {
    if (is.na(date_value) || date_value == "" || date_value == "Unknown") {
      return(FALSE)
    }
    # Try to parse as date, return FALSE if it fails
    tryCatch({
      as.Date(date_value)
      return(TRUE)
    }, error = function(e) {
      return(FALSE)
    })
  }
  
  # Helper to check if pairing date is valid (not NA, empty, or "Unknown")
  is_valid_pairing_date <- function(date_value) {
    if (is.na(date_value) || date_value == "" || date_value == "Unknown") {
      return(FALSE)
    }
    # Try to parse as date, return FALSE if it fails
    tryCatch({
      as.Date(date_value)
      return(TRUE)
    }, error = function(e) {
      return(FALSE)
    })
  }
  
  # --- Modification History UI ---
  output$modification_history_ui <- renderUI({
    req(plugging_state$viewing_id)
    plugging_state$viewing_refresh
    
    con <- db_connect()
    plugging <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_state$viewing_id))
    db_disconnect(con)
    
    if (nrow(plugging) == 0) {
      return(NULL)
    }
    
    female_id <- plugging$female_id[1]
    
    # Get all relevant audit trail entries
    tryCatch({
      history <- get_plugging_modification_history(plugging_state$viewing_id)
      
      if (nrow(history) == 0) {
        return(tags$div("No modification history found."))
      }
      
      # Group entries by timestamp to handle transactions properly
      history$timestamp_group <- as.character(history$timestamp)
      timestamp_groups <- unique(history$timestamp_group)
      
      filtered <- list()
      
      for (timestamp_group in timestamp_groups) {
        # Get all entries with the same timestamp
        same_time_entries <- history[history$timestamp_group == timestamp_group, ]
        
        # Check if any of these entries involve euthanasia
        has_euthanasia <- FALSE
        for (j in seq_len(nrow(same_time_entries))) {
          entry <- same_time_entries[j, ]
          if (entry$table_name == "mice_stock") {
            new_vals <- tryCatch({
              if (!is.null(entry$new_values) && entry$new_values != "" && !is.na(entry$new_values)) {
                jsonlite::fromJSON(entry$new_values)
              } else {
                list()
              }
            }, error = function(e) list())
            
            if (!is.null(new_vals$status) && new_vals$status == "Deceased" && 
                !is.null(new_vals$source) && new_vals$source == "Plugging Tab" &&
                !is.null(new_vals$source_event_id) && as.character(new_vals$source_event_id) == as.character(plugging_state$viewing_id)) {
              has_euthanasia <- TRUE
              break
            }
          }
        }
        
        # Process each entry in this timestamp group
        for (j in seq_len(nrow(same_time_entries))) {
          row <- same_time_entries[j, ]
          
          old <- tryCatch({
            if (is.null(row$old_values) || row$old_values == "" || is.na(row$old_values)) {
              list()
            } else {
              jsonlite::fromJSON(row$old_values)
            }
          }, error = function(e) {
            list()
          })
          
          new <- tryCatch({
            if (is.null(row$new_values) || row$new_values == "" || is.na(row$new_values)) {
              list()
            } else {
              jsonlite::fromJSON(row$new_values)
            }
          }, error = function(e) {
            list()
          })
          
          # Get plug observed date and date of death if available
          plug_observed_date <- NA
          date_of_death <- NA
          if (row$table_name == "plugging_history") {
            if (!is.null(new$plug_observed_date) && new$plug_observed_date != "" && new$plug_observed_date != "Unknown") {
              plug_observed_date <- new$plug_observed_date
            }
          }
          if (row$table_name == "mice_stock") {
            if (!is.null(new$date_of_death) && new$date_of_death != "") {
              date_of_death <- new$date_of_death
            }
          }
          # Format extra info
          extra_info <- c()
          if (!is.na(plug_observed_date) && plug_observed_date != "") {
            extra_info <- c(extra_info, paste0("Plug Observed: ", format(as.Date(plug_observed_date), "%d-%b-%Y")))
          }
          if (!is.na(date_of_death) && date_of_death != "") {
            extra_info <- c(extra_info, paste0("Date of Death: ", format(as.Date(date_of_death), "%d-%b-%Y")))
          }
          extra_info_str <- if (length(extra_info) > 0) paste0(" (", paste(extra_info, collapse = ", "), ")") else ""
          
          if (row$table_name == "plugging_history" && row$action == "DELETE") {
            filtered[[length(filtered) + 1]] <- list(user = row$user_id, date = row$timestamp, action = paste0("Deleted", extra_info_str))
          } else if (row$table_name == "plugging_history" && row$action == "INSERT") {
            filtered[[length(filtered) + 1]] <- list(user = row$user_id, date = row$timestamp, action = paste0("Plugging Record Created", extra_info_str))
          } else if (row$table_name == "plugging_history" && !is.null(old$plugging_status) && !is.null(new$plugging_status) && old$plugging_status != new$plugging_status) {
            # Special handling for Empty status: use the euthanasia flag from the same transaction
            if (new$plugging_status == "Empty") {
              action_label <- paste0("Plugging Status: ", old$plugging_status, "  →  ", ifelse(has_euthanasia, "Empty (Euthanized)", "Empty (Alive)"))
            } else {
              action_label <- paste0("Plugging Status: ", old$plugging_status, "  →  ", new$plugging_status)
            }
            if (row$user_id == "system(auto)") {
              action_label <- paste0(action_label, " (Auto)")
            }
            filtered[[length(filtered) + 1]] <- list(user = row$user_id, date = row$timestamp, action = paste0(action_label, extra_info_str))
          } else if (row$table_name == "mice_stock" && !is.null(old$status) && !is.null(new$status) && old$status != new$status) {
            if (!is.null(new$source) && new$source == "Plugging Tab" && new$status == "Deceased") {
              filtered[[length(filtered) + 1]] <- list(user = row$user_id, date = row$timestamp, action = paste0("Euthanized", extra_info_str))
            } else {
              filtered[[length(filtered) + 1]] <- list(user = row$user_id, date = row$timestamp, action = paste0("Mouse Status: ", old$status, "  →  ", new$status, extra_info_str))
            }
          } else if (row$table_name == "mice_stock" && row$action == "UPDATE") {
            if (!is.null(row$operation_details) && grepl("Euthanasia", row$operation_details, ignore.case = TRUE)) {
              filtered[[length(filtered) + 1]] <- list(user = row$user_id, date = row$timestamp, action = paste0("Euthanized", extra_info_str))
            } else {
              filtered[[length(filtered) + 1]] <- list(user = row$user_id, date = row$timestamp, action = paste0("Mouse Update: ", ifelse(is.null(row$operation_details) || row$operation_details == "", "Status change", row$operation_details), extra_info_str))
            }
          }
        }
      }
      
      filtered <- Filter(Negate(is.null), filtered)
      
      if (length(filtered) == 0) {
        return(tags$div("No status modification history found."))
      }
      
      df <- data.frame(
        User = sapply(filtered, function(x) x$user),
        Date = sapply(filtered, function(x) {
          tryCatch({
            if (!is.null(history$formatted_time)) {
              idx <- which(history$timestamp == x$date)
              if (length(idx) > 0) history$formatted_time[idx[1]] else as.character(x$date)
            } else {
              as.character(x$date)
            }
          }, error = function(e) {
            as.character(x$date)
          })
        }),
        Action = sapply(filtered, function(x) x$action),
        stringsAsFactors = FALSE
      )
      
      dt <- DT::datatable(df, rownames = FALSE, options = list(dom = 't', ordering = FALSE), escape = FALSE, selection = 'none')
      # Add color formatting: yellow for Plugging Status, blue for Mouse Status and Euthanized, red for Deleted, gray for Auto
      DT::formatStyle(
        dt,
        'Action',
        target = 'row',
        backgroundColor = DT::styleEqual(
          c('Plugging Record Created',
            unique(df$Action[grepl('Plugging Status:', df$Action) & !grepl('\\(Auto\\)$', df$Action)]),
            unique(df$Action[grepl('Mouse Status:', df$Action)]),
            'Euthanized',
            'Deleted',
            unique(df$Action[grepl('Plugging Status:.*\\(Auto\\)$', df$Action)])),
          c('#fffbe6', # light yellow for Plugging Record Created and Plugging Status
            rep('#fffbe6', length(unique(df$Action[grepl('Plugging Status:', df$Action) & !grepl('\\(Auto\\)$', df$Action)]))),
            rep('#e6f0ff', length(unique(df$Action[grepl('Mouse Status:', df$Action)]))), # light blue for Mouse Status
            '#e6f0ff', # light blue for Euthanized
            '#ffeaea', # light red for Deleted
            rep('#eeeeee', length(unique(df$Action[grepl('Plugging Status:.*\\(Auto\\)$', df$Action)]))) # gray for Auto
          )
        )
      )
    }, error = function(e) {
      tags$div(paste("Error loading modification history:", e$message))
    })
  })
  
  show_plugging_details_modal <- function(plugging_id) {
    if (is.null(plugging_id)) return()

    con <- db_connect()
    tryCatch({
      plugging <- DBI::dbGetQuery(con, 
        "SELECT ph.*, 
                m.dob as male_dob, m.breeding_line as male_breeding_line, m.genotype as male_genotype, m.status as male_status,
                f.dob as female_dob, f.breeding_line as female_breeding_line, f.genotype as female_genotype, f.status as female_status,
                f.date_of_death as female_date_of_death
         FROM plugging_history ph
         LEFT JOIN mice_stock m ON ph.male_id = m.asu_id
         LEFT JOIN mice_stock f ON ph.female_id = f.asu_id
         WHERE ph.id = ?",
        params = list(plugging_id))
      
      # Get body weight data for the female mouse
      female_body_weight_history <- tryCatch({
        DBI::dbGetQuery(con, paste0("SELECT * FROM body_weight_history WHERE asu_id = '", plugging[1, "female_id"], "' ORDER BY measurement_date DESC"))
      }, error = function(e) {
        data.frame()
      })
      
      # Get plugging history for the female mouse (for chart overlay)
      female_plugging_history <- tryCatch({
        DBI::dbGetQuery(con, paste0("SELECT * FROM plugging_history WHERE female_id = '", plugging[1, "female_id"], "' AND plugging_status != 'Deleted'"))
      }, error = function(e) {
        data.frame()
      })
      
      if (nrow(plugging) == 0) return()
      
      row <- plugging[1, ]
      report_defaults <- extract_plugging_final_report(plugging)
      mixed_age_text <- format_age_groups_text_from_json(report_defaults$final_report_age_groups_json)
      report_details <- extract_plugging_final_report(plugging)
      event_body_weight_history <- build_event_weight_window(female_body_weight_history, plugging, female_plugging_history)
      training_dataset <- details_prediction_training_dataset()
      current_prediction_mode <- normalize_prediction_breeding_line_mode(plugging_state$prediction_breeding_line_mode)
      current_prediction <- tryCatch(
        predict_plugging_event_outcome(
          plugging,
          event_body_weight_history,
          training_dataset,
          breeding_line_mode = current_prediction_mode
        ),
        error = function(e) list(
          likelihood = "Unavailable",
          confidence = "Low",
          estimated_age_range = "Unknown",
          conclusion = "Prediction is unavailable for this event.",
          anchor = list(date = as.Date(NA), type = "Unknown Anchor"),
          fitted_anchor = list(offset_days = 0, fitted_curve = data.frame(), anchor_label = "Unknown"),
          fitted_curve = data.frame()
        )
      )
      summary_text <- build_plugging_details_clipboard_text(row)
      summary_text_area_id <- paste0("plugging_summary_text_", row$id)
      copy_summary_onclick <- paste0(
        "(function(){",
        "var el = document.getElementById('", summary_text_area_id, "');",
        "if (!el) { return; }",
        "el.style.display = 'block';",
        "el.focus();",
        "el.select();",
        "el.setSelectionRange(0, el.value.length);",
        "var copied = false;",
        "try { copied = document.execCommand('copy'); } catch (e) { copied = false; }",
        "el.style.display = 'none';",
        "if (copied) {",
        "  if (window.Shiny && Shiny.setInputValue) { Shiny.setInputValue('plugging_summary_copied', Date.now(), {priority: 'event'}); }",
        "} else {",
        "  window.prompt('Copy to clipboard:', el.value);",
        "}",
        "})();"
      )
      
      # Calculate ages
      male_age <- if(!is.na(row$male_dob)) round(as.numeric(Sys.Date() - as.Date(row$male_dob)) / 7, 1) else NA
      female_age <- if(!is.na(row$female_dob)) round(as.numeric(Sys.Date() - as.Date(row$female_dob)) / 7, 1) else NA
      has_body_weight_records <- nrow(event_body_weight_history) > 0
      info_column_width <- 7
      body_weight_column_width <- 5
      
      showModal(modalDialog(
        title = paste("Plugging Event Details (", row$female_id, ")"),
        size = "l",
        tagList(
          tags$script(HTML(
            "setTimeout(function() {
              var dialogs = document.querySelectorAll('.modal-dialog');
              var dialog = dialogs[dialogs.length - 1];
              if (!dialog) {
                return;
              }
              dialog.style.setProperty('width', '58vw', 'important');
              dialog.style.setProperty('max-width', '984px', 'important');
              var body = dialog.querySelector('.modal-body');
              if (body) {
                body.style.padding = '12px 14px';
                body.style.maxHeight = '82vh';
                body.style.overflowY = 'auto';
              }
            }, 0);"
          )),
          div(
            style = "font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;",
            h4(paste("🐭 Plugging Details:", row$female_id), 
               style = "text-align: center; color: #1e3a5f; margin: 0 0 14px 0;"),
            div(
              style = "display: grid; grid-template-columns: minmax(280px, 1.25fr) minmax(520px, 3.55fr); gap: 12px; align-items: stretch; max-width: 984px; margin: 0 auto;",
              div(
                style = "display: flex; flex-direction: column; height: 100%;",
                div(
                  style = "background: linear-gradient(135deg, #e8f5e8 0%, #d4edda 100%); border-radius: 8px; padding: 10px 12px; border-left: 4px solid #28a745; margin-bottom: 10px;",
                  h5("👫 Breeding Pair", style = "margin: 0 0 10px 0; color: #2c3e50;"),
                  div(
                    div(
                      strong("Female: "), paste0(row$female_id, " (", ifelse(is.na(female_age), "Unknown age", paste0(female_age, " wks")), ")"),
                      br(),
                      "Line: ", ifelse(is.na(row$female_breeding_line), "Unknown", row$female_breeding_line),
                      br(),
                      "Genotype: ", ifelse(is.na(row$female_genotype), "Unknown", row$female_genotype)
                    ),
                    br(),
                    div(
                      strong("Male: "), paste0(row$male_id, " (", ifelse(is.na(male_age), "Unknown age", paste0(male_age, " wks")), ")"),
                      br(),
                      "Line: ", ifelse(is.na(row$male_breeding_line), "Unknown", row$male_breeding_line),
                      br(),
                      "Genotype: ", ifelse(is.na(row$male_genotype), "Unknown", row$male_genotype)
                    )
                  )
                ),
                div(
                  style = "background: rgba(135, 206, 235, 0.1); border-radius: 8px; padding: 10px 12px; border-left: 4px solid #87CEEB; margin-bottom: 10px;",
                  h5("⏰ Timeline", style = "margin: 0 0 8px 0; color: #2c3e50;"),
                  div(
                    "Pairing Start: ", strong(ifelse(is.na(row$pairing_start_date) || row$pairing_start_date == "", "Unknown", row$pairing_start_date)),
                    br(),
                    "Pairing End: ", strong(ifelse(is.na(row$pairing_end_date) || row$pairing_end_date == "", "Unknown", row$pairing_end_date)),
                    br(),
                    "Plug Observed: ", strong(ifelse(!is_valid_plug_date(row$plug_observed_date), "Unknown", row$plug_observed_date))
                  )
                ),
                div(
                  style = "background: rgba(255, 193, 7, 0.1); border-radius: 8px; padding: 10px 12px; border-left: 4px solid #ffc107; flex: 1;",
                  h5("📊 Status", style = "margin: 0 0 8px 0; color: #2c3e50;"),
                  div(
                    "Current Status: ", strong(row$plugging_status),
                    br(),
                    "Expected Harvest Age: ", strong(ifelse(is.na(row$expected_age_for_harvesting) || row$expected_age_for_harvesting == "", "Not specified", row$expected_age_for_harvesting)),
                    if (!is.na(strip_plugging_status_audit_notes(row$notes)) && strip_plugging_status_audit_notes(row$notes) != "") {
                      tagList(br(), "Notes: ", span(style = "font-style: italic;", strip_plugging_status_audit_notes(row$notes)))
                    }
                  )
                ),
                tagList({
                  summary_lines <- build_prediction_summary_lines(current_prediction)
                  metadata_lines <- build_prediction_metadata_lines(current_prediction)

                  if (length(c(summary_lines, metadata_lines)) > 0) {
                    div(
                      style = "background: rgba(245, 158, 11, 0.1); border-radius: 8px; padding: 10px 12px; border-left: 4px solid #f59e0b; margin-top: 10px;",
                      h5("🧪 Prediction Details", style = "margin: 0 0 8px 0; color: #2c3e50;"),
                      if (length(summary_lines) > 0) {
                        div(
                          style = "color: #92400e; font-size: 0.92em; line-height: 1.4;",
                          tagList(lapply(summary_lines, function(line_text) div(line_text)))
                        )
                      },
                      if (length(metadata_lines) > 0) {
                        div(
                          style = "margin-top: 8px; color: #a16207; font-size: 0.82em; line-height: 1.35;",
                          tagList(lapply(metadata_lines, function(line_text) div(line_text)))
                        )
                      }
                    )
                  }
                })
              ),
              if (has_body_weight_records) {
                div(
                  style = "border-radius: 8px; padding: 8px 10px; display: flex; flex-direction: column; height: 100%;",
                  div(
                    style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
                    h5("📈 Female Body Weight Trend", style = "margin: 0; color: #2c3e50;"),
                    actionButton(
                      inputId = "add_body_weight_from_plugging_btn",
                      label = "Add Body Weight",
                      class = "btn-success btn-sm",
                      onclick = paste0("Shiny.setInputValue('add_body_weight_from_plugging_clicked', '", row$female_id, "', {priority: 'event'});")
                    )
                  ),
                  div(
                    style = "background: #fff8e1; border: 1px solid #fde68a; border-radius: 8px; padding: 10px 12px; margin-bottom: 10px;",
                    h5("Pregnancy Fit", style = "margin: 0 0 8px 0; color: #b45309;"),
                    div(
                      style = "color: #92400e; font-weight: 600;",
                      build_prediction_plot_label(current_prediction)
                    ),
                    {
                      timing_lines <- build_prediction_timing_lines(current_prediction)
                      tagList(
                        if (length(timing_lines) > 0) {
                          div(
                            style = "margin-top: 8px; color: #92400e; font-size: 0.92em; line-height: 1.4;",
                            tagList(lapply(timing_lines, function(line_text) {
                              div(
                                style = "margin-top: 6px; font-weight: 700; color: #7c2d12; background: rgba(245, 158, 11, 0.18); border-left: 3px solid #f59e0b; padding: 4px 8px; border-radius: 6px;",
                                line_text
                              )
                            }))
                          )
                        }
                      )
                    }
                  ),
                  div(
                    id = "plugging_body_weight_preview_plot_container",
                    style = "height: 360px; flex: 1;",
                    plotlyOutput(paste0("plugging_body_weight_preview_plot_", row$female_id), height = "360px")
                  )
                )
              } else {
                div(
                  style = "border-radius: 8px; padding: 8px 10px; display: flex; align-items: center; justify-content: center; height: 100%; min-height: 320px;",
                  div(
                    style = "text-align: center; color: #6c757d;",
                    h5("📈 No Body Weight Data", style = "margin-bottom: 10px;"),
                    p("No body weight records found for this mouse."),
                    div(
                      style = "background: #fff8e1; border: 1px solid #fde68a; border-radius: 8px; padding: 10px 12px; margin: 10px 0; text-align: left;",
                      div(
                        style = "color: #92400e; font-weight: 600;",
                        build_prediction_plot_label(current_prediction)
                      ),
                      {
                        timing_lines <- build_prediction_timing_lines(current_prediction)
                        tagList(
                          if (length(timing_lines) > 0) {
                            div(
                              style = "margin-top: 8px; color: #92400e; font-size: 0.92em; line-height: 1.4;",
                              tagList(lapply(timing_lines, function(line_text) {
                                div(
                                  style = "margin-top: 6px; font-weight: 700; color: #7c2d12; background: rgba(245, 158, 11, 0.18); border-left: 3px solid #f59e0b; padding: 4px 8px; border-radius: 6px;",
                                  line_text
                                )
                              }))
                            )
                          }
                        )
                      }
                    ),
                    actionButton(
                      inputId = "add_body_weight_from_plugging_btn",
                      label = "Add First Record",
                      class = "btn-success btn-sm",
                      onclick = paste0("Shiny.setInputValue('add_body_weight_from_plugging_clicked', '", row$female_id, "', {priority: 'event'});")
                    )
                  )
                )
              }
            )
          ),
          wellPanel(
            style = "margin: 12px auto 0 auto; padding: 10px 12px; max-width: 984px;",
            tags$h4("Modification History", style = "margin: 0 0 8px 0;"),
            uiOutput("modification_history_ui")
          ),
          tags$textarea(
            id = summary_text_area_id,
            style = "position: absolute; left: -9999px; top: -9999px; opacity: 0; display: none;",
            readonly = "readonly",
            summary_text
          )
        ),
        footer = tagList(
          div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            div(
              style = "margin-top: 0;",
              if (row$female_status != "Deceased" && row$plugging_status != "Deleted" && is_active_status(row$plugging_status)) {
                tagList(
                  # Show "Mark as Plugged" for Ongoing status
                  if (row$plugging_status == "Ongoing") {
                    actionButton("mark_plug_observed_btn", "Plugged", class = "btn-success")
                  },
                  # Show "Surprising Plug!! 😱" for Not Observed (Waiting for confirmation) status
                  if (row$plugging_status == "Not Observed (Waiting for confirmation)") {
                    actionButton("mark_surprising_plug_btn", "Surprising Plug!!😱", class = "btn-success", style = "background-color: #ff6b6b; border-color: #ff6b6b;")
                  },
                  # Show "Euthanized" for all active statuses (always allowed)
                  actionButton("euthanize_mice_btn", "Euthanized", class = "btn-warning"),
                  # Show "Empty Plug" for statuses that can be marked as empty (always allowed)
                  if (can_mark_empty(row$plugging_status)) {
                    actionButton("set_status_empty_btn", "Empty Plug (Alive)", class = "btn-info")
                  }
                )
              }
            ),
            div(
              tags$button(type = "button", class = "btn btn-info", style = "margin-right: 6px;", onclick = copy_summary_onclick, "Copy Summary"),
              actionButton("edit_plugging_details_btn", "Edit", class = "btn-primary", style = "margin-left: 6px;"),
              # Only hide the Delete button when locked, allow other actions
              if (!is_not_observed_confirmed(row$plugging_status) && row$female_status != "Deceased" && row$plugging_status != "Deleted" && row$plugging_status != "Not Observed (Waiting for confirmation)" && !is_system_locked()) {
                actionButton("delete_plugging_btn", "Delete", class = "btn-danger")
              },
              modalButton("Cancel")
            )
          )
        )
      ))
      
      # Render body weight chart if data exists
      if (nrow(event_body_weight_history) > 0) {
        # Use custom output name for plugging modal to avoid conflicts
        output[[paste0("plugging_body_weight_preview_plot_", row$female_id)]] <- renderPlotly({
          # Create the base plotly chart
          weight_data <- event_body_weight_history
          
          # Robust date conversion - handle various date formats
          weight_data$measurement_date <- tryCatch({
            if (is.character(weight_data$measurement_date)) {
              as.Date(weight_data$measurement_date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%Y/%m/%d"))
            } else {
              as.Date(weight_data$measurement_date)
            }
          }, error = function(e) {
            # If date conversion fails, use current date as fallback
            rep(Sys.Date(), nrow(weight_data))
          })
          
          p <- plot_ly(
            data = weight_data,
            x = ~measurement_date,
            y = ~weight_grams,
            type = "scatter",
            mode = "lines+markers",
            line = list(color = "#2196f3", width = 2),
            marker = list(color = "#2196f3", size = 7),
            name = "Body Weight",
            showlegend = TRUE,
            hovertemplate = paste(
              "<b>Date:</b> %{x}<br>",
              "<b>Weight:</b> %{y} grams<br>",
              "<extra></extra>"
            )
          )

          if (!is.na(current_prediction$anchor$date) && nrow(current_prediction$fitted_curve) > 0) {
            fitted_curve_data <- current_prediction$fitted_curve[current_prediction$fitted_curve$day_since_anchor >= 0, , drop = FALSE]
            fitted_curve_data$measurement_date <- as.POSIXct(current_prediction$anchor$date) + fitted_curve_data$day_since_anchor * 86400

            p <- add_trace(
              p,
              data = fitted_curve_data,
              x = ~measurement_date,
              y = ~predicted_weight,
              type = "scatter",
              mode = "lines+markers",
              name = "Pregnancy Date Curve",
              line = list(color = "#f59e0b", width = 2, dash = "dash"),
              marker = list(color = "#f59e0b", size = 6),
              hovertemplate = "<b>Date:</b> %{x}<br><b>Curve:</b> %{y:.2f} grams<br><extra></extra>"
            )
          }

          y_range <- build_body_weight_plot_y_range(
            actual_weights = weight_data$weight_grams,
            fitted_weights = if (exists("fitted_curve_data")) fitted_curve_data$predicted_weight else numeric(0)
          )
          
          # Initialize shapes list for layout
          shapes_list <- list()
          
          # Add plugging events if they exist (simplified for preview)
          if (nrow(female_plugging_history) > 0) {
            # Process each plugging record (simplified)
            for (i in seq_len(nrow(female_plugging_history))) {
              plug_row <- female_plugging_history[i, ]
              
              # Add gray shaded area for pairing period
              if (!is.na(plug_row$pairing_start_date) && plug_row$pairing_start_date != "") {
                start_date <- tryCatch({
                  if (is.character(plug_row$pairing_start_date)) {
                    as.Date(plug_row$pairing_start_date, 
                           tryFormats = c("%Y-%m-%d", "%m/%d/%Y", 
                                         "%d/%m/%Y", "%Y/%m/%d"))
                  } else {
                    as.Date(plug_row$pairing_start_date)
                  }
                }, error = function(e) {
                  Sys.Date()  # Fallback to today
                })
                
                end_date <- if (!is.na(plug_row$pairing_end_date) && plug_row$pairing_end_date != "") {
                  tryCatch({
                    if (is.character(plug_row$pairing_end_date)) {
                      as.Date(plug_row$pairing_end_date, 
                           tryFormats = c("%Y-%m-%d", "%m/%d/%Y", 
                                         "%d/%m/%Y", "%Y/%m/%d"))
                    } else {
                      as.Date(plug_row$pairing_end_date)
                    }
                  }, error = function(e) {
                    max(weight_data$measurement_date, Sys.Date())
                  })
                } else {
                  max(weight_data$measurement_date, Sys.Date())
                }
                
                # Add rectangle shape for pairing period
                shapes_list[[length(shapes_list) + 1]] <- list(
                  type = "rect",
                  x0 = start_date, x1 = end_date,
                  y0 = 0, y1 = 1, yref = "paper",
                  fillcolor = "rgba(128, 128, 128, 0.3)",
                  line = list(color = "rgba(0,0,0,0)", width = 0),
                  layer = "below"
                )
              }
              
              # Add plug observed markers
              if (!is.na(plug_row$plug_observed_date) && plug_row$plug_observed_date != "") {
                plug_date <- tryCatch({
                  if (is.character(plug_row$plug_observed_date)) {
                    as.Date(plug_row$plug_observed_date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%Y/%m/%d"))
                  } else {
                    as.Date(plug_row$plug_observed_date)
                  }
                }, error = function(e) {
                  NULL  # Skip this marker if date parsing fails
                })
                
                if (!is.null(plug_date)) {
                  # Determine color based on plugging status
                  plug_color <- if (plug_row$plugging_status == "Collected") {
                    "#388e3c"  # Success - Green
                  } else if (plug_row$plugging_status %in% c("Empty", "Not Pregnant (Confirmed)", "Not Pregnant")) {
                    "#d32f2f"  # Failed - Red
                  } else {
                    "#ff9800"  # Pending - Orange
                  }
                  
                  # Add line shape for vertical line
                  shapes_list[[length(shapes_list) + 1]] <- list(
                    type = "line",
                    x0 = plug_date, x1 = plug_date,
                    y0 = 0, y1 = 1, yref = "paper",
                    line = list(color = plug_color, width = 2)
                  )
                }
              }
            }

            if (!is.na(current_prediction$anchor$date) && !is.null(current_prediction$fitted_anchor$offset_days) &&
                is.finite(current_prediction$fitted_anchor$offset_days) && abs(current_prediction$fitted_anchor$offset_days) >= 0.5) {
              potential_date <- as.POSIXct(current_prediction$anchor$date) + current_prediction$fitted_anchor$offset_days * 86400
              shapes_list[[length(shapes_list) + 1]] <- list(
                type = "line",
                x0 = potential_date, x1 = potential_date,
                y0 = 0, y1 = 1, yref = "paper",
                line = list(color = "#f59e0b", width = 2, dash = "dot")
              )
            }
          }
          
          # Calculate x-axis range
          all_dates <- weight_data$measurement_date
          if (length(all_dates) > 0) {
            date_range <- range(all_dates, na.rm = TRUE)
            date_diff <- as.numeric(diff(date_range))
            
            # For single data point or very short periods, use minimum 7-day window
            if (date_diff <= 3) {
              center_date <- mean(date_range)
              x_range <- c(center_date - 3.5, center_date + 3.5)
            } else {
              date_padding <- date_diff * 0.15
              x_range <- c(date_range[1] - date_padding, 
                           date_range[2] + date_padding)
            }
          } else {
            x_range <- NULL
          }
          
          # Apply layout with shapes
          p <- p %>% layout(
            xaxis = list(
              title = "",
              showgrid = TRUE,
              gridcolor = "#e0e0e0",
              tickformat = "%d-%b",
              range = x_range
            ),
            yaxis = list(
              title = "Weight (grams)",
              showgrid = TRUE,
              gridcolor = "#e0e0e0",
              range = y_range
            ),
            shapes = shapes_list,
            hovermode = "closest",
            plot_bgcolor = "rgba(0,0,0,0)",
            paper_bgcolor = "rgba(0,0,0,0)",
            margin = list(t = 20, b = 30, l = 50, r = 20),
            showlegend = TRUE,
            legend = list(orientation = "h", y = -0.2)
          )
          
          return(p)
        })
      }
      
      plugging_state$viewing_id <- plugging_id
      plugging_state$viewing_refresh <- isolate(plugging_state$viewing_refresh) + 1
      
    }, finally = {
      db_disconnect(con)
    })
  }

  # Edit plugging event - show detailed information
  observeEvent(input$plugging_history_table_row_dblclicked, {
    plugging_id <- input$plugging_history_table_row_dblclicked
    if (is.null(plugging_id)) return()

    show_plugging_details_modal(plugging_id)
    
    # Reset the input so the same row can be double-clicked again
    session$sendInputMessage("plugging_history_table_row_dblclicked", NULL)
  })

  observeEvent(input$add_body_weight_from_plugging_clicked, {
    asu_id <- input$add_body_weight_from_plugging_clicked
    if (!is.null(asu_id) && asu_id != "") {
      show_body_weight_input(
        input,
        output,
        session,
        asu_id,
        back_input_id = "plugging_body_weight_back_clicked",
        close_input_id = "plugging_body_weight_close_clicked"
      )
    }
  }, ignoreInit = TRUE)

  observeEvent(input$plugging_body_weight_back_clicked, {
    req(input$plugging_body_weight_back_clicked)

    removeModal()

    if (!is.null(plugging_state$viewing_id)) {
      show_plugging_details_modal(plugging_state$viewing_id)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$plugging_body_weight_close_clicked, {
    req(input$plugging_body_weight_close_clicked)

    removeModal()

    if (!is.null(plugging_state$viewing_id)) {
      show_plugging_details_modal(plugging_state$viewing_id)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$plugging_summary_copied, {
    showNotification("Plugging summary copied to clipboard", type = "message")
  })

  observeEvent(plugging_state$open_details_id, {
    req(plugging_state$open_details_id)

    external_id <- plugging_state$open_details_id
    plugging_state$open_details_id <- NULL
    show_plugging_details_modal(external_id)
  }, ignoreInit = TRUE)

  observeEvent(plugging_state$open_collection_id, {
    req(plugging_state$open_collection_id)

    external_id <- plugging_state$open_collection_id
    plugging_state$open_collection_id <- NULL
    show_collection_report_modal(external_id)
  }, ignoreInit = TRUE)

  # Controls whether canceling edit should reopen details modal.
  # TRUE for standard details->edit flow; FALSE for external direct edit requests.
  return_to_details_after_edit <- reactiveVal(TRUE)
  edit_modal_date_defaults <- reactiveVal(NULL)

  show_plugging_edit_modal <- function(plugging_id) {
    if (is.null(plugging_id)) {
      return()
    }

    edit_embryo_autofill_enabled(TRUE)

    align_for_binding <- function(source_df, target_columns) {
      missing_columns <- setdiff(target_columns, colnames(source_df))
      for (column_name in missing_columns) {
        source_df[[column_name]] <- NA
      }

      source_df[, target_columns, drop = FALSE]
    }
    
    con <- db_connect()
    tryCatch({
      plugging <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      
      if (nrow(plugging) == 0) return()
      row <- plugging[1, ]
      report_defaults <- extract_plugging_final_report(plugging)
      initialize_edit_other_age_rows(report_defaults)
      mixed_age_text <- format_age_groups_text_from_json(report_defaults$final_report_age_groups_json)
      
      # Get available LIVE mice for dropdown
      live_mice_data <- get_live_mice()
      
      # Get the full details of the mice currently in the record,
      # as they might be deceased and not in the live list.
      con_mice <- db_connect()
      current_male <- dbGetQuery(con_mice, "SELECT * FROM mice_stock WHERE asu_id = ?", params = list(row$male_id))
      current_female <- dbGetQuery(con_mice, "SELECT * FROM mice_stock WHERE asu_id = ?", params = list(row$female_id))
      db_disconnect(con_mice)

      # --- Male Choices ---
      # Combine live males with the current male if it's not already in the list
      all_males <- live_mice_data$males
      if (nrow(current_male) > 0 && !(current_male$asu_id[1] %in% all_males$asu_id)) {
        # Ensure columns match before rbind
        if (nrow(all_males) > 0) {
          all_males <- rbind(align_for_binding(current_male, colnames(all_males)), all_males)
        } else {
          all_males <- current_male
        }
      }
      male_choices <- setNames(all_males$asu_id, 
                              paste(all_males$asu_id, "-", all_males$breeding_line, "(", all_males$genotype, ")"))

      # --- Female Choices ---
      # Combine live females with the current female if it's not already in the list
      all_females <- live_mice_data$females
      if (nrow(current_female) > 0 && !(current_female$asu_id[1] %in% all_females$asu_id)) {
        # Ensure columns match before rbind
        if (nrow(all_females) > 0) {
          all_females <- rbind(align_for_binding(current_female, colnames(all_females)), all_females)
        } else {
          all_females <- current_female
        }
      }
      female_choices <- setNames(all_females$asu_id, 
                                paste(all_females$asu_id, "-", all_females$breeding_line, "(", all_females$genotype, ")"))

      pairing_start_valid <- is_valid_pairing_date(row$pairing_start_date)
      pairing_end_valid <- is_valid_pairing_date(row$pairing_end_date)
      plug_observed_valid <- is_valid_plug_date(row$plug_observed_date)

      default_pairing_start <- if (pairing_start_valid) as.Date(row$pairing_start_date) else Sys.Date()
      default_pairing_end <- if (pairing_end_valid) as.Date(row$pairing_end_date) else Sys.Date()
      default_plug_observed <- if (plug_observed_valid) as.Date(row$plug_observed_date) else Sys.Date()

      edit_modal_date_defaults(list(
        pairing_start_was_missing = !pairing_start_valid,
        pairing_end_was_missing = !pairing_end_valid,
        plug_observed_was_missing = !plug_observed_valid,
        pairing_start_default = as.character(default_pairing_start),
        pairing_end_default = as.character(default_pairing_end),
        plug_observed_default = as.character(default_plug_observed)
      ))
      
      # Close the details modal first
      removeModal()
      
      # Show edit modal
      showModal(modalDialog(
        title = paste("Edit Plugging Event - Female: ", row$female_id),
        size = "l",
        tagList(
          div(
            style = "background-color: #e3f2fd; border-left: 4px solid #2196f3; padding: 10px; margin-bottom: 15px; border-radius: 4px;",
            tags$strong("Note: "), 
            "This edit modal is for the specific plugging event with female ", tags$code(row$female_id), 
            ". For trio mating, each female has a separate plugging record."
          ),
          fluidRow(
            column(6, selectInput("edit_plugging_female", "Female ID", choices = female_choices, selected = row$female_id)),
            column(6, selectInput("edit_plugging_male", "Male ID", choices = male_choices, selected = row$male_id))
          ),
          fluidRow(
            column(6, dateInput("edit_pairing_start_date", "Pairing Start Date", 
                                value = default_pairing_start)),
            column(6, dateInput("edit_pairing_end_date", "Pairing End Date", 
                                value = default_pairing_end))
          ),
          fluidRow(
            column(6, 
              radioButtons("edit_plug_observed_type", "Plug Observed Date Type", 
                          choices = c("Specific Date" = "date", "Unknown" = "unknown"),
                          selected = if(is_valid_plug_date(row$plug_observed_date)) "date" else "unknown"),
              conditionalPanel(
                condition = "input.edit_plug_observed_type == 'date'",
                div(style = "margin-top: -10px;", dateInput("edit_plug_observed_date", "Plug Observed Date", 
                          value = default_plug_observed))
              )
            ),
            column(6, selectInput("edit_plugging_status", "Plugging Status", 
                                 choices = PLUGGING_STATUSES, selected = row$plugging_status)),
            column(6, textInput("edit_expected_age_for_harvesting", "Expected Age for Harvesting (Embryonic Days, e.g. 14)", value = if(!is.null(row$expected_age_for_harvesting) && !is.na(row$expected_age_for_harvesting)) row$expected_age_for_harvesting else "", width = "100%"))
          ),
          textAreaInput("edit_plugging_notes", "Plugging History Notes", value = row$notes, rows = 3),
          conditionalPanel(
            condition = "input.edit_plugging_status == 'Collected' || input.edit_plugging_status == 'Empty'",
            div(
              style = "margin-top: 14px; padding: 12px 14px; background: #fff8e1; border-left: 4px solid #f59e0b; border-radius: 6px;",
              h4("Collection / Sacrifice Report", style = "margin: 0 0 10px 0; color: #b45309; font-size: 1.05em;"),
              fluidRow(
                column(6,
                  fluidRow(
                    column(4,
                      numericInput(
                        "edit_final_report_total_embryos",
                        "Total Embryos",
                        value = ifelse(is.na(report_defaults$final_report_total_embryos), NA, report_defaults$final_report_total_embryos),
                        min = 0,
                        step = 1
                      )
                    ),
                    column(8,
                      textInput(
                        "edit_final_report_primary_age",
                        "Primary Embryo Age",
                        value = ifelse(is.na(report_defaults$final_report_primary_age), "", report_defaults$final_report_primary_age),
                        placeholder = "E16 or E16.5"
                      )
                    )
                  ),
                  dateInput(
                    "edit_final_report_date",
                    "Collection / Sacrifice Date",
                    value = if (!is.na(report_defaults$final_report_date) && report_defaults$final_report_date != "") as.Date(report_defaults$final_report_date) else Sys.Date()
                  ),
                  checkboxInput(
                    "edit_final_report_mixed_age",
                    "Mixed embryo ages in this collection",
                    value = isTRUE(report_defaults$final_report_mixed_age)
                  ),
                  tags$div(style = "border-top: 1px solid #cbd5e1; margin: 8px 0 6px 0;"),
                  tags$div(style = "font-weight: 600; color: #334155; margin-top: 6px; margin-bottom: 4px;", "Other Embryo Ages and Number"),
                  uiOutput("edit_other_age_rows_ui"),
                  div(
                    style = "display: none;",
                    textAreaInput(
                      "edit_final_report_age_groups",
                      "Age Group Details",
                      value = "",
                      rows = 2,
                      placeholder = "E15.5 x 1; E16.5 x 7"
                    )
                  )
                ),
                column(6,
                  tags$div(style = "border-top: 1px solid #cbd5e1; margin: 8px 0 10px 0;"),
                  fluidRow(
                    column(4, numericInput("edit_final_report_male_embryos", "Male", value = ifelse(is.na(report_defaults$final_report_male_embryos), NA, report_defaults$final_report_male_embryos), min = 0, step = 1)),
                    column(4, numericInput("edit_final_report_female_embryos", "Female", value = ifelse(is.na(report_defaults$final_report_female_embryos), NA, report_defaults$final_report_female_embryos), min = 0, step = 1)),
                    column(4, numericInput("edit_final_report_unknown_embryos", "Unknown", value = ifelse(is.na(report_defaults$final_report_unknown_embryos), NA, report_defaults$final_report_unknown_embryos), min = 0, step = 1))
                  ),
                  textAreaInput(
                    "edit_final_report_notes",
                    "Sample Collection Notes",
                    value = if (!is.null(report_defaults$final_report_notes) && !is.na(report_defaults$final_report_notes)) report_defaults$final_report_notes else "",
                    rows = 3
                  )
                )
              )
            )
          )
        ),
        footer = tagList(
          actionButton("cancel_plugging_edit_btn", "Cancel", class = "btn btn-default"),
          actionButton("save_plugging_edit_btn", "Save Changes", class = "btn-primary")
        )
      ))
      
      plugging_state$editing_id <- plugging_id
      
    }, finally = {
      db_disconnect(con)
    })
  }

  # Edit button from details view
  observeEvent(input$edit_plugging_details_btn, {
    plugging_id <- plugging_state$viewing_id
    if (is.null(plugging_id)) return()
    return_to_details_after_edit(TRUE)
    show_plugging_edit_modal(plugging_id)
  })

  # External edit request (for prediction not_pregnant rows)
  observeEvent(plugging_state$open_edit_id, {
    req(plugging_state$open_edit_id)

    external_id <- plugging_state$open_edit_id
    plugging_state$open_edit_id <- NULL
    return_to_details_after_edit(FALSE)
    show_plugging_edit_modal(external_id)
  })
  
  # Helper function to perform the actual save operation, callable from multiple places
  perform_plugging_update <- function(plugging_id, update_data) {
    con <- db_connect()
    tryCatch({
      current <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      if (nrow(current) == 0) {
        showNotification("Plugging event not found", type = "error")
        return()
      }
      old_values <- current[1, ]

      normalize_compare_value <- function(value) {
        if (is.null(value) || length(value) == 0 || all(is.na(value))) {
          return(NA_character_)
        }

        scalar <- value[1]
        if (is.logical(scalar)) {
          if (is.na(scalar)) return(NA_character_)
          return(ifelse(isTRUE(scalar), "1", "0"))
        }

        if (is.numeric(scalar)) {
          if (is.na(scalar)) return(NA_character_)
          return(as.character(scalar))
        }

        text_value <- trimws(as.character(scalar))
        if (identical(text_value, "") || identical(tolower(text_value), "na")) {
          return(NA_character_)
        }

        text_value
      }

      changed_fields <- names(update_data)[vapply(names(update_data), function(field_name) {
        old_value <- if (field_name %in% colnames(old_values)) old_values[[field_name]][1] else NA
        new_value <- update_data[[field_name]]

        old_norm <- normalize_compare_value(old_value)
        new_norm <- normalize_compare_value(new_value)

        !(is.na(old_norm) && is.na(new_norm)) && !identical(old_norm, new_norm)
      }, logical(1))]

      if (length(changed_fields) == 0) {
        showNotification("No changes detected. Nothing was saved.", type = "warning")
        removeModal()
        plugging_state$editing_id <- NULL
        edit_modal_date_defaults(NULL)
        return_to_details_after_edit(TRUE)
        return()
      }

      changed_update_data <- update_data[changed_fields]
      
      result <- DBI::dbExecute(con, 
        "UPDATE plugging_history SET 
         male_id = ?, female_id = ?, pairing_start_date = ?, pairing_end_date = ?,
         plug_observed_date = ?, plugging_status = ?, expected_age_for_harvesting = ?,
         notes = ?, final_report_date = ?, final_report_primary_age = ?, final_report_primary_age_value = ?,
         final_report_total_embryos = ?, final_report_male_embryos = ?, final_report_female_embryos = ?,
         final_report_unknown_embryos = ?, final_report_mixed_age = ?, final_report_age_groups_json = ?,
         final_report_notes = ?, updated_at = DATETIME('now')
         WHERE id = ?",
        params = list(
          update_data$male_id, update_data$female_id, update_data$pairing_start_date,
          update_data$pairing_end_date, update_data$plug_observed_date, update_data$plugging_status,
          update_data$expected_age_for_harvesting, update_data$notes,
          update_data$final_report_date, update_data$final_report_primary_age, update_data$final_report_primary_age_value,
          update_data$final_report_total_embryos, update_data$final_report_male_embryos, update_data$final_report_female_embryos,
          update_data$final_report_unknown_embryos, ifelse(isTRUE(update_data$final_report_mixed_age), 1L, 0L),
          update_data$final_report_age_groups_json, update_data$final_report_notes, plugging_id
        )
      )
      
      if (result > 0) {
        log_audit_trail("plugging_history", plugging_id, "UPDATE", old_values, changed_update_data)
        showNotification("Plugging event updated successfully", type = "message")
        removeModal() # Close confirmation modal if open
        removeModal() # Close edit modal
        Sys.sleep(1)
        auto_update_plugging_status_to_unknown()
        plugging_state$editing_id <- NULL
        edit_modal_date_defaults(NULL)
        plugging_state$viewing_id <- plugging_id
        # Save state before reload
        session$sendCustomMessage(type = "eval", message = "if(typeof saveDataTableState === 'function') saveDataTableState('plugging_history_table');")
        session$sendCustomMessage(type = "eval", message = "if(typeof saveScrollForAllTables === 'function') saveScrollForAllTables();")
        plugging_state$reload <- Sys.time()
        plugging_state$open_details_id <- plugging_id
      } else {
        showNotification("Failed to update plugging event", type = "error")
      }
    }, error = function(e) {
      showNotification(paste("Error updating plugging event:", e$message), type = "error")
    }, finally = {
      db_disconnect(con)
    })
  }

  # Save plugging edit - Step 1: Validate and check for ID changes
  observeEvent(input$save_plugging_edit_btn, {
    plugging_id <- plugging_state$editing_id
    if (is.null(plugging_id)) return()

    normalize_text_scalar <- function(value) {
      if (is.null(value) || length(value) == 0 || is.na(value[1])) {
        return("")
      }

      trimws(as.character(value[1]))
    }
    
    # Validation
    if (is.null(input$edit_plugging_male) || input$edit_plugging_male == "" || is.null(input$edit_plugging_female) || input$edit_plugging_female == "") {
      showNotification("Please select both a male and a female mouse.", type = "error")
      return()
    }
    if (input$edit_plugging_male == input$edit_plugging_female) {
      showNotification("Male and female cannot be the same mouse", type = "error")
      return()
    }
    
    con <- db_connect()
    original_record <- tryCatch({
      DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
    }, finally = {
      db_disconnect(con)
    })
    
    if (nrow(original_record) == 0) {
      showNotification("Original record not found.", type = "error")
      return()
    }

    id_changed <- (original_record$male_id[1] != input$edit_plugging_male) || (original_record$female_id[1] != input$edit_plugging_female)
    
    safe_modal_date_text <- function(date_value) {
      parsed_date <- safe_analysis_date(date_value)
      if (is.na(parsed_date)) {
        return(NA_character_)
      }

      as.character(parsed_date)
    }

    normalize_edit_modal_date <- function(date_value, missing_flag_key, default_value_key) {
      parsed_value <- safe_modal_date_text(date_value)
      if (is.na(parsed_value)) {
        return(NA_character_)
      }

      modal_defaults <- edit_modal_date_defaults()
      if (!is.null(modal_defaults) && isTRUE(modal_defaults[[missing_flag_key]]) &&
          !is.null(modal_defaults[[default_value_key]]) &&
          identical(parsed_value, as.character(modal_defaults[[default_value_key]]))) {
        return(NA_character_)
      }

      parsed_value
    }

    pairing_start_date_value <- normalize_edit_modal_date(input$edit_pairing_start_date, "pairing_start_was_missing", "pairing_start_default")
    pairing_end_date_value <- normalize_edit_modal_date(input$edit_pairing_end_date, "pairing_end_was_missing", "pairing_end_default")
    plug_observed_date_value <- if (input$edit_plug_observed_type == "unknown") {
      "Unknown"
    } else {
      normalize_edit_modal_date(input$edit_plug_observed_date, "plug_observed_was_missing", "plug_observed_default")
    }

    report_details <- extract_plugging_final_report(original_record)
    if (input$edit_plugging_status %in% c("Collected", "Empty")) {
      primary_age_text <- normalize_text_scalar(input$edit_final_report_primary_age)
      normalized_primary_age <- normalize_embryo_age_input(primary_age_text)
      primary_age_label <- if (is.na(normalized_primary_age$numeric)) {
        if (primary_age_text == "") NA_character_ else primary_age_text
      } else {
        normalized_primary_age$label
      }

      mixed_age_text <- normalize_text_scalar(input$edit_final_report_age_groups)
      mixed_age_flag <- isTRUE(input$edit_final_report_mixed_age)
      age_groups_json <- NA_character_
      normalized_counts <- normalize_final_report_embryo_counts(
        total_embryos = input$edit_final_report_total_embryos,
        male_embryos = input$edit_final_report_male_embryos,
        female_embryos = input$edit_final_report_female_embryos,
        unknown_embryos = input$edit_final_report_unknown_embryos
      )
      if (!is.null(normalized_counts$validation_message)) {
        showNotification(normalized_counts$validation_message, type = "error")
        return()
      }

      if (mixed_age_text != "") {
        parsed_age_groups <- parse_age_groups_text(mixed_age_text)
        parsed_age_groups <- complete_age_groups_with_primary(
          parsed_age_groups,
          normalized_counts$final_report_total_embryos,
          primary_age_label = primary_age_label,
          primary_age_value = normalized_primary_age$numeric
        )
        if (nrow(parsed_age_groups) > 0) {
          age_groups_json <- jsonlite::toJSON(parsed_age_groups, auto_unbox = TRUE, dataframe = "rows", null = "null")
          mixed_age_flag <- TRUE
        }
      }

      report_details$final_report_date <- as.character(input$edit_final_report_date)
      report_details$final_report_primary_age <- primary_age_label
      report_details$final_report_primary_age_value <- if (is.na(normalized_primary_age$numeric)) NA_real_ else normalized_primary_age$numeric
      report_details$final_report_total_embryos <- normalized_counts$final_report_total_embryos
      report_details$final_report_male_embryos <- normalized_counts$final_report_male_embryos
      report_details$final_report_female_embryos <- normalized_counts$final_report_female_embryos
      report_details$final_report_unknown_embryos <- normalized_counts$final_report_unknown_embryos
      report_details$final_report_mixed_age <- mixed_age_flag
      report_details$final_report_age_groups_json <- age_groups_json
      report_details$final_report_notes <- normalize_text_scalar(input$edit_final_report_notes)
    }

    update_data <- list(
      male_id = input$edit_plugging_male,
      female_id = input$edit_plugging_female,
      pairing_start_date = pairing_start_date_value,
      pairing_end_date = pairing_end_date_value,
      plug_observed_date = plug_observed_date_value,
      plugging_status = input$edit_plugging_status,
      expected_age_for_harvesting = input$edit_expected_age_for_harvesting,
      notes = input$edit_plugging_notes,
      final_report_date = report_details$final_report_date,
      final_report_primary_age = report_details$final_report_primary_age,
      final_report_primary_age_value = report_details$final_report_primary_age_value,
      final_report_total_embryos = report_details$final_report_total_embryos,
      final_report_male_embryos = report_details$final_report_male_embryos,
      final_report_female_embryos = report_details$final_report_female_embryos,
      final_report_unknown_embryos = report_details$final_report_unknown_embryos,
      final_report_mixed_age = report_details$final_report_mixed_age,
      final_report_age_groups_json = report_details$final_report_age_groups_json,
      final_report_notes = report_details$final_report_notes
    )

    if (id_changed) {
      plugging_state$pending_edit_data <- update_data
      
      # Build a more informative warning message
      changes <- list()
      if (original_record$female_id[1] != input$edit_plugging_female) {
        changes <- c(changes, list(tags$li(HTML(paste0("<b>Female ID:</b> ", original_record$female_id[1], " → ", input$edit_plugging_female)))))
      }
      if (original_record$male_id[1] != input$edit_plugging_male) {
        changes <- c(changes, list(tags$li(HTML(paste0("<b>Male ID:</b> ", original_record$male_id[1], " → ", input$edit_plugging_male)))))
      }
      
      warning_message <- tagList(
        p("You are about to change the mouse ID(s) for this record. This is a significant change. Please confirm the changes below:"),
        tags$ul(changes),
        p("Are you sure you want to proceed?")
      )
      
      showModal(modalDialog(
        title = "Confirm Mouse ID Change",
        warning_message,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_id_change_and_save_btn", "Confirm and Save", class = "btn-danger")
        )
      ))
    } else {
      perform_plugging_update(plugging_id, update_data)
    }
  })

  observeEvent(input$cancel_plugging_edit_btn, {
    removeModal()

    plugging_id <- plugging_state$editing_id
    plugging_state$editing_id <- NULL
    edit_modal_date_defaults(NULL)

    should_return_to_details <- isTRUE(return_to_details_after_edit())
    return_to_details_after_edit(TRUE)
    edit_embryo_autofill_enabled(TRUE)

    if (should_return_to_details && !is.null(plugging_id)) {
      plugging_state$viewing_id <- plugging_id
      show_plugging_details_modal(plugging_id)
    }
  }, ignoreInit = TRUE)

  # Save plugging edit - Step 2: Final confirmation after ID change warning
  observeEvent(input$confirm_id_change_and_save_btn, {
    plugging_id <- plugging_state$editing_id
    update_data <- plugging_state$pending_edit_data
    
    if (is.null(plugging_id) || is.null(update_data)) {
      showNotification("Could not save. Required information is missing.", type = "error")
      return()
    }
    
    perform_plugging_update(plugging_id, update_data)
    plugging_state$pending_edit_data <- NULL # Clean up
  })

  # Mark plug as observed
  observeEvent(input$mark_plug_observed_btn, {
    plugging_id <- plugging_state$viewing_id
    if (is.null(plugging_id)) return()
    
    # Get current notes for the modal
    con <- db_connect()
    current_notes <- ""
    tryCatch({
      current_record <- DBI::dbGetQuery(con, "SELECT notes FROM plugging_history WHERE id = ?", params = list(plugging_id))
      if (nrow(current_record) > 0) {
        current_notes <- ifelse(is.na(current_record$notes) || current_record$notes == "", "", current_record$notes)
      }
    }, finally = {
      db_disconnect(con)
    })
    
    showModal(modalDialog(
      title = "Mark Plug as Observed",
      size = "m",
      tagList(
        fluidRow(
          column(6,
            div(style = "margin-bottom: 0px;",
              tags$h5("Plug Observed Date"),
              radioButtons("plug_observed_type", NULL, 
                choices = c("Specific Date" = "date", "Unknown" = "unknown"),
                selected = "date"
              ),
              conditionalPanel(style = "margin-top: -10px;",
                condition = "input.plug_observed_type == 'date'",
                dateInput("plug_observed_date_input", "", value = Sys.Date(), width = "100%")
              )
            ),
            div(style = "margin-bottom: 12px;",
              tags$h5("Expected Age for Harvesting (E. days)"),
              textInput("expected_age_for_harvesting_input", NULL, value = "", width = "100%")
            )
          ),
          column(6,
            div(style = "margin-bottom: 12px;",
              tags$h5("Notes"),
              textAreaInput("plug_observed_notes_input", NULL, value = current_notes, rows = 5, width = "100%")
            ),
            div(style = "margin-top: 18px; color: #888; font-size: 0.98em;",
              tags$p("This will update the plugging status to 'Plugged' and set the plug observed date.")
            )
          )
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_plug_observed_btn", "Confirm", class = "btn-success")
      )
    ))
  })
  
  # Confirm plug observed
  observeEvent(input$confirm_plug_observed_btn, {
    plugging_id <- plugging_state$viewing_id
    if (is.null(plugging_id)) return()
    
    con <- db_connect()
    tryCatch({
      # Get current values for audit trail
      current <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      
      if (nrow(current) == 0) {
        showNotification("Plugging event not found", type = "error")
        return()
      }
      
      old_values <- current[1, ]
      
      # Determine plug observed date value based on radio button selection
      plug_observed_date_value <- if(input$plug_observed_type == "unknown") {
        "Unknown"
      } else {
        as.character(input$plug_observed_date_input)
      }
      
      # Auto-set pairing end date to match plug observed date for quick updates
      pairing_end_date_value <- plug_observed_date_value
      
      # Update the plugging event
      updated_notes <- normalize_plugging_notes_input(input$plug_observed_notes_input)
      result <- DBI::dbExecute(con, 
        "UPDATE plugging_history SET 
         plug_observed_date = ?,
         pairing_end_date = ?,
         plugging_status = 'Plugged',
         expected_age_for_harvesting = ?,
         notes = ?,
         updated_at = DATETIME('now')
         WHERE id = ?",
        params = list(
          plug_observed_date_value,
          pairing_end_date_value,
          input$expected_age_for_harvesting_input,
          updated_notes,
          plugging_id
        )
      )
      
      if (result > 0) {
        # Log to audit trail
        log_audit_trail(
          "plugging_history",
          plugging_id,
          "UPDATE",
          old_values,
          list(
            plug_observed_date = plug_observed_date_value,
            pairing_end_date = pairing_end_date_value,
            plugging_status = "Plugged",
            expected_age_for_harvesting = input$expected_age_for_harvesting_input,
            notes = input$plug_observed_notes_input
          )
        )
        
        showNotification("Plug marked as observed successfully!", type = "message")
        removeModal()
        plugging_state$viewing_id <- NULL
        # Save state before reload
        session$sendCustomMessage(type = "eval", message = "if(typeof saveDataTableState === 'function') saveDataTableState('plugging_history_table');")
        session$sendCustomMessage(type = "eval", message = "if(typeof saveScrollForAllTables === 'function') saveScrollForAllTables();")
        plugging_state$reload <- Sys.time()
        
        # Trigger global refresh for cross-module updates
        global_refresh_trigger(Sys.time())
        
        # Refresh all_mice_table if available
        if (!is.null(all_mice_table)) {
          con_refresh <- db_connect()
          tryCatch({
            all_data <- DBI::dbGetQuery(con_refresh, "SELECT * FROM mice_stock ORDER BY asu_id")
            all_mice_table(all_data)
          }, finally = {
            db_disconnect(con_refresh)
          })
        }
      } else {
        showNotification("Failed to update plugging event", type = "error")
      }
      
    }, error = function(e) {
      showNotification(paste("Error updating plugging event:", e$message), type = "error")
    }, finally = {
      db_disconnect(con)
    })
  })
  
 
  

  
  # Euthanize mice
  observeEvent(input$euthanize_mice_btn, {
    euthanasia_embryo_autofill_enabled(TRUE)
    plugging_id <- plugging_state$viewing_id
    if (is.null(plugging_id)) return()
    
    con <- db_connect()
    tryCatch({
      plugging <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      
      if (nrow(plugging) == 0) return()
      row <- plugging[1, ]
      report_defaults <- extract_plugging_final_report(plugging)
      
      # Get female mouse information
      female_info <- DBI::dbGetQuery(con, 
        "SELECT asu_id, dob, breeding_line, genotype FROM mice_stock WHERE asu_id = ?", 
        params = list(row$female_id))
      
      if (nrow(female_info) == 0) return()
      
      collection_date_default <- safe_analysis_date(report_defaults$final_report_date)
      if (is.na(collection_date_default) && "date_of_death" %in% names(female_info)) {
        collection_date_default <- safe_analysis_date(female_info$date_of_death[1])
      }
      if (is.na(collection_date_default)) {
        collection_date_default <- Sys.Date()
      }

      female_age_at_collection <- NA
      if (!is.na(female_info$dob[1]) && female_info$dob[1] != "") {
        female_dob <- safe_analysis_date(female_info$dob[1])
        if (!is.na(female_dob) && !is.na(collection_date_default)) {
          female_age_at_collection <- round(as.numeric(collection_date_default - female_dob) / 7, 1)
        }
      }
      
      initialize_euthanasia_other_age_rows(report_defaults)
      
      showModal(modalDialog(
        title = "Confirm Plug Status for Euthanasia",
        size = "l",
        tagList(
          tags$style(HTML("\
            .euthanasia-layout-row { display: flex; align-items: stretch; }\
            .euthanasia-left-col, .euthanasia-right-col { display: flex; flex-direction: column; }\
            .euthanasia-left-col { gap: 12px; }\
            .euthanasia-right-col .euthanasia-card { height: 100%; }\
            .euthanasia-card .form-group { margin-bottom: 10px; }\
            .euthanasia-collection-row { margin-bottom: 2px; }\
          ")),
          div(
            style = "background: #f8fafc; border: 1px solid #dbeafe; border-radius: 8px; padding: 10px 12px; margin-bottom: 12px; color: #1e3a5f;",
            tags$div(style = "font-weight: 600; margin-bottom: 4px;", "Female Mouse Summary"),
            tags$div(
              style = "display: flex; flex-wrap: wrap; gap: 10px 18px; font-size: 0.95em;",
              span(tags$b("ASU ID:"), female_info$asu_id),
              span(tags$b("Age at Collection/Death:"), ifelse(is.na(female_age_at_collection), "N/A", paste0(female_age_at_collection, " weeks"))),
              span(tags$b("Line:"), ifelse(is.na(female_info$breeding_line) || female_info$breeding_line == "", "N/A", female_info$breeding_line)),
              span(tags$b("Genotype:"), ifelse(is.na(female_info$genotype) || female_info$genotype == "", "N/A", female_info$genotype))
            )
          ),
          fluidRow(
            class = "euthanasia-layout-row",
            column(5,
              class = "euthanasia-left-col",
              div(
                class = "euthanasia-card",
                style = "background: #fff7ed; border: 1px solid #fed7aa; border-radius: 8px; padding: 10px 12px; flex: 1;",
                tags$div(style = "font-weight: 600; color: #9a3412; margin-bottom: 8px;", "Status Update"),
                dateInput("euthanasia_date_input", "Date of Death", value = collection_date_default),
                radioButtons("euthanasia_status_choice", "Plugging Status after Euthanasia:",
                  choices = c("Empty" = "Empty", "Sample Collected" = "Collected"),
                  selected = "Collected"
                )
              ),
              div(
                class = "euthanasia-card",
                style = "background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px; padding: 10px 12px; flex: 1;",
                tags$div(style = "font-weight: 600; color: #334155; margin-bottom: 8px;", "Plugging History Notes"),
                textAreaInput("euthanasia_notes_input", NULL, value = ifelse(is.na(plugging$notes) || plugging$notes == "", "", plugging$notes), rows = 3, width = "100%")
              )
            ),
            column(7,
              class = "euthanasia-right-col",
              conditionalPanel(
                condition = "input.euthanasia_status_choice == 'Collected'",
                div(
                  class = "euthanasia-card",
                  style = "background: #f0fdf4; border: 1px solid #bbf7d0; border-radius: 8px; padding: 10px 12px;",
                  tags$div(style = "font-weight: 600; color: #166534; margin-bottom: 8px;", "Sample Collection Report"),
                  fluidRow(
                    column(4,
                      numericInput(
                        "euthanasia_total_embryos_input",
                        "Total Embryos",
                        value = ifelse(is.na(report_defaults$final_report_total_embryos), NA, report_defaults$final_report_total_embryos),
                        min = 0,
                        step = 1
                      )
                    ),
                    column(8,
                      textInput(
                        "euthanasia_primary_embryo_age_input",
                        "Primary Embryo Stage",
                        value = ifelse(is.na(report_defaults$final_report_primary_age), "", report_defaults$final_report_primary_age),
                        placeholder = "E16 or E16.5"
                      )
                    )
                  ),
                  checkboxInput(
                    "euthanasia_mixed_embryo_ages_input",
                    "Mixed embryo ages in this collection",
                    value = isTRUE(report_defaults$final_report_mixed_age)
                  ),
                  tags$div(style = "border-top: 1px solid #cbd5e1; margin: 8px 0 6px 0;"),
                  tags$div(style = "font-weight: 600; color: #334155; margin-top: 6px; margin-bottom: 4px;", "Other Embryo Ages and Number"),
                  uiOutput("euthanasia_other_age_rows_ui"),
                  div(
                    style = "display: none;",
                    textAreaInput(
                      "euthanasia_embryo_age_groups_input",
                      "Age Group Details",
                      value = "",
                      rows = 2,
                      placeholder = "E15.5 x 1; E16.5 x 7"
                    )
                  ),
                  tags$div(style = "border-top: 1px solid #cbd5e1; margin: 8px 0 10px 0;"),
                  fluidRow(
                    column(4, numericInput("euthanasia_male_embryos_input", "Male", value = ifelse(is.na(report_defaults$final_report_male_embryos), NA, report_defaults$final_report_male_embryos), min = 0, step = 1)),
                    column(4, numericInput("euthanasia_female_embryos_input", "Female", value = ifelse(is.na(report_defaults$final_report_female_embryos), NA, report_defaults$final_report_female_embryos), min = 0, step = 1)),
                    column(4, numericInput("euthanasia_unknown_embryos_input", "Unknown", value = ifelse(is.na(report_defaults$final_report_unknown_embryos), NA, report_defaults$final_report_unknown_embryos), min = 0, step = 1))
                  ),
                  textAreaInput(
                    "euthanasia_collection_notes_input",
                    "Sample Collection Notes",
                    value = if (!is.null(report_defaults$final_report_notes) && !is.na(report_defaults$final_report_notes)) report_defaults$final_report_notes else "",
                    rows = 3,
                    width = "100%"
                  )
                )
              ),
              conditionalPanel(
                condition = "input.euthanasia_status_choice == 'Empty'",
                div(
                  class = "euthanasia-card",
                  style = "background: #f8fafc; border: 1px dashed #cbd5e1; border-radius: 8px; padding: 12px; color: #475569;",
                  "No collection report is needed when the final status is Empty."
                )
              )
            )
          ),
          div(
            style = "text-align: center; margin-top: 15px;",
            tags$p(tags$i("Are you sure you want to mark this female mouse as deceased? This action cannot be undone."))
          )
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_euthanasia_btn", "Confirm", class = "btn-danger")
        )
      ))
      
    }, finally = {
      db_disconnect(con)
    })
  })

  # Confirm euthanasia (updated logic)
  observeEvent(input$euthanasia_status_choice, {
    selected_status <- input$euthanasia_status_choice
    if (is.null(selected_status)) {
      return()
    }

    if (identical(selected_status, "Collected")) {
      updateActionButton(session, "confirm_euthanasia_btn", label = "Confirm Sample Collected")
    } else {
      updateActionButton(session, "confirm_euthanasia_btn", label = "Confirm Empty")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$confirm_euthanasia_btn, {
    plugging_id <- plugging_state$viewing_id
    if (is.null(plugging_id)) return()
    selected_status <- input$euthanasia_status_choice
    if (is.null(selected_status)) return()

    normalize_modal_text <- function(value) {
      if (is.null(value) || length(value) == 0 || is.na(value[1])) {
        return("")
      }

      trimws(as.character(value[1]))
    }
    
    con <- db_connect()
    tryCatch({
      plugging <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      if (nrow(plugging) == 0) return()
      row <- plugging[1, ]
      # Always update mouse to Deceased
      result <- DBI::dbExecute(con, 
        "UPDATE mice_stock SET \
         status = 'Deceased',\
         date_of_death = ?,\
         deceased_timestamp = DATETIME('now'),\
         last_updated = DATETIME('now')\
         WHERE asu_id = ?",
        params = list(
          as.character(input$euthanasia_date_input),
          row$female_id
        )
      )
      if (result > 0) {
        # Log to audit trail for female mouse
        audit_result <- log_audit_trail(
          "mice_stock",
          row$female_id,
          "UPDATE",
          list(status = "Alive"),
          list(
            status = "Deceased",
            date_of_death = as.character(input$euthanasia_date_input),
            notes = input$euthanasia_notes_input,
            source = "Plugging Tab",
            source_event_id = plugging_id
          )
        )
        # Update plugging status to selected value
        con2 <- db_connect()
        old_plug <- DBI::dbGetQuery(con2, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
        collection_notes_value <- normalize_modal_text(input$euthanasia_collection_notes_input)
        existing_report_details <- extract_plugging_final_report(old_plug)
        if (identical(selected_status, "Collected")) {
          primary_age_text <- normalize_modal_text(input$euthanasia_primary_embryo_age_input)
          normalized_primary_age <- normalize_embryo_age_input(primary_age_text)
          primary_age_label <- if (is.na(normalized_primary_age$numeric)) {
            if (primary_age_text == "") NA_character_ else primary_age_text
          } else {
            normalized_primary_age$label
          }
          normalized_counts <- normalize_final_report_embryo_counts(
            total_embryos = input$euthanasia_total_embryos_input,
            male_embryos = input$euthanasia_male_embryos_input,
            female_embryos = input$euthanasia_female_embryos_input,
            unknown_embryos = input$euthanasia_unknown_embryos_input
          )
          if (!is.null(normalized_counts$validation_message)) {
            db_disconnect(con2)
            showNotification(normalized_counts$validation_message, type = "error")
            return()
          }

          mixed_age_text_e <- normalize_modal_text(input$euthanasia_embryo_age_groups_input)
          mixed_age_flag_e <- isTRUE(input$euthanasia_mixed_embryo_ages_input)
          age_groups_json_e <- NA_character_
          if (mixed_age_text_e != "") {
            parsed_age_groups_e <- parse_age_groups_text(mixed_age_text_e)
            parsed_age_groups_e <- complete_age_groups_with_primary(
              parsed_age_groups_e,
              normalized_counts$final_report_total_embryos,
              primary_age_label = primary_age_label,
              primary_age_value = if (is.na(normalized_primary_age$numeric)) existing_report_details$final_report_primary_age_value else normalized_primary_age$numeric
            )
            if (nrow(parsed_age_groups_e) > 0) {
              age_groups_json_e <- jsonlite::toJSON(parsed_age_groups_e, auto_unbox = TRUE, dataframe = "rows", null = "null")
              mixed_age_flag_e <- TRUE
            }
          }

          DBI::dbExecute(con2, 
            "UPDATE plugging_history SET \
             plugging_status = ?,\
             updated_at = DATETIME('now'),\
             notes = ?,\
             final_report_date = ?,\
             final_report_primary_age = ?,\
             final_report_primary_age_value = ?,\
             final_report_total_embryos = ?,\
             final_report_male_embryos = ?,\
             final_report_female_embryos = ?,\
             final_report_unknown_embryos = ?,\
             final_report_mixed_age = ?,\
             final_report_age_groups_json = ?,\
             final_report_notes = ?\
             WHERE id = ?",
            params = list(
              selected_status,
              normalize_plugging_notes_input(input$euthanasia_notes_input),
              as.character(input$euthanasia_date_input),
              primary_age_label,
              if (is.na(normalized_primary_age$numeric)) existing_report_details$final_report_primary_age_value else normalized_primary_age$numeric,
              normalized_counts$final_report_total_embryos,
              normalized_counts$final_report_male_embryos,
              normalized_counts$final_report_female_embryos,
              normalized_counts$final_report_unknown_embryos,
              ifelse(isTRUE(mixed_age_flag_e), 1L, 0L),
              age_groups_json_e,
              collection_notes_value,
              plugging_id
            )
          )
        } else {
          DBI::dbExecute(con2, 
            "UPDATE plugging_history SET \
             plugging_status = ?,\
             updated_at = DATETIME('now'),\
             notes = ?\
             WHERE id = ?",
            params = list(
              selected_status,
              normalize_plugging_notes_input(input$euthanasia_notes_input),
              plugging_id
            )
          )
        }
        # Log to audit trail for plugging_history change
        plug_audit_result <- log_audit_trail(
          "plugging_history",
          plugging_id,
          "UPDATE",
          old_plug[1, ],
          list(
            plugging_status = selected_status,
            completion_date = as.character(input$euthanasia_date_input),
            notes = normalize_plugging_notes_input(input$euthanasia_notes_input),
            final_report_date = if (identical(selected_status, "Collected")) as.character(input$euthanasia_date_input) else NULL,
            final_report_primary_age = if (identical(selected_status, "Collected")) primary_age_label else NULL,
            final_report_total_embryos = if (identical(selected_status, "Collected")) normalized_counts$final_report_total_embryos else NULL,
            final_report_male_embryos = if (identical(selected_status, "Collected")) normalized_counts$final_report_male_embryos else NULL,
            final_report_female_embryos = if (identical(selected_status, "Collected")) normalized_counts$final_report_female_embryos else NULL,
            final_report_unknown_embryos = if (identical(selected_status, "Collected")) normalized_counts$final_report_unknown_embryos else NULL,
            final_report_notes = if (identical(selected_status, "Collected")) collection_notes_value else NULL
          )
        )
        db_disconnect(con2)
        showNotification("Female mouse marked as deceased and plugging status updated!", type = "message")
        removeModal()
        plugging_state$viewing_id <- NULL
        plugging_state$reload <- Sys.time()
        global_refresh_trigger(Sys.time())
        if (!is.null(all_mice_table)) {
          con_refresh <- db_connect()
          tryCatch({
            all_data <- DBI::dbGetQuery(con_refresh, "SELECT * FROM mice_stock ORDER BY asu_id")
            all_mice_table(all_data)
          }, finally = {
            db_disconnect(con_refresh)
          })
        }
      } else {
        showNotification("Failed to update female mouse status", type = "error")
      }
    }, error = function(e) {
      showNotification(paste("Error updating mouse status:", e$message), type = "error")
    }, finally = {
      db_disconnect(con)
    })
  })
  
  # Show calendar modal when Event Calendar button is clicked
  observeEvent(input$show_calendar_btn, {
    show_plugging_calendar_modal()
  })

  # Delete plugging record: show confirmation modal
  observeEvent(input$delete_plugging_btn, {
    showModal(modalDialog(
      title = "Delete Plugging Record",
      "Are you sure you want to delete this plugging record? This action cannot be undone.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_plugging_btn", "Delete", class = "btn-danger")
      )
    ))
  })

  # Confirm delete plugging record
  observeEvent(input$confirm_delete_plugging_btn, {
    plugging_id <- plugging_state$viewing_id
    if (is.null(plugging_id)) return()
    con <- db_connect()
    tryCatch({
      # Get old record for audit
      old_row <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      if (nrow(old_row) == 0) return()
      # Set plugging_status to 'Deleted'
      DBI::dbExecute(con, "UPDATE plugging_history SET plugging_status = 'Deleted' WHERE id = ?", params = list(plugging_id))
      # Log plugging_status change to Deleted
      log_audit_trail(
        "plugging_history",
        plugging_id,
        "UPDATE",
        as.list(old_row[1, ]),
        modifyList(as.list(old_row[1, ]), list(plugging_status = 'Deleted'))
      )
      showNotification("Plugging record marked as deleted.", type = "message")
      removeModal()
      plugging_state$viewing_id <- NULL
      plugging_state$reload <- Sys.time()
    }, finally = {
      db_disconnect(con)
    })
  })
  
  # Quick confirm plugging status
  observeEvent(input$quick_confirm_plugging_btn, {
    plugging_id <- input$quick_confirm_plugging_btn
    if (is.null(plugging_id)) return()
    
    # Get current plugging record to determine available options
    con <- db_connect()
    current_record <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
    db_disconnect(con)
    
    if (nrow(current_record) == 0) {
      showNotification("Plugging record not found", type = "error")
      return()
    }
    
    current_status <- current_record$plugging_status[1]
    
    # Define available status options based on current status
    status_choices <- switch(current_status,
      "Ongoing" = c(
        "Plugged" = "Plugged",
        "Not Observed (Waiting for confirmation)" = "Not Observed (Waiting for confirmation)"
        #"Not Pregnant" = "Not Pregnant"
      ),
      "Plugged" = c(
        "Plug Confirmed" = "Plug Confirmed",
        "Not Pregnant" = "Not Pregnant",
        "Empty Plug! (Euthanized) 🔴" = "Empty",
        "Empty Plug (Alive) 🟢" = "Empty_Alive_UI",
        "Sample Collected" = "Collected"
      ),
      "Plug Confirmed" = c(
        "Not Pregnant" = "Not Pregnant",
        "Empty Plug! (Euthanized) 🔴" = "Empty",
        "Empty Plug (Alive) 🟢" = "Empty_Alive_UI",
        "Sample Collected" = "Collected"
      ),
      "Not Observed (Waiting for confirmation)" = c(
        "Not Observed (Confirmed)" = "Not Observed (Confirmed)",
        "Not Pregnant" = "Not Pregnant",
        "Plugged (Report Delayed)" = "Plugged",
        "Surprising Plug!! 😱" = "Surprising Plug!!",
        #"Empty Plug! (Euthanized) 🔴" = "Empty",
        "Sample Collected" = "Collected"
      ),
      "Not Observed (Confirmed)" = c(
        "Surprising Plug!! 😱" = "Surprising Plug!!",
        "Empty Plug! (Euthanized) 🔴" = "Empty",
        "Empty Plug (Alive) 🟢" = "Empty_Alive_UI",
        "Sample Collected" = "Collected"
      ),
      "Surprising Plug!!" = c(
        "Empty Plug! (Euthanized) 🔴" = "Empty",
        "Empty Plug (Alive) 🟢" = "Empty_Alive_UI",
        "Sample Collected" = "Collected"
      ),
      # Default options for any other active status
      c(
        "Plug Confirmed" = "Plug Confirmed",
        "Not Pregnant" = "Not Pregnant",
        "Surprising Plug!!" = "Surprising Plug!!",
        "Not Observed (Confirmed)" = "Not Observed (Confirmed)",
        "Empty Plug! (Euthanized) 🔴" = "Empty",
        "Empty Plug (Alive) 🟢" = "Empty_Alive_UI",
        "Sample Collected" = "Collected"
      )
    )
    
    # Get current notes, expected age, and plug observed date for the modal
    con <- db_connect()
    current_notes <- ""
    current_expected_age <- ""
    current_plug_observed_date <- ""
    tryCatch({
      current_record <- DBI::dbGetQuery(con, "SELECT notes, expected_age_for_harvesting, plug_observed_date FROM plugging_history WHERE id = ?", params = list(plugging_id))
      if (nrow(current_record) > 0) {
        current_notes <- ifelse(is.na(current_record$notes) || current_record$notes == "", "", current_record$notes)
        current_expected_age <- ifelse(is.na(current_record$expected_age_for_harvesting) || current_record$expected_age_for_harvesting == "", "", current_record$expected_age_for_harvesting)
        current_plug_observed_date <- ifelse(is.na(current_record$plug_observed_date) || current_record$plug_observed_date == "", "", current_record$plug_observed_date)
      }
    }, finally = {
      db_disconnect(con)
    })
    
    # Determine default radio selection for plug observed date
    plug_observed_type_default <- if (current_plug_observed_date == "Unknown" || current_plug_observed_date == "") "unknown" else "date"
    plug_observed_date_value <- if (plug_observed_type_default == "date") current_plug_observed_date else as.character(Sys.Date())
    
    # Show confirmation modal with status options
    showModal(modalDialog(
      title = paste("Update Plugging Status - Current:", current_status),
      size = "l",
      tagList(
        div(
          style = "background: linear-gradient(135deg, rgba(37, 99, 235, 0.08) 0%, rgba(245, 158, 11, 0.12) 100%); border: 1px solid #dbeafe; border-radius: 10px; padding: 12px 14px; margin-bottom: 12px;",
          tags$div(style = "font-weight: 700; color: #1e3a5f; margin-bottom: 4px;", "Quick Status Update"),
          "Choose a new status below. If you pick Empty or Sample Collected, you will continue to the detailed euthanasia/collection report form."
        ),
        fluidRow(
          column(
            6,
            div(
              style = "background: #ffffff; border: 1px solid #e2e8f0; border-radius: 8px; padding: 10px 12px;",
              radioButtons("confirm_status_choice", "Status Options:",
                choices = status_choices,
                selected = names(status_choices)[1],
                width = NULL,
                inline = FALSE
              )
            )
          ),
          column(
            6,
            div(
              style = "background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px; padding: 10px 12px;",
              textAreaInput("quick_status_notes_input", "Notes", value = current_notes, rows = 4, width = "100%")
            )
          )
        ),
        conditionalPanel(
          condition = "input.confirm_status_choice == 'Plugged'",
          div(
            style = "margin-top: 10px; background: #eff6ff; border: 1px solid #bfdbfe; border-radius: 8px; padding: 10px 12px;",
            radioButtons("confirm_plug_observed_type", "Plug Observed Date Type",
              choices = c("Specific Date" = "date", "Unknown" = "unknown"),
              selected = plug_observed_type_default
            ),
            conditionalPanel(
              condition = "input.confirm_plug_observed_type == 'date'",
              dateInput("confirm_plug_observed_date", "Plug Observed Date", value = plug_observed_date_value, width = "100%")
            ),
            textInput("confirm_expected_age_for_harvesting", "Expected Age for Harvesting (Embryonic Days, e.g. 14)", value = current_expected_age, width = "100%")
          )
        ),
        div(
          style = "font-size: 12px; color: #555; margin-top: 10px;",
          tags$em(
            tags$strong("Not Pregnant:"), " False pregnant without Euthanizing.", tags$br(),
            tags$strong("Empty Plug:"), " Euthanized without embryos", tags$br(),
            tags$strong("Sample Collected:"), " Euthanized with embryos."
          )
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_status_btn", "Continue", class = "btn-primary")
      )
    ))
    
    # Store the plugging_id for use in the confirmation
    plugging_state$confirming_id <- plugging_id
  })
  
  # Handle the status confirmation
  observeEvent(input$confirm_status_btn, {
    plugging_id <- plugging_state$confirming_id
    selected_status <- input$confirm_status_choice
    
    if (is.null(plugging_id) || is.null(selected_status)) {
      showNotification("Missing information for confirmation", type = "error")
      return()
    }
    
    # Helper to open the correct modal for statuses that require extra info
    open_status_detail_modal <- function(status) {
      plugging_id <- plugging_state$confirming_id
      con <- db_connect()
      plugging_row <- NULL
      female_info <- NULL
      female_age <- NA
      female_age_at_collection <- NA
      if (!is.null(plugging_id)) {
        plugging_row <- tryCatch({
          DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
        }, error = function(e) NULL)
        if (!is.null(plugging_row) && nrow(plugging_row) > 0) {
          female_id <- plugging_row$female_id[1]
          female_info <- tryCatch({
            DBI::dbGetQuery(con, "SELECT * FROM mice_stock WHERE asu_id = ?", params = list(female_id))
          }, error = function(e) NULL)
          if (!is.null(female_info) && nrow(female_info) > 0) {
            dob <- female_info$dob[1]
            if (!is.na(dob) && dob != "") {
              female_age <- floor(as.numeric(Sys.Date() - as.Date(dob)) / 7)
            }
          }
        }
      }
      db_disconnect(con)
      if (status == "Plugged") {
        # Get current notes for the modal
        current_notes <- ""
        if (!is.null(plugging_row) && nrow(plugging_row) > 0) {
          current_notes <- ifelse(is.na(plugging_row$notes) || plugging_row$notes == "", "", plugging_row$notes)
        }
        
        showModal(modalDialog(
          title = "Mark Plug as Observed",
          size = "m",
          tagList(
            fluidRow(
              column(6,
                div(style = "margin-bottom: 12px;",
                  tags$h5("Plug Observed Date"),
                  radioButtons("plug_observed_type", NULL, 
                    choices = c("Specific Date" = "date", "Unknown" = "unknown"),
                    selected = "date"
                  ),
                  conditionalPanel(style = "margin-top: -10px;",
                    condition = "input.plug_observed_type == 'date'",
                    dateInput("plug_observed_date_input", "", value = Sys.Date(), width = "100%")
                  )
                ),
                div(style = "margin-bottom: 12px;",
                  tags$h5("Expected Age for Harvesting (Embryonic Days)"),
                  textInput("expected_age_for_harvesting_input", NULL, value = "", width = "100%")
                )
              ),
              column(6,
                div(style = "margin-bottom: 12px;",
                  tags$h5("Notes"),
                  textAreaInput("plug_observed_notes_input", NULL, value = current_notes, rows = 5, width = "100%")
                ),
                div(style = "margin-top: 18px; color: #888; font-size: 0.98em;",
                  tags$p("This will update the plugging status to 'Plugged' and set the plug observed date.")
                )
              )
            )
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirm_plug_observed_btn", "Confirm", class = "btn-success")
          )
        ))
      } else if (status == "Surprising Plug!!") {
        # Get current notes for the modal
        current_notes <- ""
        if (!is.null(plugging_row) && nrow(plugging_row) > 0) {
          current_notes <- ifelse(is.na(plugging_row$notes) || plugging_row$notes == "", "", plugging_row$notes)
        }
        
        showModal(modalDialog(
          title = "Mark as Surprising Plug!!😱😮‍💨",
          size = "m",
          tagList(
            div(
              style = "text-align: center; padding: 20px;",
              tags$p("This will set the plug observed date to 'Unknown' and estimate the plug date using the pairing start date for calendar purposes."),
              br(),
              textInput("expected_age_for_harvesting_surprising_input_quick", "Expected Age for Harvesting (Embryonic Days, e.g. 14)", value = "", width = "100%"),
              br(),
              textAreaInput("surprising_plug_notes_input_quick", "Notes", value = current_notes, rows = 3, width = "100%"),
              br(),
              tags$p("This will update the plugging status to 'Surprising Plug!!' and set the plug observed date to 'Unknown'.")
            )
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirm_surprising_plug_btn_quick", "Confirm Surprising Plug!!", class = "btn-success")
          )
        ))
      } else if (status == "Empty_Alive_UI") {
        # Get current notes for the modal
        current_notes <- ""
        if (!is.null(plugging_row) && nrow(plugging_row) > 0) {
          current_notes <- ifelse(is.na(plugging_row$notes) || plugging_row$notes == "", "", plugging_row$notes)
        }
        
        showModal(show_empty_alive_modal(
          female_info, female_age, Sys.Date(), current_notes, 
          "quick_empty_alive_date_input", "confirm_quick_empty_alive_btn"
        ))
      } else if (status %in% c("Empty", "Collected")) {
        # Get current notes for the modal
        current_notes <- ""
        if (!is.null(plugging_row) && nrow(plugging_row) > 0) {
          current_notes <- ifelse(is.na(plugging_row$notes) || plugging_row$notes == "", "", plugging_row$notes)
        }
        report_defaults <- extract_plugging_final_report(plugging_row)
        initialize_quick_other_age_rows(report_defaults)
        mixed_age_text <- format_age_groups_text_from_json(report_defaults$final_report_age_groups_json)

        collection_date_default <- safe_analysis_date(report_defaults$final_report_date)
        if (is.na(collection_date_default) && !is.null(female_info) && nrow(female_info) > 0 && "date_of_death" %in% names(female_info)) {
          collection_date_default <- safe_analysis_date(female_info$date_of_death[1])
        }
        if (is.na(collection_date_default)) {
          collection_date_default <- Sys.Date()
        }
        if (!is.null(female_info) && nrow(female_info) > 0 && !is.na(female_info$dob[1]) && female_info$dob[1] != "") {
          female_dob <- safe_analysis_date(female_info$dob[1])
          if (!is.na(female_dob) && !is.na(collection_date_default)) {
            female_age_at_collection <- floor(as.numeric(collection_date_default - female_dob) / 7)
          }
        }
        
        showModal(modalDialog(
          title = ifelse(status == "Empty", "Confirm Set Status to Empty (Euthanized)", "Confirm Sample Collected (Euthanized)"),
          size = ifelse(status == "Collected", "l", "m"),
          tagList(
            tags$style(HTML("\
              .quick-eu-layout-row { display: flex; align-items: stretch; }\
              .quick-eu-left-col, .quick-eu-right-col { display: flex; flex-direction: column; }\
              .quick-eu-left-col { gap: 12px; }\
              .quick-eu-card .form-group { margin-bottom: 10px; }\
            ")),
            div(
              style = "background: #f8fafc; border: 1px solid #dbeafe; border-radius: 8px; padding: 10px 12px; margin-bottom: 12px; color: #1e3a5f;",
              tags$div(style = "font-weight: 600; margin-bottom: 4px;", "Female Mouse Summary"),
              tags$div(
                style = "display: flex; flex-wrap: wrap; gap: 10px 18px; font-size: 0.95em;",
                span(tags$b("ASU ID:"), if (!is.null(female_info) && nrow(female_info) > 0) female_info$asu_id[1] else "N/A"),
                span(tags$b("Age at Collection/Death:"), if (!is.na(female_age_at_collection)) paste0(female_age_at_collection, " weeks") else "N/A"),
                span(tags$b("Line:"), if (!is.null(female_info) && nrow(female_info) > 0 && !is.na(female_info$breeding_line[1]) && female_info$breeding_line[1] != "") female_info$breeding_line[1] else "N/A"),
                span(tags$b("Genotype:"), if (!is.null(female_info) && nrow(female_info) > 0 && !is.na(female_info$genotype[1]) && female_info$genotype[1] != "") female_info$genotype[1] else "N/A")
              )
            ),
            if (status == "Collected") {
              tagList(
                fluidRow(
                  class = "quick-eu-layout-row",
                  column(5,
                    class = "quick-eu-left-col",
                    div(
                      class = "quick-eu-card",
                      style = "background: #fff7ed; border: 1px solid #fed7aa; border-radius: 8px; padding: 10px 12px; flex: 1;",
                      tags$div(style = "font-weight: 600; color: #9a3412; margin-bottom: 8px;", "Status Update"),
                      tags$div(style = "color: #7c2d12; font-size: 0.92em; margin-bottom: 8px;", "This record will be saved as Sample Collected."),
                      dateInput("quick_euthanasia_date_input", "Date of Death", value = collection_date_default)
                    ),
                    div(
                      class = "quick-eu-card",
                      style = "background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px; padding: 10px 12px; flex: 1;",
                      tags$div(style = "font-weight: 600; color: #334155; margin-bottom: 8px;", "Sample Collection Notes"),
                      textAreaInput(
                        "quick_euthanasia_notes_input",
                        NULL,
                        value = if (!is.null(report_defaults$final_report_notes) && !is.na(report_defaults$final_report_notes) && report_defaults$final_report_notes != "") report_defaults$final_report_notes else "",
                        rows = 4,
                        width = "100%"
                      )
                    )
                  ),
                  column(7,
                    class = "quick-eu-right-col",
                    div(
                      class = "quick-eu-card",
                      style = "background: #f0fdf4; border: 1px solid #bbf7d0; border-radius: 8px; padding: 10px 12px; flex: 1;",
                      tags$div(style = "font-weight: 600; color: #166534; margin-bottom: 8px;", "Sample Collection Report"),
                      fluidRow(
                        column(4,
                          numericInput(
                            "quick_total_embryos_input",
                            "Total Embryos",
                            value = ifelse(is.na(report_defaults$final_report_total_embryos), NA, report_defaults$final_report_total_embryos),
                            min = 0, step = 1
                          )
                        ),
                        column(8,
                          textInput(
                            "quick_primary_embryo_age_input",
                            "Primary Embryo Age",
                            value = ifelse(is.na(report_defaults$final_report_primary_age), "", report_defaults$final_report_primary_age),
                            placeholder = "E16 or E16.5"
                          )
                        )
                      ),
                      checkboxInput(
                        "quick_mixed_embryo_ages_input",
                        "Mixed embryo ages in this collection",
                        value = isTRUE(report_defaults$final_report_mixed_age)
                      ),
                      tags$div(style = "border-top: 1px solid #cbd5e1; margin: 8px 0 6px 0;"),
                      tags$div(style = "font-weight: 600; color: #334155; margin-top: 6px; margin-bottom: 4px;", "Other Embryo Ages and Number"),
                      uiOutput("quick_other_age_rows_ui"),
                      div(
                        style = "display: none;",
                        textAreaInput(
                          "quick_embryo_age_groups_input",
                          "Age Group Details",
                          value = "",
                          rows = 2,
                          placeholder = "E15.5 x 1; E16.5 x 7"
                        )
                      ),
                      tags$div(style = "border-top: 1px solid #cbd5e1; margin: 8px 0 10px 0;"),
                      fluidRow(
                        column(4, numericInput("quick_male_embryos_input", "Male", value = ifelse(is.na(report_defaults$final_report_male_embryos), NA, report_defaults$final_report_male_embryos), min = 0, step = 1)),
                        column(4, numericInput("quick_female_embryos_input", "Female", value = ifelse(is.na(report_defaults$final_report_female_embryos), NA, report_defaults$final_report_female_embryos), min = 0, step = 1)),
                        column(4, numericInput("quick_unknown_embryos_input", "Unknown", value = ifelse(is.na(report_defaults$final_report_unknown_embryos), NA, report_defaults$final_report_unknown_embryos), min = 0, step = 1))
                      )
                    )
                  )
                ),
                div(
                  style = "text-align: center; margin-top: 15px;",
                  tags$p(tags$i("Are you sure you want to mark this female mouse as deceased? This action cannot be undone."))
                )
              )
            } else {
              tagList(
                div(
                  style = "background: #fff7ed; border: 1px solid #fed7aa; border-radius: 8px; padding: 10px 12px; margin-bottom: 12px;",
                  tags$div(style = "font-weight: 600; color: #9a3412; margin-bottom: 8px;", "Status Update"),
                  dateInput("quick_euthanasia_date_input", "Date of Death", value = Sys.Date())
                ),
                div(
                  style = "text-align: center; margin-top: 15px;",
                  tags$p(tags$i("Are you sure you want to mark this female mouse as deceased? This action cannot be undone."))
                )
              )
            }
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirm_quick_euthanasia_btn", "Confirm", class = "btn-danger")
          )
        ))
      }
    }

    # Determine if the selected status requires extra info
    if (selected_status %in% c("Plugged", "Surprising Plug!!", "Euthanized", "Empty", "Empty_Alive_UI", "Collected")) {
      # For Plugged, if coming from quick confirm, update expected_age_for_harvesting and plug_observed_date as well
      if (selected_status == "Plugged" && (!is.null(input$confirm_expected_age_for_harvesting) || !is.null(input$confirm_plug_observed_type))) {
        con <- db_connect()
        tryCatch({
          current <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
          if (nrow(current) == 0) {
            showNotification("Plugging event not found", type = "error")
            return()
          }
          old_values <- current[1, ]
          notes_input <- if (is.null(input$quick_status_notes_input) || length(input$quick_status_notes_input) == 0) {
            ""
          } else {
            normalize_plugging_notes_input(input$quick_status_notes_input)
          }
          expected_age <- input$confirm_expected_age_for_harvesting
          plug_observed_date_value <- if (input$confirm_plug_observed_type == "unknown") {
            "Unknown"
          } else {
            as.character(input$confirm_plug_observed_date)
          }
          
          # Auto-set pairing end date to match plug observed date for quick updates
          pairing_end_date_value <- plug_observed_date_value
          
          result <- DBI::dbExecute(con, 
            "UPDATE plugging_history SET plugging_status = ?, expected_age_for_harvesting = ?, plug_observed_date = ?, pairing_end_date = ?, updated_at = DATETIME('now'), notes = ? WHERE id = ?",
            params = list(
              selected_status,
              expected_age,
              plug_observed_date_value,
              pairing_end_date_value,
              notes_input,
              plugging_id
            )
          )
          log_audit_trail(
            "plugging_history",
            plugging_id,
            "UPDATE",
            old_values,
            list(
              plugging_status = selected_status,
              expected_age_for_harvesting = expected_age,
              plug_observed_date = plug_observed_date_value,
              pairing_end_date = pairing_end_date_value,
              confirmation_date = as.character(Sys.Date()),
              notes = notes_input
            )
          )
          showNotification(paste("Plugging status updated to '", selected_status, "' successfully!", sep = ""), type = "message")
          removeModal()
          plugging_state$confirming_id <- NULL
          Sys.sleep(1)
          auto_update_plugging_status_to_unknown()
          plugging_state$reload <- Sys.time()
          if (!is.null(global_refresh_trigger)) {
            global_refresh_trigger(Sys.time())
          }
          invalidateLater(100, session)
        }, error = function(e) {
          showNotification(paste("Error updating plugging status:", e$message), type = "error")
        }, finally = {
          db_disconnect(con)
        })
        return()
      }
      open_status_detail_modal(selected_status)
      return()
    }

    # For other statuses, update immediately as before
    con <- db_connect()
    tryCatch({
      current <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      if (nrow(current) == 0) {
        showNotification("Plugging event not found", type = "error")
        return()
      }
      old_values <- current[1, ]
      # Handle notes input safely
      notes_input <- if (is.null(input$quick_status_notes_input) || length(input$quick_status_notes_input) == 0) {
        ""
      } else {
        normalize_plugging_notes_input(input$quick_status_notes_input)
      }
      
      # Auto-set plug_observed_date to "Unknown" for specific statuses in quick updates
      statuses_requiring_unknown_plug_date <- c("Ongoing", "Not Observed (Confirmed)", "Not Observed (Waiting for confirmation)", "Not Pregnant")
      
      if (selected_status %in% statuses_requiring_unknown_plug_date) {
        # For these statuses, only set plug_observed_date to Unknown (pairing_end_date unchanged)
        update_query <- "UPDATE plugging_history SET plugging_status = ?, plug_observed_date = 'Unknown', updated_at = DATETIME('now'), notes = ? WHERE id = ?"
        update_params <- list(
          selected_status,
          notes_input,
          plugging_id
        )
      } else {
        update_query <- "UPDATE plugging_history SET plugging_status = ?, updated_at = DATETIME('now'), notes = ? WHERE id = ?"
        update_params <- list(
          selected_status,
          notes_input,
          plugging_id
        )
      }
      result <- DBI::dbExecute(con, update_query, params = update_params)
      if (result > 0) {
        # If status is set to Collected, also set female mouse to Deceased
        if (selected_status == "Collected") {
          female_id <- old_values$female_id
          DBI::dbExecute(con, 
            "UPDATE mice_stock SET status = 'Deceased', date_of_death = ?, deceased_timestamp = DATETIME('now'), last_updated = DATETIME('now') WHERE asu_id = ? AND status != 'Deceased'",
            params = list(as.character(Sys.Date()), female_id)
          )
        }
        # Build audit trail data based on what was actually updated
        audit_data <- list(
          plugging_status = selected_status,
          confirmation_date = as.character(Sys.Date()),
          notes = notes_input
        )
        
        # Add plug_observed_date to audit if it was auto-set to Unknown
        if (selected_status %in% statuses_requiring_unknown_plug_date) {
          audit_data$plug_observed_date <- "Unknown"
        }
        
        log_audit_trail(
          "plugging_history",
          plugging_id,
          "UPDATE",
          old_values,
          audit_data
        )
        showNotification(paste("Plugging status updated to '", selected_status, "' successfully!", sep = ""), type = "message")
        removeModal()
        plugging_state$confirming_id <- NULL
        Sys.sleep(1)
        auto_update_plugging_status_to_unknown()
        plugging_state$reload <- Sys.time()
        if (!is.null(global_refresh_trigger)) {
          global_refresh_trigger(Sys.time())
        }
        invalidateLater(100, session)
      } else {
        showNotification("Failed to update plugging status", type = "error")
      }
    }, error = function(e) {
      showNotification(paste("Error updating plugging status:", e$message), type = "error")
    }, finally = {
      db_disconnect(con)
    })
  })

  observeEvent(input$confirm_status_choice, {
    selected_status <- input$confirm_status_choice
    if (is.null(selected_status)) {
      return()
    }

    if (identical(selected_status, "Collected")) {
      updateActionButton(session, "confirm_status_btn", label = "Open Sample Collection Form")
    } else if (identical(selected_status, "Empty")) {
      updateActionButton(session, "confirm_status_btn", label = "Open Empty Confirmation")
    } else {
      updateActionButton(session, "confirm_status_btn", label = "Update Status")
    }
  }, ignoreInit = TRUE)

  show_collection_report_modal <- function(plugging_id) {
    if (is.null(plugging_id)) return()

    write_collection_debug_log("open_modal_request", list(plugging_id = plugging_id))

    con <- db_connect()
    tryCatch({
      plugging_row <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      if (nrow(plugging_row) == 0) return()

      female_info <- DBI::dbGetQuery(con, "SELECT * FROM mice_stock WHERE asu_id = ?", params = list(plugging_row$female_id[1]))

      current_notes <- ifelse(is.na(plugging_row$notes[1]) || plugging_row$notes[1] == "", "", plugging_row$notes[1])
      report_defaults <- extract_plugging_final_report(plugging_row)
      initialize_quick_other_age_rows(report_defaults)
      mixed_age_text <- format_age_groups_text_from_json(report_defaults$final_report_age_groups_json)

      collection_date_default <- safe_analysis_date(report_defaults$final_report_date)
      if (is.na(collection_date_default) && nrow(female_info) > 0 && "date_of_death" %in% names(female_info)) {
        collection_date_default <- safe_analysis_date(female_info$date_of_death[1])
      }
      if (is.na(collection_date_default)) {
        collection_date_default <- Sys.Date()
      }

      female_age_at_collection <- NA
      if (nrow(female_info) > 0 && !is.na(female_info$dob[1]) && female_info$dob[1] != "") {
        female_dob <- safe_analysis_date(female_info$dob[1])
        if (!is.na(female_dob) && !is.na(collection_date_default)) {
          female_age_at_collection <- floor(as.numeric(collection_date_default - female_dob) / 7)
        }
      }

      plugging_state$confirming_id <- plugging_id
  write_collection_debug_log("open_modal_ready", list(plugging_id = plugging_id, female_id = plugging_row$female_id[1]))
      showModal(modalDialog(
        title = "Review Sample Collected Report",
        size = "l",
        tagList(
          tags$input(id = "confirm_status_choice", type = "hidden", value = "Collected"),
          tags$style(HTML("\
            .review-collection-layout-row { display: flex; align-items: stretch; }\
            .review-collection-left-col, .review-collection-right-col { display: flex; flex-direction: column; }\
            .review-collection-left-col { gap: 12px; }\
            .review-collection-right-col .review-collection-card { height: 100%; }\
            .review-collection-card .form-group { margin-bottom: 10px; }\
            .review-collection-row { margin-bottom: 2px; }\
          ")),
          div(
            style = "background: #f8fafc; border: 1px solid #dbeafe; border-radius: 8px; padding: 10px 12px; margin-bottom: 12px; color: #1e3a5f;",
            tags$div(style = "font-weight: 600; margin-bottom: 4px;", "Female Mouse Summary"),
            tags$div(
              style = "display: flex; flex-wrap: wrap; gap: 10px 18px; font-size: 0.95em;",
              span(tags$b("ASU ID:"), if (nrow(female_info) > 0) female_info$asu_id[1] else "N/A"),
              span(tags$b("Age at Collection/Death:"), ifelse(is.na(female_age_at_collection), "N/A", paste0(female_age_at_collection, " weeks"))),
              span(tags$b("Line:"), if (nrow(female_info) > 0 && !is.na(female_info$breeding_line[1]) && female_info$breeding_line[1] != "") female_info$breeding_line[1] else "N/A"),
              span(tags$b("Genotype:"), if (nrow(female_info) > 0 && !is.na(female_info$genotype[1]) && female_info$genotype[1] != "") female_info$genotype[1] else "N/A")
            )
          ),
          fluidRow(
            class = "review-collection-layout-row",
            column(5,
              class = "review-collection-left-col",
              div(
                class = "review-collection-card",
                style = "background: #fff7ed; border: 1px solid #fed7aa; border-radius: 8px; padding: 10px 12px; flex: 1;",
                tags$div(style = "font-weight: 600; color: #9a3412; margin-bottom: 8px;", "Status Update"),
                tags$div(style = "color: #7c2d12; font-size: 0.92em; margin-bottom: 8px;", "This record will be saved as Sample Collected."),
                dateInput("quick_euthanasia_date_input", "Date of Death", value = collection_date_default)
              ),
              div(
                class = "review-collection-card",
                style = "background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px; padding: 10px 12px; flex: 1;",
                tags$div(style = "font-weight: 600; color: #334155; margin-bottom: 8px;", "Sample Collection Notes"),
                textAreaInput(
                  "quick_euthanasia_notes_input",
                  NULL,
                  value = if (!is.null(report_defaults$final_report_notes) && !is.na(report_defaults$final_report_notes) && report_defaults$final_report_notes != "") report_defaults$final_report_notes else "",
                  rows = 4,
                  width = "100%"
                )
              )
            ),
            column(7,
              class = "review-collection-right-col",
              div(
                class = "review-collection-card",
                style = "background: #f0fdf4; border: 1px solid #bbf7d0; border-radius: 8px; padding: 10px 12px;",
                tags$div(style = "font-weight: 600; color: #166534; margin-bottom: 8px;", "Sample Collection Report"),
                div(
                  class = "review-collection-row",
                  fluidRow(
                    column(4,
                      numericInput(
                        "quick_total_embryos_input",
                        "Total Embryos",
                        value = ifelse(is.na(report_defaults$final_report_total_embryos), NA, report_defaults$final_report_total_embryos),
                        min = 0,
                        step = 1
                      )
                    ),
                    column(8,
                      textInput(
                        "quick_primary_embryo_age_input",
                        "Primary Embryo Age",
                        value = ifelse(is.na(report_defaults$final_report_primary_age), "", report_defaults$final_report_primary_age),
                        placeholder = "E16 or E16.5"
                      )
                    )
                  )
                ),
                div(
                  class = "review-collection-row",
                  checkboxInput(
                    "quick_mixed_embryo_ages_input",
                    "Mixed embryo ages in this collection",
                    value = isTRUE(report_defaults$final_report_mixed_age)
                  )
                ),
                div(
                  class = "review-collection-row",
                  tags$div(style = "border-top: 1px solid #cbd5e1; margin: 8px 0 6px 0;"),
                  tags$div(style = "font-weight: 600; color: #334155; margin-top: 6px; margin-bottom: 4px;", "Other Embryo Ages and Number"),
                  uiOutput("quick_other_age_rows_ui"),
                  div(
                    style = "display: none;",
                    textAreaInput(
                      "quick_embryo_age_groups_input",
                      "Age Group Details",
                      value = "",
                      rows = 2,
                      placeholder = "E15.5 x 1; E16.5 x 7"
                    )
                  )
                ),
                tags$div(style = "border-top: 1px solid #cbd5e1; margin: 8px 0 10px 0;"),
                fluidRow(
                  column(4,
                    numericInput("quick_male_embryos_input", "Male", value = ifelse(is.na(report_defaults$final_report_male_embryos), NA, report_defaults$final_report_male_embryos), min = 0, step = 1)
                  ),
                  column(4,
                    numericInput("quick_female_embryos_input", "Female", value = ifelse(is.na(report_defaults$final_report_female_embryos), NA, report_defaults$final_report_female_embryos), min = 0, step = 1)
                  ),
                  column(4,
                    numericInput("quick_unknown_embryos_input", "Unknown", value = ifelse(is.na(report_defaults$final_report_unknown_embryos), NA, report_defaults$final_report_unknown_embryos), min = 0, step = 1)
                  )
                )
              )
            )
          )
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_collection_report_btn", "Save Collection Report", class = "btn-danger")
        )
      ))
    }, finally = {
      db_disconnect(con)
    })
  }
  
  # Set plugging status to Empty (Alive) - using unified modal
  observeEvent(input$set_status_empty_btn, {
    plugging_id <- plugging_state$viewing_id
    if (is.null(plugging_id)) return()
    con <- db_connect()
    tryCatch({
      plugging_row <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      if (nrow(plugging_row) == 0) return()
      female_id <- plugging_row$female_id[1]
      female_info <- DBI::dbGetQuery(con, "SELECT * FROM mice_stock WHERE asu_id = ?", params = list(female_id))
      female_age <- NA
      if (!is.null(female_info) && nrow(female_info) > 0) {
        dob <- female_info$dob[1]
        if (!is.na(dob) && dob != "") {
          female_age <- floor(as.numeric(Sys.Date() - as.Date(dob)) / 7)
        }
      }
      # Get current notes for the modal
      current_notes <- ifelse(is.na(plugging_row$notes) || plugging_row$notes == "", "", plugging_row$notes)
      showModal(show_empty_alive_modal(female_info, female_age, Sys.Date(), current_notes, "set_status_empty_date_input", "confirm_set_status_empty_btn"))
    }, finally = {
      db_disconnect(con)
    })
  })
  
  # Confirm set status empty
  observeEvent(input$confirm_set_status_empty_btn, {
    plugging_id <- plugging_state$viewing_id
    if (is.null(plugging_id)) return()
    date_of_death <- input$set_status_empty_date_input
    notes <- input$set_status_empty_notes_input
    
    con <- db_connect()
    tryCatch({
      # Get current values for audit trail
      current <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      
      if (nrow(current) == 0) {
        showNotification("Plugging event not found", type = "error")
        return()
      }
      old_values <- current[1, ]
      
      # Update the plugging event
      updated_notes <- normalize_plugging_notes_input(notes)
      result <- DBI::dbExecute(con, 
        "UPDATE plugging_history SET \
         plugging_status = 'Empty',\
         updated_at = DATETIME('now'),\
         notes = ?\
         WHERE id = ?",
        params = list(
          updated_notes,
          plugging_id
        )
      )
      
      if (result > 0) {
        # Log to audit trail
        log_audit_trail(
          "plugging_history",
          plugging_id,
          "UPDATE",
          old_values,
          list(
            plugging_status = "Empty",
            confirmation_date = as.character(date_of_death),
            notes = notes
          )
        )
        
        showNotification("Plugging status set to Empty!", type = "message")
        removeModal()
        plugging_state$viewing_id <- NULL
        plugging_state$reload <- Sys.time()
      } else {
        showNotification("Failed to update plugging status", type = "error")
      }
      
    }, error = function(e) {
      showNotification(paste("Error updating plugging status:", e$message), type = "error")
    }, finally = {
      db_disconnect(con)
    })
  })

  # Function to auto-update plugging_status to 'Not Observed (Waiting for confirmation)' if pairing period is over and status is still 'Ongoing'
  auto_update_plugging_status_to_unknown <- function() {
    con <- db_connect()
    tryCatch({
      today <- as.character(Sys.Date())
      # Find all ongoing records where pairing_end_date is before today (not equal to today)
      records <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE plugging_status = 'Ongoing' AND pairing_end_date IS NOT NULL AND pairing_end_date < ?", params = list(today))
      for (i in seq_len(nrow(records))) {
        rec <- records[i, ]
        # Update status to Not Observed (Waiting for confirmation) and set plug_observed_date to Unknown
        DBI::dbExecute(con, "UPDATE plugging_history SET plugging_status = 'Not Observed (Waiting for confirmation)', plug_observed_date = 'Unknown', updated_at = DATETIME('now') WHERE id = ?", params = list(rec$id))
        # Log to audit trail (use list for new_values, just like user actions, use user_id = 'system(auto)' )
        log_audit_trail(
          "plugging_history",
          rec$id,
          "UPDATE",
          as.list(rec),
          list(
            plugging_status = "Not Observed (Waiting for confirmation)",
            plug_observed_date = "Unknown",
            confirmation_date = as.character(Sys.Date()),
            notes = NULL
          ),
          user_id = "system(auto)"
        )
      }
    }, finally = {
      db_disconnect(con)
    })
  }

  # Auto-update plugging_status to Unknown if needed
  auto_update_plugging_status_to_unknown()

  # Add observeEvent for quick_delete_plugging_btn
  observeEvent(input$quick_delete_plugging_btn, {
    plugging_id <- input$quick_delete_plugging_btn
    if (is.null(plugging_id)) return()
    pending_delete_id(plugging_id)
    showModal(modalDialog(
      title = "Confirm Delete Plugging Record",
      #size = "s",
      "Are you sure you want to delete this plugging record? This action cannot be undone.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_quick_delete_plugging_btn", "Delete", class = "btn-danger")
      )
    ))
  })

  # Register the confirmation observer only once
  observeEvent(input$confirm_quick_delete_plugging_btn, {
    plugging_id <- pending_delete_id()
    if (is.null(plugging_id)) return()
    removeModal()
    con <- db_connect()
    tryCatch({
      old_row <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      if (nrow(old_row) == 0) return()
      DBI::dbExecute(con, "UPDATE plugging_history SET plugging_status = 'Deleted' WHERE id = ?", params = list(plugging_id))
      log_audit_trail(
        "plugging_history",
        plugging_id,
        "UPDATE",
        as.list(old_row[1, ]),
        modifyList(as.list(old_row[1, ]), list(plugging_status = 'Deleted'))
      )
      showNotification("Plugging record marked as deleted.", type = "message")
      # Save state before reload
      session$sendCustomMessage(type = "eval", message = "if(typeof saveDataTableState === 'function') saveDataTableState('plugging_history_table');")
      session$sendCustomMessage(type = "eval", message = "if(typeof saveScrollForAllTables === 'function') saveScrollForAllTables();")
      plugging_state$reload <- Sys.time()
    }, finally = {
      db_disconnect(con)
    })
    pending_delete_id(NULL)
  })

  # Mark plug as Surprising Plug!!
  observeEvent(input$mark_surprising_plug_btn, {
    plugging_id <- plugging_state$viewing_id
    if (is.null(plugging_id)) return()
    
    # Get current notes for the modal
    con <- db_connect()
    current_notes <- ""
    tryCatch({
      current_record <- DBI::dbGetQuery(con, "SELECT notes FROM plugging_history WHERE id = ?", params = list(plugging_id))
      if (nrow(current_record) > 0) {
        current_notes <- ifelse(is.na(current_record$notes) || current_record$notes == "", "", current_record$notes)
      }
    }, finally = {
      db_disconnect(con)
    })
    
    showModal(modalDialog(
      title = "Mark as Surprising Plug!!😱😮‍💨",
      size = "m",
      tagList(
        div(
          style = "text-align: center; padding: 20px;",
          tags$p("This will set the plug observed date to 'Unknown' and estimate the plug date using the pairing start date for calendar purposes."),
          br(),
          textInput("expected_age_for_harvesting_surprising_input", "Expected Age for Harvesting (Embryonic Days, e.g. 14)", value = "", width = "100%"),
          br(),
          textAreaInput("surprising_plug_notes_input", "Notes", value = current_notes, rows = 3, width = "100%"),
          br(),
          tags$p("This will update the plugging status to 'Surprising Plug!!' and set the plug observed date to 'Unknown'.")
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_surprising_plug_btn", "Confirm Surprising Plug!!", class = "btn-success")
      )
    ))
  })
  
  # Confirm Surprising Plug!!
  observeEvent(input$confirm_surprising_plug_btn, {
    plugging_id <- plugging_state$viewing_id
    if (is.null(plugging_id)) return()
    
    con <- db_connect()
    tryCatch({
      # Get current values for audit trail
      current <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      
      if (nrow(current) == 0) {
        showNotification("Plugging event not found", type = "error")
        return()
      }
      
      old_values <- current[1, ]
      
      # Update the plugging event
      updated_notes <- normalize_plugging_notes_input(input$surprising_plug_notes_input)
      result <- DBI::dbExecute(con, 
        "UPDATE plugging_history SET 
         plug_observed_date = 'Unknown',
         plugging_status = 'Surprising Plug!!',
         expected_age_for_harvesting = ?,
         notes = ?,
         updated_at = DATETIME('now')
         WHERE id = ?",
        params = list(
          input$expected_age_for_harvesting_surprising_input,
          updated_notes,
          plugging_id
        )
      )
      
      if (result > 0) {
        # Log to audit trail
        log_audit_trail(
          "plugging_history",
          plugging_id,
          "UPDATE",
          old_values,
          list(
            plug_observed_date = "Unknown",
            plugging_status = "Surprising Plug!!",
            expected_age_for_harvesting = input$expected_age_for_harvesting_surprising_input,
            notes = input$surprising_plug_notes_input
          )
        )
        
        showNotification("🎉 Marked as Surprising Plug!! successfully!", type = "message")
        removeModal()
        plugging_state$viewing_id <- NULL
        plugging_state$reload <- Sys.time()
        
        # Trigger global refresh for cross-module updates
        global_refresh_trigger(Sys.time())
        
        # Refresh all_mice_table if available
        if (!is.null(all_mice_table)) {
          con_refresh <- db_connect()
          tryCatch({
            all_data <- DBI::dbGetQuery(con_refresh, "SELECT * FROM mice_stock ORDER BY asu_id")
            all_mice_table(all_data)
          }, finally = {
            db_disconnect(con_refresh)
          })
        }
      } else {
        showNotification("Failed to update plugging event", type = "error")
      }
      
    }, error = function(e) {
      showNotification(paste("Error updating plugging event:", e$message), type = "error")
    }, finally = {
      db_disconnect(con)
    })
  })

  # Remove the old modal logic and call the new module server
  add_plugging_modal_server(
    "add_plugging_modal",
    get_live_mice = get_live_mice,
    get_mouse_info = function(asu_id) {
      # Use unified function from validation.R (without status for modal display)
      get_mouse_info(asu_id, include_status = FALSE)
    },
    validate_mice_active_status = validate_mice_active_status,
    db_connect = db_connect,
    db_disconnect = db_disconnect,
    log_audit_trail = log_audit_trail,
    auto_update_plugging_status_to_unknown = auto_update_plugging_status_to_unknown,
    plugging_state = plugging_state,
    is_system_locked = is_system_locked,
    global_refresh_trigger = global_refresh_trigger
  )
  
  # Refresh button observer
  observeEvent(input$refresh_plugging_table_btn, {
    # Save scroll and table state before refresh
    session$sendCustomMessage(type = "eval", message = "if(typeof saveDataTableState === 'function') saveDataTableState('plugging_history_table');")
    session$sendCustomMessage(type = "eval", message = "if(typeof saveScrollForAllTables === 'function') saveScrollForAllTables();")
    
    plugging_state$reload <- Sys.time()
    if (!is.null(global_refresh_trigger)) {
      global_refresh_trigger(Sys.time())
    }
    showNotification("Table refreshed!", type = "message")
  })

  # Quick update modal: Confirm Plugged
  observeEvent(input$confirm_plug_observed_btn_quick, {
    plugging_id <- plugging_state$confirming_id
    if (is.null(plugging_id)) return()
    con <- db_connect()
    tryCatch({
      current <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      if (nrow(current) == 0) {
        showNotification("Plugging event not found", type = "error")
        return()
      }
      old_values <- current[1, ]
      plug_observed_date_value <- if(input$plug_observed_type == "unknown") {
        "Unknown"
      } else {
        as.character(input$plug_observed_date_input)
      }
      
      # Auto-set pairing end date to match plug observed date for quick updates
      pairing_end_date_value <- plug_observed_date_value
      
      result <- DBI::dbExecute(con, 
        "UPDATE plugging_history SET \
         plug_observed_date = ?,\
         pairing_end_date = ?,\
         plugging_status = 'Plugged',\
         expected_age_for_harvesting = ?,\
         notes = ?,\
         updated_at = DATETIME('now')\
         WHERE id = ?",
        params = list(
          plug_observed_date_value,
          pairing_end_date_value,
          input$expected_age_for_harvesting_input,
          normalize_plugging_notes_input(input$plug_observed_notes_input),
          plugging_id
        )
      )
      if (result > 0) {
        log_audit_trail(
          "plugging_history",
          plugging_id,
          "UPDATE",
          old_values,
          list(
            plug_observed_date = plug_observed_date_value,
            pairing_end_date = pairing_end_date_value,
            plugging_status = "Plugged",
            expected_age_for_harvesting = input$expected_age_for_harvesting_input,
            notes = input$plug_observed_notes_input
          )
        )
        showNotification("Plug marked as observed successfully!", type = "message")
        removeModal()
        plugging_state$confirming_id <- NULL
        # Save state before reload
        session$sendCustomMessage(type = "eval", message = "if(typeof saveDataTableState === 'function') saveDataTableState('plugging_history_table');")
        session$sendCustomMessage(type = "eval", message = "if(typeof saveScrollForAllTables === 'function') saveScrollForAllTables();")
        plugging_state$reload <- Sys.time()
        if (!is.null(global_refresh_trigger)) global_refresh_trigger(Sys.time())
      } else {
        showNotification("Failed to update plugging event", type = "error")
      }
    }, error = function(e) {
      showNotification(paste("Error updating plugging event:", e$message), type = "error")
    }, finally = {
      db_disconnect(con)
    })
  })

  # Quick update modal: Confirm Surprising Plug!!
  observeEvent(input$confirm_surprising_plug_btn_quick, {
    plugging_id <- plugging_state$confirming_id
    if (is.null(plugging_id)) return()
    con <- db_connect()
    tryCatch({
      current <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      if (nrow(current) == 0) {
        showNotification("Plugging event not found", type = "error")
        return()
      }
      old_values <- current[1, ]
      result <- DBI::dbExecute(con, 
        "UPDATE plugging_history SET \
         plug_observed_date = 'Unknown',\
         plugging_status = 'Surprising Plug!!',\
         expected_age_for_harvesting = ?,\
         notes = ?,\
         updated_at = DATETIME('now')\
         WHERE id = ?",
        params = list(
          input$expected_age_for_harvesting_surprising_input_quick,
          normalize_plugging_notes_input(input$surprising_plug_notes_input_quick),
          plugging_id
        )
      )
      if (result > 0) {
        log_audit_trail(
          "plugging_history",
          plugging_id,
          "UPDATE",
          old_values,
          list(
            plug_observed_date = "Unknown",
            plugging_status = "Surprising Plug!!",
            expected_age_for_harvesting = input$expected_age_for_harvesting_surprising_input_quick,
            notes = input$surprising_plug_notes_input_quick
          )
        )
        showNotification("🎉 Marked as Surprising Plug!! successfully!", type = "message")
        removeModal()
        plugging_state$confirming_id <- NULL
        plugging_state$reload <- Sys.time()
        if (!is.null(global_refresh_trigger)) global_refresh_trigger(Sys.time())
      } else {
        showNotification("Failed to update plugging event", type = "error")
      }
    }, error = function(e) {
      showNotification(paste("Error updating plugging event:", e$message), type = "error")
    }, finally = {
      db_disconnect(con)
    })
  })

  # Quick update modal: Confirm Euthanized
  observeEvent(input$confirm_euthanasia_btn_quick, {
    plugging_id <- plugging_state$confirming_id
    if (is.null(plugging_id)) return()
    con <- db_connect()
    tryCatch({
      plugging <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      if (nrow(plugging) == 0) return()
      row <- plugging[1, ]
      # Update female mouse status to Deceased
      result <- DBI::dbExecute(con, 
        "UPDATE mice_stock SET status = 'Deceased', date_of_death = ?, deceased_timestamp = DATETIME('now'), last_updated = DATETIME('now') WHERE asu_id = ?",
        params = list(
          as.character(input$euthanasia_date_input_quick),
          row$female_id
        )
      )
      if (result > 0) {
        # Log to audit trail for female mouse
        audit_result <- log_audit_trail(
          "mice_stock",
          row$female_id,
          "UPDATE",
          list(status = "Alive"),
          list(
            status = "Deceased",
            date_of_death = as.character(input$euthanasia_date_input_quick),
            notes = input$euthanasia_notes_input_quick,
            source = "Plugging Tab",
            source_event_id = plugging_id
          )
        )
        # Update plugging_status to selected value (Empty or Collected)
        selected_status <- input$euthanasia_status_choice
        DBI::dbExecute(con, 
          "UPDATE plugging_history SET plugging_status = ?, updated_at = DATETIME('now') WHERE id = ?",
          params = list(selected_status, plugging_id)
        )
        showNotification(paste("Female mouse marked as deceased and plug set to", selected_status, "!"), type = "message")
        removeModal()
        plugging_state$confirming_id <- NULL
        plugging_state$reload <- Sys.time()
        if (!is.null(global_refresh_trigger)) global_refresh_trigger(Sys.time())
      } else {
        showNotification("Failed to update female mouse status", type = "error")
      }
    }, error = function(e) {
      showNotification(paste("Error updating mouse status:", e$message), type = "error")
    }, finally = {
      db_disconnect(con)
    })
  })

  # Quick update modal: Confirm Empty Plug
  observeEvent(input$confirm_set_status_empty_btn_quick, {
    plugging_id <- plugging_state$confirming_id
    if (is.null(plugging_id)) return()
    notes <- input$set_status_empty_notes_input_quick
    con <- db_connect()
    tryCatch({
      current <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      if (nrow(current) == 0) {
        showNotification("Plugging event not found", type = "error")
        return()
      }
      old_values <- current[1, ]
      result <- DBI::dbExecute(con, 
        "UPDATE plugging_history SET \
         plugging_status = 'Empty',\
         updated_at = DATETIME('now'),\
         notes = ?\
         WHERE id = ?",
        params = list(
          normalize_plugging_notes_input(notes),
          plugging_id
        )
      )
      if (result > 0) {
        log_audit_trail(
          "plugging_history",
          plugging_id,
          "UPDATE",
          old_values,
          list(
            plugging_status = "Empty",
            confirmation_date = as.character(input$set_status_empty_date_input_quick),
            notes = notes
          )
        )
        showNotification("Plugging status set to Empty!", type = "message")
        removeModal()
        plugging_state$confirming_id <- NULL
        plugging_state$reload <- Sys.time()
        if (!is.null(global_refresh_trigger)) global_refresh_trigger(Sys.time())
      } else {
        showNotification("Failed to update plugging status", type = "error")
      }
    }, error = function(e) {
      showNotification(paste("Error updating plugging status:", e$message), type = "error")
    }, finally = {
      db_disconnect(con)
    })
  })

  # Add new observer for quick euthanasia/collected confirmation
  save_collection_report <- function(trigger_value = NULL, trigger_name = "unknown") {
    plugging_id <- plugging_state$confirming_id
    write_collection_debug_log(
      "save_clicked",
      list(
        trigger = trigger_name,
        button_value = ifelse(is.null(trigger_value), "NULL", trigger_value),
        plugging_id = ifelse(is.null(plugging_id), "NULL", plugging_id),
        selected_status_raw = ifelse(is.null(input$confirm_status_choice), "NULL", input$confirm_status_choice)
      )
    )
    if (is.null(plugging_id)) {
      write_collection_debug_log("early_return_missing_plugging_id")
      showNotification("Collection report save did not start: missing plugging event context.", type = "error")
      return()
    }

    selected_status <- input$confirm_status_choice
    if (is.null(selected_status) || identical(selected_status, "")) {
      selected_status <- "Collected"
    }
    date_of_death <- input$quick_euthanasia_date_input
    notes <- input$quick_euthanasia_notes_input
    if (is.null(selected_status) || !(selected_status %in% c("Empty", "Collected"))) {
      write_collection_debug_log("early_return_invalid_status", list(selected_status = selected_status))
      showNotification("Collection report save did not start: invalid status value.", type = "error")
      return()
    }

    normalize_text_scalar <- function(value) {
      if (is.null(value) || length(value) == 0 || is.na(value[1])) {
        return("")
      }

      trimws(as.character(value[1]))
    }

    normalize_integer_scalar <- function(value) {
      if (is.null(value) || length(value) == 0) {
        return(NA_integer_)
      }

      suppressWarnings(as.integer(value[1]))
    }

    normalize_flag_scalar <- function(value) {
      if (is.null(value) || length(value) == 0 || is.na(value[1])) {
        return(FALSE)
      }

      isTRUE(as.logical(value[1]))
    }

    saved_db_path <- normalizePath(get_current_db_path(), mustWork = FALSE)
    saved_db_label <- basename(saved_db_path)
    con <- db_connect()
    transaction_started <- FALSE
    save_committed <- FALSE

    tryCatch({
      write_collection_debug_log("start", list(plugging_id = plugging_id, selected_status = selected_status))
      DBI::dbBegin(con)
      transaction_started <- TRUE
      write_collection_debug_log("transaction_started")

      current <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      if (nrow(current) == 0) {
        stop("Plugging event not found")
      }
      write_collection_debug_log("loaded_current_row", list(rows = nrow(current), female_id = current$female_id[1]))
      old_values <- current[1, ]
      existing_row <- current[1, , drop = FALSE]

      current_text_value <- function(column_name) {
        if (!(column_name %in% names(existing_row))) {
          return("")
        }

        normalize_text_scalar(existing_row[[column_name]][1])
      }

      current_integer_value <- function(column_name) {
        if (!(column_name %in% names(existing_row))) {
          return(NA_integer_)
        }

        normalize_integer_scalar(existing_row[[column_name]][1])
      }

      current_flag_value <- function(column_name) {
        if (!(column_name %in% names(existing_row))) {
          return(FALSE)
        }

        normalize_flag_scalar(existing_row[[column_name]][1])
      }

      collection_date_value <- safe_analysis_date(date_of_death)
      collection_date_text <- if (is.na(collection_date_value)) {
        current_text_value("final_report_date")
      } else {
        as.character(collection_date_value)
      }

      primary_age_text <- normalize_text_scalar(input$quick_primary_embryo_age_input)
      if (primary_age_text == "") {
        primary_age_text <- current_text_value("final_report_primary_age")
      }

      normalized_primary_age <- normalize_embryo_age_input(primary_age_text)
      primary_age_value <- if (is.na(normalized_primary_age$numeric)) {
        suppressWarnings(as.numeric(existing_row$final_report_primary_age_value[1]))
      } else {
        normalized_primary_age$numeric
      }
      primary_age_label <- if (is.na(normalized_primary_age$numeric)) {
        if (primary_age_text == "") NA_character_ else primary_age_text
      } else {
        normalized_primary_age$label
      }

      mixed_age_text <- normalize_text_scalar(input$quick_embryo_age_groups_input)
      mixed_age_flag <- isTRUE(input$quick_mixed_embryo_ages_input) || current_flag_value("final_report_mixed_age")
      age_groups_json <- NA_character_

      total_embryos_value <- normalize_integer_scalar(input$quick_total_embryos_input)
      if (is.na(total_embryos_value)) {
        total_embryos_value <- current_integer_value("final_report_total_embryos")
      }

      male_embryos_value <- normalize_integer_scalar(input$quick_male_embryos_input)
      if (is.na(male_embryos_value)) {
        male_embryos_value <- current_integer_value("final_report_male_embryos")
      }

      female_embryos_value <- normalize_integer_scalar(input$quick_female_embryos_input)
      if (is.na(female_embryos_value)) {
        female_embryos_value <- current_integer_value("final_report_female_embryos")
      }

      unknown_embryos_value <- normalize_integer_scalar(input$quick_unknown_embryos_input)
      if (is.na(unknown_embryos_value)) {
        unknown_embryos_value <- current_integer_value("final_report_unknown_embryos")
      }

      normalized_embryo_counts <- normalize_final_report_embryo_counts(
        total_embryos = total_embryos_value,
        male_embryos = male_embryos_value,
        female_embryos = female_embryos_value,
        unknown_embryos = unknown_embryos_value
      )
      write_collection_debug_log(
        "normalized_embryo_counts",
        list(
          total = normalized_embryo_counts$final_report_total_embryos,
          male = normalized_embryo_counts$final_report_male_embryos,
          female = normalized_embryo_counts$final_report_female_embryos,
          unknown = normalized_embryo_counts$final_report_unknown_embryos,
          autofilled = paste(normalized_embryo_counts$autofilled_fields, collapse = "/"),
          validation = ifelse(is.null(normalized_embryo_counts$validation_message), "", normalized_embryo_counts$validation_message)
        )
      )
      if (!is.null(normalized_embryo_counts$validation_message)) {
        stop(normalized_embryo_counts$validation_message)
      }

      total_embryos_value <- normalized_embryo_counts$final_report_total_embryos
      male_embryos_value <- normalized_embryo_counts$final_report_male_embryos
      female_embryos_value <- normalized_embryo_counts$final_report_female_embryos
      unknown_embryos_value <- normalized_embryo_counts$final_report_unknown_embryos

      if (mixed_age_text != "") {
        parsed_age_groups <- parse_age_groups_text(mixed_age_text)
        parsed_age_groups <- complete_age_groups_with_primary(
          parsed_age_groups,
          total_embryos_value,
          primary_age_label = primary_age_label,
          primary_age_value = primary_age_value
        )
        if (nrow(parsed_age_groups) > 0) {
          age_groups_json <- jsonlite::toJSON(parsed_age_groups, auto_unbox = TRUE, dataframe = "rows", null = "null")
          mixed_age_flag <- TRUE
        }
      } else if (mixed_age_flag) {
        existing_age_groups_json <- current_text_value("final_report_age_groups_json")
        age_groups_json <- if (existing_age_groups_json == "") NA_character_ else existing_age_groups_json
      }

      final_report_notes_value <- normalize_text_scalar(notes)

      report_details <- list(
        final_report_date = if (collection_date_text == "") NA_character_ else collection_date_text,
        final_report_primary_age = primary_age_label,
        final_report_primary_age_value = if (is.na(primary_age_value)) NA_real_ else primary_age_value,
        final_report_total_embryos = total_embryos_value,
        final_report_male_embryos = male_embryos_value,
        final_report_female_embryos = female_embryos_value,
        final_report_unknown_embryos = unknown_embryos_value,
        final_report_mixed_age = mixed_age_flag,
        final_report_age_groups_json = age_groups_json,
        final_report_notes = final_report_notes_value
      )
      write_collection_debug_log(
        "report_details_ready",
        list(
          final_report_date = report_details$final_report_date,
          primary_age = report_details$final_report_primary_age,
          total = report_details$final_report_total_embryos,
          male = report_details$final_report_male_embryos,
          female = report_details$final_report_female_embryos,
          unknown = report_details$final_report_unknown_embryos,
          mixed_age = report_details$final_report_mixed_age
        )
      )

      female_id <- old_values$female_id
      female_state <- DBI::dbGetQuery(
        con,
        "SELECT status, date_of_death FROM mice_stock WHERE asu_id = ? LIMIT 1",
        params = list(female_id)
      )
      female_requires_update <- nrow(female_state) == 0 || !identical(normalize_text_scalar(female_state$status[1]), "Deceased")

      resolved_date_of_death <- safe_analysis_date(date_of_death)
      if (is.na(resolved_date_of_death)) {
        resolved_date_of_death <- safe_analysis_date(report_details$final_report_date)
      }
      if (is.na(resolved_date_of_death) && nrow(female_state) > 0 && "date_of_death" %in% names(female_state)) {
        resolved_date_of_death <- safe_analysis_date(female_state$date_of_death[1])
      }
      if (is.na(resolved_date_of_death)) {
        resolved_date_of_death <- Sys.Date()
      }

      has_plugging_changes <- !identical(normalize_text_scalar(existing_row$plugging_status[1]), normalize_text_scalar(selected_status)) ||
        !identical(normalize_text_scalar(existing_row$final_report_date[1]), normalize_text_scalar(report_details$final_report_date)) ||
        !identical(normalize_text_scalar(existing_row$final_report_primary_age[1]), normalize_text_scalar(report_details$final_report_primary_age)) ||
        !identical(suppressWarnings(as.numeric(existing_row$final_report_primary_age_value[1])), suppressWarnings(as.numeric(report_details$final_report_primary_age_value))) ||
        !identical(normalize_integer_scalar(existing_row$final_report_total_embryos[1]), normalize_integer_scalar(report_details$final_report_total_embryos)) ||
        !identical(normalize_integer_scalar(existing_row$final_report_male_embryos[1]), normalize_integer_scalar(report_details$final_report_male_embryos)) ||
        !identical(normalize_integer_scalar(existing_row$final_report_female_embryos[1]), normalize_integer_scalar(report_details$final_report_female_embryos)) ||
        !identical(normalize_integer_scalar(existing_row$final_report_unknown_embryos[1]), normalize_integer_scalar(report_details$final_report_unknown_embryos)) ||
        !identical(normalize_flag_scalar(existing_row$final_report_mixed_age[1]), normalize_flag_scalar(report_details$final_report_mixed_age)) ||
        !identical(normalize_text_scalar(existing_row$final_report_age_groups_json[1]), normalize_text_scalar(report_details$final_report_age_groups_json)) ||
        !identical(normalize_text_scalar(existing_row$final_report_notes[1]), normalize_text_scalar(report_details$final_report_notes))

      if (!has_plugging_changes && !female_requires_update) {
        write_collection_debug_log("no_changes_detected", list(plugging_id = plugging_id))
        DBI::dbRollback(con)
        transaction_started <- FALSE
        showNotification("No changes detected. Nothing was saved.", type = "warning")
        removeModal()
        plugging_state$confirming_id <- NULL
        return()
      }

      # Update plugging status
      plugging_rows_updated <- DBI::dbExecute(con, 
        "UPDATE plugging_history SET plugging_status = ?, updated_at = DATETIME('now'), final_report_date = ?, final_report_primary_age = ?, final_report_primary_age_value = ?, final_report_total_embryos = ?, final_report_male_embryos = ?, final_report_female_embryos = ?, final_report_unknown_embryos = ?, final_report_mixed_age = ?, final_report_age_groups_json = ?, final_report_notes = ? WHERE id = ?",
        params = list(
          selected_status,
          report_details$final_report_date,
          report_details$final_report_primary_age,
          report_details$final_report_primary_age_value,
          report_details$final_report_total_embryos,
          report_details$final_report_male_embryos,
          report_details$final_report_female_embryos,
          report_details$final_report_unknown_embryos,
          ifelse(isTRUE(report_details$final_report_mixed_age), 1L, 0L),
          report_details$final_report_age_groups_json,
          report_details$final_report_notes,
          plugging_id
        )
      )
      write_collection_debug_log("plugging_history_updated", list(rows = plugging_rows_updated))
      if (!isTRUE(plugging_rows_updated == 1L)) {
        stop("Plugging report update did not affect the expected record.")
      }

      # Update female mouse to Deceased
      DBI::dbExecute(con, 
        "UPDATE mice_stock SET status = 'Deceased', date_of_death = ?, deceased_timestamp = DATETIME('now'), last_updated = DATETIME('now') WHERE asu_id = ? AND status != 'Deceased'",
        params = list(
          as.character(resolved_date_of_death),
          female_id
        )
      )
      write_collection_debug_log("female_status_updated", list(female_id = female_id, date_of_death = as.character(resolved_date_of_death)))

      updated_row <- DBI::dbGetQuery(
        con,
        "SELECT plugging_status, final_report_date, final_report_primary_age, final_report_total_embryos, final_report_notes, final_report_mixed_age FROM plugging_history WHERE id = ?",
        params = list(plugging_id)
      )
      write_collection_debug_log("reloaded_updated_row", list(rows = nrow(updated_row)))

      if (nrow(updated_row) != 1) {
        stop("Unable to confirm the saved collection report in the database.")
      }

      if (!identical(normalize_text_scalar(updated_row$plugging_status[1]), normalize_text_scalar(selected_status)) ||
          !identical(normalize_text_scalar(updated_row$final_report_date[1]), normalize_text_scalar(report_details$final_report_date)) ||
          !identical(normalize_text_scalar(updated_row$final_report_primary_age[1]), normalize_text_scalar(report_details$final_report_primary_age)) ||
          !identical(normalize_integer_scalar(updated_row$final_report_total_embryos[1]), normalize_integer_scalar(report_details$final_report_total_embryos)) ||
          !identical(normalize_flag_scalar(updated_row$final_report_mixed_age[1]), normalize_flag_scalar(report_details$final_report_mixed_age)) ||
          !identical(normalize_text_scalar(updated_row$final_report_notes[1]), normalize_text_scalar(report_details$final_report_notes))) {
        stop("Collection report save could not be verified after updating the database.")
      }

      DBI::dbCommit(con)
      transaction_started <- FALSE
      save_committed <- TRUE
      write_collection_debug_log("transaction_committed")

      showNotification(
        paste0(
          "Plugging status updated to '",
          selected_status,
          "' and saved to ",
          saved_db_label,
          "."
        ),
        type = "message"
      )
      if (length(normalized_embryo_counts$autofilled_fields) > 0) {
        autofill_labels <- vapply(
          normalized_embryo_counts$autofilled_fields,
          function(field_name) {
            switch(
              field_name,
              total = "Total Embryos",
              male = "Male",
              female = "Female",
              unknown = "Unknown",
              field_name
            )
          },
          character(1)
        )
        showNotification(
          paste0(paste(autofill_labels, collapse = " and "), " were autofilled from the other embryo counts."),
          type = "message"
        )
      }
      removeModal()
      plugging_state$confirming_id <- NULL

      tryCatch({
        log_audit_trail(
          "mice_stock",
          female_id,
          "UPDATE",
          list(status = "Alive"),
          list(
            status = "Deceased",
            date_of_death = as.character(resolved_date_of_death),
            notes = notes,
            source = "Plugging Tab",
            source_event_id = plugging_id,
            source_db = saved_db_path
          )
        )
        log_audit_trail(
          "plugging_history",
          plugging_id,
          "UPDATE",
          old_values,
          list(
            plugging_status = selected_status,
            confirmation_date = as.character(resolved_date_of_death),
            final_report_date = report_details$final_report_date,
            final_report_primary_age = report_details$final_report_primary_age,
            final_report_primary_age_value = report_details$final_report_primary_age_value,
            final_report_total_embryos = report_details$final_report_total_embryos,
            final_report_male_embryos = report_details$final_report_male_embryos,
            final_report_female_embryos = report_details$final_report_female_embryos,
            final_report_unknown_embryos = report_details$final_report_unknown_embryos,
            final_report_mixed_age = report_details$final_report_mixed_age,
            final_report_age_groups_json = report_details$final_report_age_groups_json,
            final_report_notes = report_details$final_report_notes,
            source_db = saved_db_path
          )
        )
        Sys.sleep(1)
        write_collection_debug_log("audit_logged")
        auto_update_plugging_status_to_unknown()
        plugging_state$reload <- Sys.time()
        if (!is.null(global_refresh_trigger)) {
          global_refresh_trigger(Sys.time())
        }
        invalidateLater(100, session)
      }, error = function(post_commit_error) {
        showNotification(
          paste("Collection report saved, but follow-up refresh failed:", post_commit_error$message),
          type = "warning"
        )
      })
    }, error = function(e) {
      write_collection_debug_log(
        "error",
        list(
          message = e$message,
          transaction_started = transaction_started,
          save_committed = save_committed
        )
      )
      if (isTRUE(transaction_started)) {
        tryCatch(DBI::dbRollback(con), error = function(rollback_error) NULL)
      }
      if (isTRUE(save_committed)) {
        showNotification(
          paste("Collection report saved to", saved_db_label, "but follow-up processing failed:", e$message),
          type = "warning"
        )
      } else {
        showNotification(paste("Error updating plugging status:", e$message), type = "error")
      }
    }, finally = {
      db_disconnect(con)
    })
  }

  observeEvent(input$confirm_quick_euthanasia_btn, {
    save_collection_report(input$confirm_quick_euthanasia_btn, "confirm_quick_euthanasia_btn")
  })

  observeEvent(input$confirm_collection_report_btn, {
    save_collection_report(input$confirm_collection_report_btn, "confirm_collection_report_btn")
  })

  # Add new observer for quick Empty (Alive) confirmation
  observeEvent(input$confirm_quick_empty_alive_btn, {
    plugging_id <- plugging_state$confirming_id
    if (is.null(plugging_id)) return()
    date_of_confirmation <- input$quick_empty_alive_date_input
    notes <- input$quick_empty_alive_notes_input
    con <- db_connect()
    tryCatch({
      current <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      if (nrow(current) == 0) {
        showNotification("Plugging event not found", type = "error")
        return()
      }
      old_values <- current[1, ]
      # Update plugging status to Empty (but don't mark mouse as deceased)
      updated_notes <- normalize_plugging_notes_input(notes)
      DBI::dbExecute(con, 
        "UPDATE plugging_history SET plugging_status = 'Empty', updated_at = DATETIME('now'), notes = ? WHERE id = ?",
        params = list(
          updated_notes,
          plugging_id
        )
      )
      log_audit_trail(
        "plugging_history",
        plugging_id,
        "UPDATE",
        old_values,
        list(
          plugging_status = "Empty",
          confirmation_date = as.character(date_of_confirmation),
          notes = notes
        )
      )
      showNotification("Plugging status updated to 'Empty (Alive)' successfully!", type = "message")
      removeModal()
      plugging_state$confirming_id <- NULL
      Sys.sleep(1)
      auto_update_plugging_status_to_unknown()
      plugging_state$reload <- Sys.time()
      if (!is.null(global_refresh_trigger)) {
        global_refresh_trigger(Sys.time())
      }
      invalidateLater(100, session)
    }, error = function(e) {
      showNotification(paste("Error updating plugging status:", e$message), type = "error")
    }, finally = {
      db_disconnect(con)
    })
  })

} 

