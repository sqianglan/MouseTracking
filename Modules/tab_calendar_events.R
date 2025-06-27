# Modules/tab_calendar_events.R
# Calendar modal for plugging events using calendR

library(shiny)
library(calendR)
library(DBI)
library(RSQLite)
library(ggsci)

# UI for the plugging calendar modal
plugging_calendar_modal_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6, selectInput(ns("cal_month"), "Month", choices = month.name, selected = month.name[as.integer(format(Sys.Date(), "%m"))])),
      column(6, numericInput(ns("cal_year"), "Year", value = as.integer(format(Sys.Date(), "%Y")), min = 2000, max = 2100))
    ),
    plotOutput(ns("calendar_plot"), height = "500px"),
    uiOutput(ns("calendar_legend"))
  )
}

# Server logic for the plugging calendar modal
plugging_calendar_modal_server <- function(id, db_path = DB_PATH) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Function to generate consistent colors for different asu_id values
    get_color_for_asu <- function(asu_id) {
      # Predefined color palette
     # colors <- c("orange", "blue", "green", "purple", "red", "brown", "pink", "cyan", "magenta", "darkgreen", 
     #            "darkblue", "darkred", "gold", "lightblue", "lightgreen", "lightcoral", "lightpink", "lightsalmon")
      colors <- pal_npg()(10)
      # Generate a hash-based index for consistent color assignment
      hash_val <- sum(utf8ToInt(asu_id)) %% length(colors)
      return(colors[hash_val + 1])  # +1 because R is 1-indexed
    }

    # Reactive: Get plugging events with observed dates and also the contents in Harvesting @
    plugging_data <- reactive({
      con <- NULL
      result <- data.frame(female_id = character(0), plug_observed_date = as.Date(character(0)), expected_age_for_harvesting = character(0), asu_id = character(0), plugging_status = character(0), pairing_start_date = as.Date(character(0)), stringsAsFactors = FALSE)
      tryCatch({
        con <- dbConnect(SQLite(), db_path)
        query <- "SELECT ph.female_id, ph.plug_observed_date, ph.expected_age_for_harvesting, ph.plugging_status, ph.pairing_start_date, ms.asu_id FROM plugging_history ph LEFT JOIN mice_stock ms ON ph.female_id = ms.asu_id WHERE (ph.plug_observed_date IS NOT NULL AND ph.plug_observed_date != '') OR (ph.plugging_status = 'Not Observed (Waiting for confirmation)' AND ph.pairing_start_date IS NOT NULL AND ph.pairing_start_date != '')"
        result <- dbGetQuery(con, query)
        if (nrow(result) > 0) {
          result$plug_observed_date <- as.Date(result$plug_observed_date)
          result$pairing_start_date <- as.Date(result$pairing_start_date)
        }
      }, error = function(e) {
        message("Error fetching plugging data: ", e$message)
      }, finally = {
        if (!is.null(con)) dbDisconnect(con)
      })
      result
    })

    # Render the calendar plot
    output$calendar_plot <- renderPlot({
      req(input$cal_month, input$cal_year)
      month_num <- match(input$cal_month, month.name)
      year_num <- input$cal_year
      pluggings <- plugging_data()
      if (nrow(pluggings) == 0) {
        calendR(year = year_num, month = month_num, title = paste("Plugging Calendar -", input$cal_month, year_num))
        return()
      }
      
      # Get all plugging events (not just current month) to calculate harvest dates
      # Expand rows for multiple expected ages, and compute correct day for each
      expanded <- do.call(rbind, lapply(seq_len(nrow(pluggings)), function(i) {
        asu <- pluggings$asu_id[i]
        exp_ages <- pluggings$expected_age_for_harvesting[i]
        status <- pluggings$plugging_status[i]
        plug_date <- pluggings$plug_observed_date[i]
        pairing_date <- pluggings$pairing_start_date[i]
        
        cat(sprintf("[DEBUG] Row %d: asu=%s, status=%s, plug_date=%s, pairing_date=%s, exp_ages=%s\n", i, as.character(asu), as.character(status), as.character(plug_date), as.character(pairing_date), as.character(exp_ages)))
        
        # Defensive: skip if any required field is missing or empty
        if (is.null(asu) || length(asu) == 0 || is.na(asu) ||
            is.null(status) || length(status) == 0 || is.na(status)) {
          cat(sprintf("[DEBUG] Skipping row %d due to missing asu or status\n", i))
          return(NULL)
        }
        
        # Defensive: ensure status is a single string
        if (length(status) != 1) {
          cat(sprintf("[DEBUG] Skipping row %d due to status length != 1\n", i))
          return(NULL)
        }
        
        # Defensive: ensure plug_date and pairing_date are either NA or length 1
        if (!is.null(plug_date) && length(plug_date) > 1) plug_date <- plug_date[1]
        if (!is.null(pairing_date) && length(pairing_date) > 1) pairing_date <- pairing_date[1]
        
        # Skip if plug_date is "Unknown"
        if (is.null(plug_date) || plug_date == "Unknown") {
          cat(sprintf("[DEBUG] Row %d: Skipping due to Unknown plug_date\n", i))
          return(NULL)
        }
        
        # Determine the base date based on status
        if (!is.null(status) && status == "Plugged" && !is.null(plug_date) && length(plug_date) > 0 && !is.na(plug_date)) {
          base_date <- plug_date
          color <- get_color_for_asu(asu)
          cat(sprintf("[DEBUG] Row %d: Using plug_date as base_date\n", i))
        } else if (!is.null(status) && status == "Not Observed (Waiting for confirmation)" && !is.null(pairing_date) && length(pairing_date) > 0 && !is.na(pairing_date)) {
          base_date <- pairing_date + 1
          color <- "gray"
          cat(sprintf("[DEBUG] Row %d: Using pairing_date as base_date\n", i))
        } else {
          cat(sprintf("[DEBUG] Skipping row %d due to status/base_date logic\n", i))
          return(NULL)
        }
        
        exp_ages_split <- character(0)
        if (!is.null(exp_ages) && length(exp_ages) > 0 && !is.na(exp_ages)) {
          exp_ages_split <- unlist(strsplit(exp_ages, "[,; ]"))
          exp_ages_split <- trimws(exp_ages_split)
          exp_ages_split <- exp_ages_split[grepl("[0-9]", exp_ages_split)]
        }
        
        if (length(exp_ages_split) == 0 || (length(exp_ages_split) == 1 && (is.na(exp_ages_split) || exp_ages_split == ""))) {
          # Default to E13.5
          offset <- 13.5
          label <- paste0(asu, " @E13.5")
          new_date <- base_date + floor(offset)
          if (is.na(new_date) || is.null(new_date) || length(new_date) == 0) {
            cat(sprintf("[DEBUG] Skipping row %d due to invalid new_date\n", i))
            return(NULL)
          }
          cat(sprintf("[DEBUG] Row %d: Using default E13.5\n", i))
          return(data.frame(asu_id = asu, label = label, day = as.integer(format(new_date, "%d")), date = new_date, 
                    target_month = as.integer(format(new_date, "%m")), target_year = as.integer(format(new_date, "%Y")), 
                    color = color, status = status, stringsAsFactors = FALSE))
        } else {
          out <- lapply(exp_ages_split, function(ea) {
            num <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", ea)))
            if (is.na(num) || is.null(num) || length(num) == 0) {
              cat(sprintf("[DEBUG] Skipping row %d, exp_ages_split=%s due to invalid num\n", i, as.character(ea)))
              return(NULL)
            }
            label <- paste0(asu, " @E", as.character(floor(num)+0.5))
            new_date <- base_date + floor(num)
            if (is.na(new_date) || is.null(new_date) || length(new_date) == 0) {
              cat(sprintf("[DEBUG] Skipping row %d, exp_ages_split=%s due to invalid new_date\n", i, as.character(ea)))
              return(NULL)
            }
            cat(sprintf("[DEBUG] Row %d: Using exp_ages_split=%s\n", i, as.character(ea)))
            return(data.frame(asu_id = asu, label = label, day = as.integer(format(new_date, "%d")), date = new_date,
                      target_month = as.integer(format(new_date, "%m")), target_year = as.integer(format(new_date, "%Y")),
                      color = color, status = status, stringsAsFactors = FALSE))
          })
          return(do.call(rbind, out))
        }
      }))
      
      # Filter for current month/year
      current_month_events <- expanded[expanded$target_month == month_num & expanded$target_year == year_num, ]
      
      if (nrow(current_month_events) == 0) {
        calendR(year = year_num, month = month_num, title = paste("Plugging Calendar -", input$cal_month, year_num))
        return()
      }
      
      # Ensure we only have valid positive integers for days
      valid_indices <- !is.na(current_month_events$day) & current_month_events$day > 0 & current_month_events$day <= 31
      current_month_events <- current_month_events[valid_indices, ]
      
      if (nrow(current_month_events) == 0 || length(current_month_events$day) == 0) {
        calendR(year = year_num, month = month_num, title = paste("Plugging Calendar -", input$cal_month, year_num))
        return()
      }
      
      days <- as.integer(current_month_events$day)
      labels <- current_month_events$label
      colors <- current_month_events$color
      
      # Final safety check
      if (length(days) == 0 || length(labels) == 0) {
        calendR(year = year_num, month = month_num, title = paste("Plugging Calendar -", input$cal_month, year_num))
        return()
      }
      
      # Create calendar with different colors for different statuses
      calendR(
        year = year_num,
        month = month_num,
        special.days = days,
        special.col = colors,
        text = labels,
        text.pos = days,
        text.size = 4,
        text.col = "black",
        title = paste("Plugging Calendar -", input$cal_month, year_num)
      )
    })

    # Optional: Legend for the calendar
    output$calendar_legend <- renderUI({
      pluggings <- plugging_data()
      req(input$cal_month, input$cal_year)
      month_num <- match(input$cal_month, month.name)
      year_num <- input$cal_year
      
      if (nrow(pluggings) == 0) return(NULL)
      
      # Legend: show all expanded labels for current month
      expanded <- do.call(rbind, lapply(seq_len(nrow(pluggings)), function(i) {
        asu <- pluggings$asu_id[i]
        exp_ages <- pluggings$expected_age_for_harvesting[i]
        status <- pluggings$plugging_status[i]
        plug_date <- pluggings$plug_observed_date[i]
        pairing_date <- pluggings$pairing_start_date[i]
        
        cat(sprintf("[DEBUG] Row %d: asu=%s, status=%s, plug_date=%s, pairing_date=%s, exp_ages=%s\n", i, as.character(asu), as.character(status), as.character(plug_date), as.character(pairing_date), as.character(exp_ages)))
        
        # Defensive: skip if any required field is missing or empty
        if (is.null(asu) || length(asu) == 0 || is.na(asu) ||
            is.null(status) || length(status) == 0 || is.na(status)) {
          cat(sprintf("[DEBUG] Skipping row %d due to missing asu or status\n", i))
          return(NULL)
        }
        
        # Defensive: ensure status is a single string
        if (length(status) != 1) {
          cat(sprintf("[DEBUG] Skipping row %d due to status length != 1\n", i))
          return(NULL)
        }
        
        # Defensive: ensure plug_date and pairing_date are either NA or length 1
        if (!is.null(plug_date) && length(plug_date) > 1) plug_date <- plug_date[1]
        if (!is.null(pairing_date) && length(pairing_date) > 1) pairing_date <- pairing_date[1]
        
        # Skip if plug_date is "Unknown"
        if (is.null(plug_date) || plug_date == "Unknown") {
          cat(sprintf("[DEBUG] Row %d: Skipping due to Unknown plug_date\n", i))
          return(NULL)
        }
        
        # Determine the base date based on status
        if (!is.null(status) && status == "Plugged" && !is.null(plug_date) && length(plug_date) > 0 && !is.na(plug_date)) {
          base_date <- plug_date
          color <- get_color_for_asu(asu)
          cat(sprintf("[DEBUG] Row %d: Using plug_date as base_date\n", i))
        } else if (!is.null(status) && status == "Not Observed (Waiting for confirmation)" && !is.null(pairing_date) && length(pairing_date) > 0 && !is.na(pairing_date)) {
          base_date <- pairing_date + 1
          color <- "gray"
          cat(sprintf("[DEBUG] Row %d: Using pairing_date as base_date\n", i))
        } else {
          cat(sprintf("[DEBUG] Skipping row %d due to status/base_date logic\n", i))
          return(NULL)
        }
        
        exp_ages_split <- character(0)
        if (!is.null(exp_ages) && length(exp_ages) > 0 && !is.na(exp_ages)) {
          exp_ages_split <- unlist(strsplit(exp_ages, "[,; ]"))
          exp_ages_split <- trimws(exp_ages_split)
          exp_ages_split <- exp_ages_split[grepl("[0-9]", exp_ages_split)]
        }
        
        if (length(exp_ages_split) == 0 || (length(exp_ages_split) == 1 && (is.na(exp_ages_split) || exp_ages_split == ""))) {
          # Default to E13.5
          offset <- 13.5
          label <- paste0(asu, " @E13.5")
          new_date <- base_date + floor(offset)
          if (is.na(new_date) || is.null(new_date) || length(new_date) == 0) {
            cat(sprintf("[DEBUG] Skipping row %d due to invalid new_date\n", i))
            return(NULL)
          }
          cat(sprintf("[DEBUG] Row %d: Using default E13.5\n", i))
          return(data.frame(asu_id = asu, label = label, date = new_date, 
                    target_month = as.integer(format(new_date, "%m")), target_year = as.integer(format(new_date, "%Y")),
                    color = color, status = status, stringsAsFactors = FALSE))
        } else {
          out <- lapply(exp_ages_split, function(ea) {
            num <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", ea)))
            if (is.na(num) || is.null(num) || length(num) == 0) {
              cat(sprintf("[DEBUG] Skipping row %d, exp_ages_split=%s due to invalid num\n", i, as.character(ea)))
              return(NULL)
            }
            label <- paste0(asu, " @E", as.character(floor(num)+0.5))
            new_date <- base_date + floor(num)
            if (is.na(new_date) || is.null(new_date) || length(new_date) == 0) {
              cat(sprintf("[DEBUG] Skipping row %d, exp_ages_split=%s due to invalid new_date\n", i, as.character(ea)))
              return(NULL)
            }
            cat(sprintf("[DEBUG] Row %d: Using exp_ages_split=%s\n", i, as.character(ea)))
            return(data.frame(asu_id = asu, label = label, date = new_date,
                      target_month = as.integer(format(new_date, "%m")), target_year = as.integer(format(new_date, "%Y")),
                      color = color, status = status, stringsAsFactors = FALSE))
          })
          return(do.call(rbind, out))
        }
      }))
      
      # Filter for current month/year
      current_month_events <- expanded[expanded$target_month == month_num & expanded$target_year == year_num, ]
      
      if (nrow(current_month_events) == 0) return(NULL)
      
      # Get unique asu_ids and their colors for the legend
      unique_events <- current_month_events[!duplicated(current_month_events$asu_id), ]
      # Only show color key for Plugged events (non-gray colors)
      plugged_events <- unique_events[unique_events$color != "gray", ]
      
      # Get plug dates for plugged events from original data
      if(nrow(plugged_events) > 0) {
        plugged_asu_ids <- plugged_events$asu_id
        plug_dates <- pluggings[pluggings$asu_id %in% plugged_asu_ids & pluggings$plugging_status == "Plugged", 
                               c("asu_id", "plug_observed_date")]
        # Merge plug dates with plugged_events
        plugged_events <- merge(plugged_events, plug_dates, by = "asu_id", all.x = TRUE)
      }
      
      tagList(
        tags$div(
          
          if(nrow(plugged_events) > 0) {
            tagList(
              tags$p(
                style = "margin-top: 10px; font-size: 12px;",
                tags$b("Plugged Mouse:")
              ),
              tags$ul(
                style = "list-style: none; padding: 0; margin: 10px 0;",
                lapply(seq_len(nrow(plugged_events)), function(i) {
                  plug_date_text <- if(!is.na(plugged_events$plug_observed_date[i])) {
                    format(as.Date(plugged_events$plug_observed_date[i]), "%d-%b")
                  } else {
                    "Unknown"
                  }
                  tags$li(
                    style = paste0("color: ", plugged_events$color[i], "; margin: 5px 0;"),
                    paste0(plugged_events$asu_id[i], " plugged @ ", plug_date_text)
                  )
                })
              )
            )
          },
          tags$p(
            style = "margin-top: 10px; font-size: 12px;",
            tags$span(style = "color: gray;", "Gray: "), "Not Observed (Waiting for confirmation) status (estimated from pairing date + 1 day)"
          )
        )
      )
    })
  })
}

# Function to show the calendar modal (reusable)
show_plugging_calendar_modal <- function(id = "plugging_calendar_modal", db_path = DB_PATH) {
  showModal(modalDialog(
    title = "Plugging Calendar",
    size = "l",
    plugging_calendar_modal_ui(id),
    easyClose = TRUE,
    footer = NULL
  ))
  plugging_calendar_modal_server(id, db_path)
}
