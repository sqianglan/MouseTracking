# Modules/tab_calendar_events.R
# Optimized Calendar modal for plugging events using calendR

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
    fluidRow(
      column(12, 
        div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
          div(),
          downloadButton(ns("export_ical_btn"), "📅 Export to iCal", 
                        style = "background: linear-gradient(135deg, #4caf50 0%, #388e3c 100%); color: white; border: none; font-weight: 500;")
        )
      )
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
      colors <- pal_npg()(10)
      hash_val <- sum(utf8ToInt(asu_id)) %% length(colors)
      return(colors[hash_val + 1])
    }

    # Helper function to parse expected ages
    parse_expected_ages <- function(exp_ages) {
      if (is.null(exp_ages) || length(exp_ages) == 0 || is.na(exp_ages) || exp_ages == "") {
        return(13.5) # Default to E13.5
      }
      
      ages_split <- unlist(strsplit(exp_ages, "[,; ]"))
      ages_split <- trimws(ages_split)
      ages_split <- ages_split[grepl("[0-9]", ages_split)]
      
      if (length(ages_split) == 0) {
        return(13.5) # Default to E13.5
      }
      
      # Convert to numeric and filter valid values
      ages_numeric <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", ages_split)))
      ages_numeric <- ages_numeric[!is.na(ages_numeric) & ages_numeric > 0]
      
      if (length(ages_numeric) == 0) {
        return(13.5) # Default to E13.5
      }
      
      return(ages_numeric)
    }

    # Helper function to safely parse dates
    safe_parse_date <- function(date_string) {
      if (is.null(date_string) || is.na(date_string) || date_string == "" || date_string == "Unknown") {
        return(as.Date(NA))
      }
      
      # Try different date formats
      date_formats <- c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%Y/%m/%d", "%m-%d-%Y", "%d-%m-%Y")
      
      for (format in date_formats) {
        tryCatch({
          parsed_date <- as.Date(date_string, format = format)
          if (!is.na(parsed_date)) {
            return(parsed_date)
          }
        }, error = function(e) {
          # Continue to next format
        })
      }
      
      # If all formats fail, return NA
      warning("Could not parse date: ", date_string)
      return(as.Date(NA))
    }

    # Helper function to calculate base date
    calculate_base_date <- function(status, plug_date, pairing_start_date, pairing_end_date) {
      # For Plugged or Plug Confirmed status
      if (status %in% c("Plugged", "Plug Confirmed")) {
        parsed_plug_date <- safe_parse_date(plug_date)
        if (!is.na(parsed_plug_date)) {
          return(list(date = parsed_plug_date, is_estimated = FALSE))
        } else {
          # If no plug observed date, use pairing end date for estimation
          parsed_pairing_end_date <- safe_parse_date(pairing_end_date)
          if (!is.na(parsed_pairing_end_date)) {
            return(list(date = parsed_pairing_end_date, is_estimated = TRUE))
          }
        }
      } else if (status == "Not Observed (Waiting for confirmation)") {
        parsed_pairing_date <- safe_parse_date(pairing_start_date)
        if (!is.na(parsed_pairing_date)) {
          return(list(date = parsed_pairing_date + 1, is_estimated = TRUE))
        }
      }
      return(NULL)
    }

    # Helper function to create calendar events
    create_calendar_events <- function(pluggings) {
      if (nrow(pluggings) == 0) {
        return(data.frame())
      }
      
      events_list <- list()
      
      for (i in seq_len(nrow(pluggings))) {
        row <- pluggings[i, ]
        
        # Skip if required fields are missing
        if (is.null(row$asu_id) || is.na(row$asu_id) || 
            is.null(row$plugging_status) || is.na(row$plugging_status)) {
          next
        }
        
        # Calculate base date
        base_date_result <- calculate_base_date(
          row$plugging_status, 
          row$plug_observed_date, 
          row$pairing_start_date,
          row$pairing_end_date
        )
        
        if (is.null(base_date_result)) {
          next
        }
        
        base_date <- base_date_result$date
        is_estimated <- base_date_result$is_estimated
        
        # Determine color
        if (row$plugging_status == "Not Observed (Waiting for confirmation)") {
          color <- "gray"
        } else if (is_estimated) {
          color <- "#FFEB3B"  # Light yellow for estimated dates
        } else {
          color <- get_color_for_asu(row$asu_id)
        }
        
        # Parse expected ages
        expected_ages <- parse_expected_ages(row$expected_age_for_harvesting)
        
        # Create events for each expected age
        for (age in expected_ages) {
          harvest_date <- base_date + floor(age)
          
          if (!is.na(harvest_date)) {
            event <- data.frame(
              asu_id = row$asu_id,
              label = paste0(row$asu_id, " @E", floor(age) + 0.5),
              day = as.integer(format(harvest_date, "%d")),
              date = harvest_date,
              target_month = as.integer(format(harvest_date, "%m")),
              target_year = as.integer(format(harvest_date, "%Y")),
              color = color,
              status = row$plugging_status,
              is_estimated = is_estimated,
              stringsAsFactors = FALSE
            )
            events_list[[length(events_list) + 1]] <- event
          }
        }
      }
      
      if (length(events_list) == 0) {
        return(data.frame())
      }
      
      return(do.call(rbind, events_list))
    }

    # Reactive: Get plugging events with observed dates
    plugging_data <- reactive({
      con <- NULL
      tryCatch({
        con <- dbConnect(SQLite(), db_path)
        query <- "
          SELECT 
            ph.female_id, 
            ph.plug_observed_date, 
            ph.expected_age_for_harvesting, 
            ph.plugging_status, 
            ph.pairing_start_date, 
            ph.pairing_end_date,
            ms.asu_id 
          FROM plugging_history ph 
          LEFT JOIN mice_stock ms ON ph.female_id = ms.asu_id 
          WHERE 
            ms.status = 'Alive'
            AND (
              (ph.plug_observed_date IS NOT NULL AND ph.plug_observed_date != '') 
              OR 
              (ph.plugging_status IN ('Plugged', 'Plug Confirmed') 
               AND ph.pairing_end_date IS NOT NULL AND ph.pairing_end_date != '')
              OR 
              (ph.plugging_status = 'Not Observed (Waiting for confirmation)' 
               AND ph.pairing_start_date IS NOT NULL AND ph.pairing_start_date != '')
            )
        "
        result <- dbGetQuery(con, query)
        
        if (nrow(result) > 0) {
          # Use safe date parsing for all date columns
          result$plug_observed_date <- sapply(result$plug_observed_date, safe_parse_date)
          result$pairing_start_date <- sapply(result$pairing_start_date, safe_parse_date)
          result$pairing_end_date <- sapply(result$pairing_end_date, safe_parse_date)
        }
        
        return(result)
      }, error = function(e) {
        warning("Error fetching plugging data: ", e$message)
        return(data.frame())
      }, finally = {
        if (!is.null(con)) dbDisconnect(con)
      })
    })

    # Reactive: Calculate all calendar events (cached for performance)
    all_calendar_events <- reactive({
      pluggings <- plugging_data()
      create_calendar_events(pluggings)
    })

    # Reactive: Filter events for current month/year
    current_month_events <- reactive({
      req(input$cal_month, input$cal_year)
      
      month_num <- match(input$cal_month, month.name)
      year_num <- input$cal_year
      
      events <- all_calendar_events()
      
      if (nrow(events) == 0) {
        return(data.frame())
      }
      
      # Filter for current month/year and validate days
      filtered <- events[
        events$target_month == month_num & 
        events$target_year == year_num & 
        !is.na(events$day) & 
        events$day > 0 & 
        events$day <= 31, 
      ]
      
      return(filtered)
    })

    # Render the calendar plot
    output$calendar_plot <- renderPlot({
      req(input$cal_month, input$cal_year)
      
      month_num <- match(input$cal_month, month.name)
      year_num <- input$cal_year
      events <- current_month_events()
      
      if (nrow(events) == 0) {
        calendR(
          year = year_num, 
          month = month_num, 
          title = paste("Plugging Calendar -", input$cal_month, year_num)
        )
        return()
      }
      
      # Create calendar with events
      calendR(
        year = year_num,
        month = month_num,
        special.days = events$day,
        special.col = events$color,
        text = events$label,
        text.pos = events$day,
        text.size = 4,
        text.col = "black",
        title = paste("Plugging Calendar -", input$cal_month, year_num)
      )
    })

    # Render calendar legend
    output$calendar_legend <- renderUI({
      req(input$cal_month, input$cal_year)
      
      events <- current_month_events()
      if (nrow(events) == 0) {
        return(NULL)
      }
      
      # Get unique plugged events (non-gray colors)
      plugged_events <- events[events$color != "gray", ]
      if (nrow(plugged_events) == 0) {
        return(tags$div(
          tags$p(
            style = "margin-top: 10px; font-size: 12px;",
            tags$span(style = "color: gray;", "Gray: "), 
            "Not Observed (Waiting for confirmation) status (estimated from pairing date + 1 day)"
          )
        ))
      }
      
      # Get unique asu_ids for plugged events
      unique_plugged <- plugged_events[!duplicated(plugged_events$asu_id), ]
      
      # Get plug dates from original data
      pluggings <- plugging_data()
      plug_dates <- pluggings[
        pluggings$asu_id %in% unique_plugged$asu_id & 
        pluggings$plugging_status %in% c("Plugged", "Plug Confirmed"), 
        c("asu_id", "plug_observed_date", "plugging_status", "pairing_end_date")
      ]
      
      # Merge with unique plugged events
      if (nrow(plug_dates) > 0) {
        unique_plugged <- merge(unique_plugged, plug_dates, by = "asu_id", all.x = TRUE)
      }
      
      # Separate confirmed and estimated events
      confirmed_events <- unique_plugged[unique_plugged$color != "#FFEB3B", ]
      estimated_events <- unique_plugged[unique_plugged$color == "#FFEB3B", ]
      
      legend_items <- list()
      
      # Add confirmed events
      if (nrow(confirmed_events) > 0) {
        legend_items[[length(legend_items) + 1]] <- tags$p(
          style = "margin-top: 10px; font-size: 12px;",
          tags$b("Confirmed Plugged Mice:")
        )
        
        legend_items[[length(legend_items) + 1]] <- tags$ul(
          style = "list-style: none; padding: 0; margin: 10px 0;",
          lapply(seq_len(nrow(confirmed_events)), function(i) {
            plug_date_text <- if (!is.na(confirmed_events$plug_observed_date[i]) && 
                                 !is.null(confirmed_events$plug_observed_date[i])) {
              tryCatch({
                format(as.Date(confirmed_events$plug_observed_date[i]), "%d-%b")
              }, error = function(e) {
                "Unknown"
              })
            } else {
              "Unknown"
            }
            tags$li(
              style = paste0("color: ", confirmed_events$color[i], "; margin: 5px 0;"),
              paste0(confirmed_events$asu_id[i], " (", confirmed_events$plugging_status[i], ") @ ", plug_date_text)
            )
          })
        )
      }
      
      # Add estimated events
      if (nrow(estimated_events) > 0) {
        legend_items[[length(legend_items) + 1]] <- tags$p(
          style = "margin-top: 10px; font-size: 12px;",
          tags$b("Estimated Plugged Mice:")
        )
        
        legend_items[[length(legend_items) + 1]] <- tags$ul(
          style = "list-style: none; padding: 0; margin: 10px 0;",
          lapply(seq_len(nrow(estimated_events)), function(i) {
            pairing_end_text <- if (!is.na(estimated_events$pairing_end_date[i]) && 
                                   !is.null(estimated_events$pairing_end_date[i])) {
              tryCatch({
                format(as.Date(estimated_events$pairing_end_date[i]), "%d-%b")
              }, error = function(e) {
                "Unknown"
              })
            } else {
              "Unknown"
            }
            tags$li(
              style = paste0("color: #FFEB3B; margin: 5px 0;"),
              paste0(estimated_events$asu_id[i], " (", estimated_events$plugging_status[i], ") estimated from pairing end @ ", pairing_end_text)
            )
          })
        )
      }
      
      # Add gray events explanation
      legend_items[[length(legend_items) + 1]] <- tags$p(
        style = "margin-top: 10px; font-size: 12px;",
        tags$span(style = "color: gray;", "Gray: "), 
        "Not Observed (Waiting for confirmation) status (estimated from pairing start date + 1 day)"
      )
      
      # Add light yellow explanation
      legend_items[[length(legend_items) + 1]] <- tags$p(
        style = "margin-top: 10px; font-size: 12px;",
        tags$span(style = "color: #FFEB3B;", "Light Yellow: "), 
        "Plugged/Plug Confirmed mice with estimated dates (no plug observed date, using pairing end date)"
      )
      
      do.call(tagList, legend_items)
    })
    
    # Function to generate iCal content
    generate_ical_content <- function(events, month_name, year) {
      if (nrow(events) == 0) {
        return("")
      }
      
      # iCal header
      ical_content <- c(
        "BEGIN:VCALENDAR",
        "VERSION:2.0",
        "PRODID:-//Mouse Management System//Calendar Export//EN",
        "CALSCALE:GREGORIAN",
        "METHOD:PUBLISH"
      )
      
      # Generate unique ID for each event
      uid_counter <- 1
      
      for (i in seq_len(nrow(events))) {
        event <- events[i, ]
        
        # Format date for iCal (YYYYMMDD format)
        event_date <- format(event$date, "%Y%m%d")
        
        # Create unique identifier
        uid <- paste0("mouse-harvest-", uid_counter, "@mousemanagement.local")
        uid_counter <- uid_counter + 1
        
        # Determine event description based on status and estimation
        description <- paste0(
          "Mouse ID: ", event$asu_id, "\n",
          "Status: ", event$status, "\n",
          "Harvest Age: ", gsub("@E", "E", event$label), "\n"
        )
        
        if (event$is_estimated) {
          description <- paste0(description, "Note: Date estimated (no plug observed date)\n")
        }
        
        # Create iCal event
        ical_event <- c(
          "BEGIN:VEVENT",
          paste0("UID:", uid),
          paste0("DTSTART;VALUE=DATE:", event_date),
          paste0("DTEND;VALUE=DATE:", event_date),
          paste0("SUMMARY:", event$label),
          paste0("DESCRIPTION:", gsub("\n", "\\n", description)),
          paste0("CATEGORIES:Mouse Harvest"),
          "STATUS:CONFIRMED",
          "SEQUENCE:0",
          paste0("DTSTAMP:", format(Sys.time(), "%Y%m%dT%H%M%SZ")),
          "END:VEVENT"
        )
        
        ical_content <- c(ical_content, ical_event)
      }
      
      # iCal footer
      ical_content <- c(ical_content, "END:VCALENDAR")
      
      return(paste(ical_content, collapse = "\r\n"))
    }
    
    # Download handler for iCal export
    output$export_ical_btn <- downloadHandler(
      filename = function() {
        month_num <- match(input$cal_month, month.name)
        year_num <- input$cal_year
        paste0("mouse_harvest_calendar_", sprintf("%02d", month_num), "_", year_num, ".ics")
      },
      content = function(file) {
        events <- current_month_events()
        ical_content <- generate_ical_content(events, input$cal_month, input$cal_year)
        writeLines(ical_content, file)
      }
    )
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
