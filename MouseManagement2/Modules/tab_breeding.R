breeding_tab_ui <- function() {
  fluidPage(
    h3("Breeding Management"),
    actionButton("show_breeding_modal_btn", "Add Breeding Event", class = "btn-primary", style = "margin-bottom: 5px; margin-right: 10px;"),
    div(
      tags$i(tags$b("Record Litters by double-clicking Breeding Events.")),
      style = "margin-bottom: 20px; color: #888; font-size: 14px; text-align: left;"
    ),
    hr(),
    h4("Breeding History"),
    checkboxInput("show_all_breeding_history", "Show all breeding records", value = FALSE),
    DT::dataTableOutput("breeding_history_table")
  )
}

breeding_tab_server <- function(input, output, session) {
  # Helper: Get live mice for selection
  get_live_mice <- function(gender) {
    con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
    mice <- DBI::dbGetQuery(con, paste0(
      "SELECT asu_id, animal_id, gender FROM mice_stock WHERE status = 'Alive' AND gender = '", gender, "' ORDER BY asu_id"))
    DBI::dbDisconnect(con)
    if (nrow(mice) == 0) return(character(0))
    choices <- setNames(mice$asu_id, mice$asu_id)
    return(choices)
  }

  # Show breeding modal
  observeEvent(input$show_breeding_modal_btn, {
    males <- get_live_mice("Male")
    females <- get_live_mice("Female")
    showModal(modalDialog(
      title = "Add Breeding Event",
      size = "m",
      tagList(
        selectInput("breeding_male", "Male (ASU ID)", choices = if(length(males) > 0) males else c("No live males available" = "")),
        uiOutput("male_info_panel"),
        selectInput("breeding_female1", "Female 1 (ASU ID)", choices = if(length(females) > 0) females else c("No live females available" = "")),
        uiOutput("female1_info_panel"),
        selectInput("breeding_female2", "Female 2 (ASU ID, optional)", choices = c("", if(length(females) > 0) females else character(0))),
        uiOutput("female2_info_panel"),
        textInput("breeding_cage", "Cage ID"),
        dateInput("breeding_start", "Start Date", value = Sys.Date()),
        dateInput("breeding_end", "End Date", value = NA),
        textAreaInput("breeding_notes", "Notes", "", rows = 2)
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("add_breeding_btn", "Add Breeding Event", class = "btn-primary")
      )
    ))
  })

  # Helper to get mouse info
  get_mouse_info <- function(asu_id) {
    if (is.null(asu_id) || asu_id == "") return(NULL)
    con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
    mouse <- DBI::dbGetQuery(con, paste0(
      "SELECT asu_id, dob, breeding_line, genotype FROM mice_stock WHERE asu_id = '", asu_id, "' LIMIT 1"))
    DBI::dbDisconnect(con)
    if (nrow(mouse) == 0) return(NULL)
    # Safe age calculation
    mouse$age_weeks <- tryCatch({
      if (is.na(mouse$dob) || is.null(mouse$dob) || mouse$dob == "") {
        NA_real_
      } else {
        round(as.numeric(Sys.Date() - as.Date(mouse$dob)) / 7, 1)
      }
    }, error = function(e) {
      NA_real_
    })
    return(mouse)
  }

  output$male_info_panel <- renderUI({
    info <- get_mouse_info(input$breeding_male)
    if (is.null(info)) return(NULL)
    wellPanel(
      tags$b("Selected Male Info:"), br(),
      paste("ASU ID:", info$asu_id), br(),
      paste("Age (weeks):", info$age_weeks), br(),
      paste("Breeding Line:", info$breeding_line), br(),
      paste("Genotype:", info$genotype)
    )
  })
  output$female1_info_panel <- renderUI({
    info <- get_mouse_info(input$breeding_female1)
    if (is.null(info)) return(NULL)
    wellPanel(
      tags$b("Selected Female 1 Info:"), br(),
      paste("ASU ID:", info$asu_id), br(),
      paste("Age (weeks):", info$age_weeks), br(),
      paste("Breeding Line:", info$breeding_line), br(),
      paste("Genotype:", info$genotype)
    )
  })
  output$female2_info_panel <- renderUI({
    info <- get_mouse_info(input$breeding_female2)
    if (is.null(info)) return(NULL)
    wellPanel(
      tags$b("Selected Female 2 Info:"), br(),
      paste("ASU ID:", info$asu_id), br(),
      paste("Age (weeks):", info$age_weeks), br(),
      paste("Breeding Line:", info$breeding_line), br(),
      paste("Genotype:", info$genotype)
    )
  })

  # Add breeding event
  observeEvent(input$add_breeding_btn, {
    req(input$breeding_male, input$breeding_female1, input$breeding_start)
    con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
    DBI::dbExecute(con, "INSERT INTO breeding_history (male_id, female1_id, female2_id, cage_id, start_date, end_date, notes) VALUES (?, ?, ?, ?, ?, ?, ?)",
      params = list(
        input$breeding_male,
        input$breeding_female1,
        ifelse(input$breeding_female2 == "", NA, input$breeding_female2),
        input$breeding_cage,
        as.character(input$breeding_start),
        ifelse(is.null(input$breeding_end) || is.na(input$breeding_end), NA, as.character(input$breeding_end)),
        input$breeding_notes
      )
    )
    # Get last inserted id
    new_id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid()")[[1]]
    log_audit_action(con, "breeding_history", "INSERT", new_id, list(
      male_id = input$breeding_male,
      female1_id = input$breeding_female1,
      female2_id = ifelse(input$breeding_female2 == "", NA, input$breeding_female2),
      cage_id = input$breeding_cage,
      start_date = as.character(input$breeding_start),
      end_date = ifelse(is.null(input$breeding_end) || is.na(input$breeding_end), NA, as.character(input$breeding_end)),
      notes = input$breeding_notes
    ), user = 'system')
    DBI::dbDisconnect(con)
    showNotification("Breeding event added!", type = "message")
    removeModal()
    breeding_history_rv$reload <- Sys.time()
  })

  # Breeding history table (with litters)
  breeding_history_rv <- reactiveValues(reload = NULL)
  output$breeding_history_table <- DT::renderDataTable({
    breeding_history_rv$reload
    con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
    breedings <- DBI::dbGetQuery(con, "SELECT * FROM breeding_history ORDER BY id DESC")
    litters <- DBI::dbGetQuery(con, "SELECT * FROM litter")
    DBI::dbDisconnect(con)
    # Mark ongoing events and add green button
    breedings$Breeding_Status_Display <- ifelse(is.null(breedings$breeding_status) | is.na(breedings$breeding_status) | breedings$breeding_status == "", "Ongoing", breedings$breeding_status)
    breedings$Ongoing <- ifelse(breedings$Breeding_Status_Display == "Ongoing",
      '<button style="background-color:#43a047;border:none;border-radius:50%;width:18px;height:18px;vertical-align:middle;margin-right:6px;"></button>',
      "")
    breedings$End_Date_Display <- ifelse(is.na(breedings$end_date) | breedings$end_date == "" | is.null(breedings$end_date),
      "Ongoing",
      as.character(breedings$end_date)
    )
    breedings$Status_Icon <- vapply(breedings$Breeding_Status_Display, function(status) {
      ## only Remove show Red color, Onging with Green color, Waiting for Pups with Yellow color, End Pairing with Yellow color
      if (status == "Ongoing") {
        '<button title="Ongoing" style="background-color:#43a047;border:none;border-radius:50%;width:18px;height:18px;vertical-align:middle;margin-right:6px;" ></button>'
      } else if (status == "Waiting for Pups") {
        '<button title="Waiting for Pups" style="background-color:#fbc02d;border:none;border-radius:50%;width:18px;height:18px;vertical-align:middle;margin-right:6px;"></button>'
      } else if (status == "Remove") {
        '<button title="Remove" style="background-color:#d32f2f;border:none;border-radius:50%;width:18px;height:18px;vertical-align:middle;margin-right:6px;"></button>'
      } else if (status == "End Pairing") {
        '<button title="End Pairing" style="background-color:#fbc02d;border:none;border-radius:50%;width:18px;height:18px;vertical-align:middle;margin-right:6px;"></button>'
      } else {
        ''
      }
    }, character(1))
    # Filter by status: if checkbox not selected, show only 3 statuses except Remove; if selected, show all
    show_all <- isTRUE(input$show_all_breeding_history)
    if (!show_all) {
      breedings <- breedings[breedings$Breeding_Status_Display %in% c("Ongoing", "Waiting for Pups", "End Pairing"), ]
    }
    # Reshape for mouse-per-row display
    mouse_rows <- do.call(rbind, lapply(seq_len(nrow(breedings)), function(i) {
      b <- breedings[i, ]
      # Get all litters for this breeding event
      lit <- litters[litters$breeding_id == b$id, ]
      # Helper to format litters for a given female
      format_litters <- function(lit_df) {
        if (nrow(lit_df) == 0) return("")
        paste0(apply(lit_df, 1, function(row) paste0(
          "DOB: ", row["dob"], ", Pups: ", row["num_pups"],
          ##ifelse(!is.null(row["female_id"]) && row["female_id"] != "", paste0(", Female: ", row["female_id"]), ""),
          ifelse(!is.null(row["notes"]) && row["notes"] != "", paste0(" (", row["notes"], ")"), "")
        )), collapse = "<br>")
      }
      rows <- list(
        data.frame(
          group_id = b$id,
          role = "Male",
          mouse_id = b$male_id,
          Status_Icon = b$Status_Icon,
          cage_id = b$cage_id,
          start_date = b$start_date,
          End_Date_Display = b$End_Date_Display,
          Breeding_Status_Display = b$Breeding_Status_Display,
          notes = b$notes,
          Litters = "",
          stringsAsFactors = FALSE
        ),
        data.frame(
          group_id = b$id,
          role = "Female 1",
          mouse_id = b$female1_id,
          Status_Icon = b$Status_Icon,
          cage_id = b$cage_id,
          start_date = b$start_date,
          End_Date_Display = b$End_Date_Display,
          Breeding_Status_Display = b$Breeding_Status_Display,
          notes = b$notes,
          Litters = format_litters(lit[lit$female_id == b$female1_id, , drop=FALSE]),
          stringsAsFactors = FALSE
        )
      )
      if (!is.na(b$female2_id) && b$female2_id != "") {
        rows[[3]] <- data.frame(
          group_id = b$id,
          role = "Female 2",
          mouse_id = b$female2_id,
          Status_Icon = b$Status_Icon,
          cage_id = b$cage_id,
          start_date = b$start_date,
          End_Date_Display = b$End_Date_Display,
          Breeding_Status_Display = b$Breeding_Status_Display,
          notes = b$notes,
          Litters = format_litters(lit[lit$female_id == b$female2_id, , drop=FALSE]),
          stringsAsFactors = FALSE
        )
      }
      do.call(rbind, rows)
    }))
    # Join mouse info for each row
    con2 <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
    mice_info <- DBI::dbGetQuery(con2, "SELECT asu_id, dob, breeding_line, genotype FROM mice_stock")
    DBI::dbDisconnect(con2)
    
    # Defensive checks before merge
    if (nrow(mouse_rows) == 0) {
      # If no mouse rows, create empty result with required columns
      mouse_rows$dob <- character(0)
      mouse_rows$breeding_line <- character(0)
      mouse_rows$genotype <- character(0)
    } else {
      # Remove any rows with NULL or empty mouse_id
      mouse_rows <- mouse_rows[!is.null(mouse_rows$mouse_id) & !is.na(mouse_rows$mouse_id) & mouse_rows$mouse_id != "", , drop = FALSE]
      
      # Ensure mice_info has unique asu_id values
      mice_info <- mice_info[!duplicated(mice_info$asu_id), , drop = FALSE]
      
      # Only proceed with merge if we have valid data
      if (nrow(mouse_rows) > 0 && nrow(mice_info) > 0) {
        mouse_rows <- merge(mouse_rows, mice_info, by.x = "mouse_id", by.y = "asu_id", all.x = TRUE)
      } else {
        # Add empty columns if no data to merge
        mouse_rows$dob <- NA_character_
        mouse_rows$breeding_line <- NA_character_
        mouse_rows$genotype <- NA_character_
      }
    }
    # Calculate age in weeks
    mouse_rows$Age <- sapply(mouse_rows$dob, function(dob) {
      if (is.na(dob) || is.null(dob) || dob == "") {
        return(NA_real_)
      }
      tryCatch({
        round(as.numeric(Sys.Date() - as.Date(dob)) / 7, 1)
      }, error = function(e) {
        return(NA_real_)
      })
    })
    # Insert empty row between each group, always put male at top and color it gray
    mouse_rows_with_gaps <- do.call(rbind, lapply(split(mouse_rows, mouse_rows$group_id), function(df) {
      # Ensure male is first
      male_row <- df[df$role == "Male", , drop=FALSE]
      other_rows <- df[df$role != "Male", , drop=FALSE]
      out <- rbind(male_row, other_rows)
      rbind(out, out[1,][NA,])
    }))
    DT::datatable(
      mouse_rows_with_gaps[, c("group_id", "Status_Icon", "role", "mouse_id", "Age", "breeding_line", "genotype", "cage_id", "start_date", "End_Date_Display", "Breeding_Status_Display", "notes", "Litters")],
      options = list(
        pageLength = 50,
        scrollX = TRUE,
        columnDefs = list(list(visible = FALSE, targets = 0)),  # Hide group_id
        rowCallback = JS(
          'function(row, data, index) {',
          '  if(data[2] === "Male") {',
          '    $(row).css("background-color", "#fafafa");',
          '  } else {',
          '    var color = (parseInt(data[1] && data[1].replace ? data[1].replace(/\\D/g, "") : "0") % 2 == 0) ? "#f9fbe7" : "#e3f2fd";',
          '    if(data[2] === null || data[2] === "") { $(row).css("background-color", "#fff"); } else { $(row).css("background-color", color); }',
          '  }',
          '}'
        )
      ),
      escape = FALSE,
      rownames = FALSE,
      colnames = c("", "Role", "Mouse ID", "Age (wks)", "Breeding Line", "Genotype", "Cage", "Start Date", "End Date", "Breeding Status", "Notes", "Litters"),
      selection = "single",
      callback = JS(
        'table.on("dblclick", "tr", function() {',
        '  var data = table.row(this).data();',
        '  if(data !== undefined) {',
        '    Shiny.setInputValue("breeding_history_table_row_dblclicked_info", {group_id: data[0], mouse_id: data[3]}, {priority: "event"});',
        '  }',
        '});'
      )
    )
  })

  observeEvent(input$breeding_history_table_row_dblclicked_info, {
    info <- input$breeding_history_table_row_dblclicked_info
    if (is.null(info$group_id) || is.null(info$mouse_id)) return()
    session$userData$editing_breeding_id <- info$group_id
    session$userData$editing_mouse_id <- info$mouse_id
    # Open the edit modal as before (using group_id)
    con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
    breedings <- DBI::dbGetQuery(con, "SELECT * FROM breeding_history WHERE id = ?", params = list(info$group_id))
    litters <- DBI::dbGetQuery(con, "SELECT * FROM litter WHERE breeding_id = ?", params = list(info$group_id))
    DBI::dbDisconnect(con)
    row <- breedings[1, ]
    # If male, show all litters for the group; if female, show only her litters
    if (info$mouse_id == row$male_id) {
      litters_to_show <- litters
      litters_title <- "Litters for this Breeding Group:"
    } else {
      litters_to_show <- litters[litters$female_id == info$mouse_id, , drop=FALSE]
      litters_title <- "Litters for this Mouse:"
    }
    litter_ui <- if (is.null(litters_to_show) || nrow(litters_to_show) == 0) {
      div("No litters recorded.", style = "color: #888; font-size: 13px;")
    } else {
      tagList(
        tags$h4(litters_title),
        DT::dataTableOutput("edit_litter_table")
      )
    }
    # Robust end date for dateInput
    end_date_val <- row$end_date
    if (is.na(end_date_val) || isTRUE(end_date_val == "") || is.null(end_date_val)) end_date_val <- NULL
    
    # Robust breeding status handling
    breeding_status_val <- row$breeding_status
    if (is.null(breeding_status_val) || is.na(breeding_status_val) || breeding_status_val == "") {
      breeding_status_val <- "Ongoing"
    }
    
    # Robust notes handling
    notes_val <- row$notes
    if (is.null(notes_val) || is.na(notes_val)) {
      notes_val <- ""
    }
    
    showModal(modalDialog(
      title = paste("Edit Breeding Event (", row$id, ")"),
      size = "m",
      tagList(
        dateInput("edit_end_date", "End Date", value = end_date_val),
        selectInput("edit_breeding_status", "Breeding Status", 
                   choices = c("Ongoing", "End Pairing", "Waiting for Pups", "Remove"), 
                   selected = breeding_status_val),
        textAreaInput("edit_notes", "Notes", value = notes_val, rows = 2),
        tags$hr(),
        litter_ui
      ),
      footer = tagList(
        fluidRow(
          column(6, actionButton("edit_modal_add_litter_btn", "Add Litter", class = "btn-success", style = "float:left;")),
          column(6, div(style = "text-align:right;", modalButton("Cancel"), actionButton("save_breeding_edit_btn", "Save Changes", class = "btn-primary")))
        )
      )
    ))
    session$userData$edit_litters <- litters
    output$edit_litter_table <- DT::renderDataTable({
      litters <- session$userData$edit_litters
      if (is.null(litters) || nrow(litters) == 0) return(NULL)
      litters$Edit <- vapply(litters$id, function(lid) {
        sprintf('<a href="#" class="edit-litter-link" data-lid="%s">Edit</a>', lid)
      }, character(1))
      litters$Delete <- vapply(litters$id, function(lid) {
        sprintf('<a href="#" class="delete-litter-link" data-lid="%s" style="color:red;">Delete</a>', lid)
      }, character(1))
      DT::datatable(litters[, c("dob", "num_pups", "female_id", "male_id", "notes", "Edit", "Delete")],
        colnames = c("Date of Birth", "Number of Pups", "Female", "Male", "Notes", "", ""),
        rownames = FALSE,
        escape = FALSE,
        selection = "none",
        options = list(dom = 't', paging = FALSE, ordering = FALSE, columnDefs = list(list(width = '80px', targets = 5:6))),
        callback = JS(
          "table.on('click', 'a.edit-litter-link', function() {",
          "  var lid = $(this).data('lid');",
          "  Shiny.setInputValue('edit_litter_link_clicked', lid, {priority: 'event'});",
          "});",
          "table.on('click', 'a.delete-litter-link', function() {",
          "  var lid = $(this).data('lid');",
          "  Shiny.setInputValue('delete_litter_link_clicked', lid, {priority: 'event'});",
          "});"
        )
      )
    })
  })

  observeEvent(input$edit_litter_link_clicked, {
    lid <- input$edit_litter_link_clicked
    litters <- session$userData$edit_litters
    if (is.null(lid) || is.null(litters) || nrow(litters) == 0) return()
    showModal(modalDialog(
      title = "Edit Litter",
      size = "s",
      dateInput("edit_litter_dob", "Litter Date of Birth", value = litters[litters$id == lid, "dob"]),
      numericInput("edit_litter_num_pups", "Number of Pups", value = litters[litters$id == lid, "num_pups"], min = 1),
      textAreaInput("edit_litter_notes", "Litter Notes", litters[litters$id == lid, "notes"], rows = 2),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_litter_edit_btn", "Save Changes", class = "btn-primary")
      )
    ))
    session$userData$editing_litter_id <- lid
  })

  observeEvent(input$save_litter_edit_btn, {
    lid <- session$userData$editing_litter_id
    if (is.null(lid)) return()
    con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
    # Get old values
    old_row <- DBI::dbGetQuery(con, "SELECT * FROM litter WHERE id = ?", params = list(lid))
    DBI::dbExecute(con, "UPDATE litter SET dob = ?, num_pups = ?, notes = ? WHERE id = ?",
      params = list(
        as.character(input$edit_litter_dob),
        input$edit_litter_num_pups,
        input$edit_litter_notes,
        lid
      )
    )
    # Get new values
    new_row <- DBI::dbGetQuery(con, "SELECT * FROM litter WHERE id = ?", params = list(lid))
    # Find changed fields
    changed <- list()
    for (col in c("dob", "num_pups", "notes")) {
      if (!identical(old_row[[col]], new_row[[col]])) changed[[col]] <- list(before=old_row[[col]], after=new_row[[col]])
    }
    if (length(changed) > 0) log_audit_action(con, "litter", "UPDATE", lid, changed, user = 'system')
    DBI::dbDisconnect(con)
    removeModal()
    showNotification("Litter updated!", type = "message")
    breeding_history_rv$reload <- Sys.time()
  })

  observeEvent(input$delete_litter_link_clicked, {
    lid <- input$delete_litter_link_clicked
    showModal(modalDialog(
      title = "Delete Litter",
      "Are you sure you want to delete this litter? This action cannot be undone.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_litter_btn", "Delete", class = "btn-danger")
      )
    ))
    session$userData$deleting_litter_id <- lid
  })

  observeEvent(input$confirm_delete_litter_btn, {
    lid <- session$userData$deleting_litter_id
    group_id <- session$userData$editing_breeding_id
    mouse_id <- session$userData$editing_mouse_id
    if (is.null(lid)) return()
    con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
    # Get old values for audit
    old_row <- DBI::dbGetQuery(con, "SELECT * FROM litter WHERE id = ?", params = list(lid))
    DBI::dbExecute(con, "DELETE FROM litter WHERE id = ?", params = list(lid))
    log_audit_action(con, "litter", "DELETE", lid, as.list(old_row[1,]), user = 'system')
    DBI::dbDisconnect(con)
    removeModal()
    showNotification("Litter deleted!", type = "message")
    # Only reopen the modal if both IDs are valid and not missing
    if (!is.null(group_id) && !is.null(mouse_id) && group_id != "" && mouse_id != "") {
      isolate({
        con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
        breedings <- DBI::dbGetQuery(con, "SELECT * FROM breeding_history WHERE id = ?", params = list(group_id))
        litters <- DBI::dbGetQuery(con, "SELECT * FROM litter WHERE breeding_id = ?", params = list(group_id))
        DBI::dbDisconnect(con)
        row <- breedings[1, ]
        litter_ui <- if (nrow(litters) == 0) {
          div("No litters recorded for this breeding event.", style = "color: #888; font-size: 13px;")
        } else {
          tagList(
            tags$h4("Litters for this Breeding Event:"),
            DT::dataTableOutput("edit_litter_table")
          )
        }
        end_date_val <- row$end_date
        if (is.na(end_date_val) || isTRUE(end_date_val == "") || is.null(end_date_val)) end_date_val <- NULL
        
        # Robust breeding status handling
        breeding_status_val <- row$breeding_status
        if (is.null(breeding_status_val) || is.na(breeding_status_val) || breeding_status_val == "") {
          breeding_status_val <- "Ongoing"
        }
        
        # Robust notes handling
        notes_val <- row$notes
        if (is.null(notes_val) || is.na(notes_val)) {
          notes_val <- ""
        }
        
        showModal(modalDialog(
          title = paste("Edit Breeding Event (", row$id, ")"),
          size = "m",
          tagList(
            dateInput("edit_end_date", "End Date", value = end_date_val),
            selectInput("edit_breeding_status", "Breeding Status", 
                       choices = c("Ongoing", "End Pairing", "Waiting for Pups", "Remove"), 
                       selected = breeding_status_val),
            textAreaInput("edit_notes", "Notes", value = notes_val, rows = 2),
            tags$hr(),
            litter_ui
          ),
          footer = tagList(
            fluidRow(
              column(6, actionButton("edit_modal_add_litter_btn", "Add Litter", class = "btn-success", style = "float:left;")),
              column(6, div(style = "text-align:right;", modalButton("Cancel"), actionButton("save_breeding_edit_btn", "Save Changes", class = "btn-primary")))
            )
          )
        ))
        session$userData$edit_litters <- litters
        output$edit_litter_table <- DT::renderDataTable({
          litters <- session$userData$edit_litters
          if (is.null(litters) || nrow(litters) == 0) return(NULL)
          litters$Edit <- vapply(litters$id, function(lid) {
            sprintf('<a href="#" class="edit-litter-link" data-lid="%s">Edit</a>', lid)
          }, character(1))
          litters$Delete <- vapply(litters$id, function(lid) {
            sprintf('<a href="#" class="delete-litter-link" data-lid="%s" style="color:red;">Delete</a>', lid)
          }, character(1))
          DT::datatable(litters[, c("dob", "num_pups", "female_id", "male_id", "notes", "Edit", "Delete")],
            colnames = c("Date of Birth", "Number of Pups", "Female", "Male", "Notes", "", ""),
            rownames = FALSE,
            escape = FALSE,
            selection = "none",
            options = list(dom = 't', paging = FALSE, ordering = FALSE, columnDefs = list(list(width = '80px', targets = 5:6))),
            callback = JS(
              "table.on('click', 'a.edit-litter-link', function() {",
              "  var lid = $(this).data('lid');",
              "  Shiny.setInputValue('edit_litter_link_clicked', lid, {priority: 'event'});",
              "});",
              "table.on('click', 'a.delete-litter-link', function() {",
              "  var lid = $(this).data('lid');",
              "  Shiny.setInputValue('delete_litter_link_clicked', lid, {priority: 'event'});",
              "});"
            )
          )
        })
      })
    }
    breeding_history_rv$reload <- Sys.time()
  })

  observeEvent(input$edit_modal_add_litter_btn, {
    id <- session$userData$editing_breeding_id
    mouse_id <- session$userData$editing_mouse_id
    if (is.null(id) || is.null(mouse_id)) return()
    con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
    breed_row <- DBI::dbGetQuery(con, "SELECT * FROM breeding_history WHERE id = ?", params = list(id))
    DBI::dbDisconnect(con)
    # Determine available females for this breeding event
    females <- c()
    female_labels <- c()
    if (!is.null(breed_row$female1_id) && !is.na(breed_row$female1_id) && breed_row$female1_id != "") {
      females <- c(females, breed_row$female1_id)
      female_labels <- c(female_labels, paste0("Female 1 (", breed_row$female1_id, ")"))
    }
    if (!is.null(breed_row$female2_id) && !is.na(breed_row$female2_id) && breed_row$female2_id != "") {
      females <- c(females, breed_row$female2_id)
      female_labels <- c(female_labels, paste0("Female 2 (", breed_row$female2_id, ")"))
    }
    if (length(females) == 0) {
      showNotification("No females available for this breeding event.", type = "error")
      return()
    }
    # Preselect and lock logic
    if (length(females) > 0 && mouse_id %in% females) {
      preselect <- mouse_id
      lock <- TRUE
    } else {
      preselect <- if(length(females) > 0) females[1] else ""
      lock <- FALSE
    }
    showModal(modalDialog(
      title = "Add Litter",
      size = "s",
      selectInput("modal_litter_female", "Assign to Female", choices = setNames(females, female_labels), selected = preselect, width = "100%"),
      dateInput("modal_litter_dob", "Litter Date of Birth", value = Sys.Date()),
      numericInput("modal_litter_num_pups", "Number of Pups", value = 1, min = 1),
      textAreaInput("modal_litter_notes", "Litter Notes", "", rows = 2),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_litter_add_btn", "Add Litter", class = "btn-primary")
      )
    ))
    session$userData$modal_litter_female_lock <- lock
  })

  # Save Litter (Add Litter modal)
  observeEvent(input$save_litter_add_btn, {
    id <- session$userData$editing_breeding_id
    if (is.null(id)) return()
    assign_to <- input$modal_litter_female
    # Defensive check: must be a single, non-empty string
    if (is.null(assign_to) || !nzchar(assign_to) || length(assign_to) != 1) {
      showNotification("Please select a female to assign the litter to.", type = "error")
      return()
    }
    con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
    breed_row <- DBI::dbGetQuery(con, "SELECT * FROM breeding_history WHERE id = ?", params = list(id))
    male_id <- breed_row$male_id
    DBI::dbExecute(con, "INSERT INTO litter (breeding_id, female_id, male_id, dob, num_pups, notes) VALUES (?, ?, ?, ?, ?, ?)",
      params = list(
        id,
        assign_to,
        male_id,
        as.character(input$modal_litter_dob),
        input$modal_litter_num_pups,
        input$modal_litter_notes
      )
    )
    # Audit log
    new_id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid()")[[1]]
    log_audit_action(con, "litter", "INSERT", new_id, list(
      breeding_id = id,
      female_id = assign_to,
      male_id = male_id,
      dob = as.character(input$modal_litter_dob),
      num_pups = input$modal_litter_num_pups,
      notes = input$modal_litter_notes
    ), user = 'system')
    DBI::dbDisconnect(con)
    removeModal()
    showNotification("Litter added!", type = "message")
    breeding_history_rv$reload <- Sys.time()
  })

  # Save changes to breeding event (Edit Breeding Event modal)
  observeEvent(input$save_breeding_edit_btn, {
    id <- session$userData$editing_breeding_id
    if (is.null(id) || id == "") return()
    con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
    # Get old values for audit
    old_row <- DBI::dbGetQuery(con, "SELECT * FROM breeding_history WHERE id = ?", params = list(id))
    # Defensive: get new values from modal inputs
    new_end_date <- input$edit_end_date
    new_status <- input$edit_breeding_status
    new_notes <- input$edit_notes
    DBI::dbExecute(con, "UPDATE breeding_history SET end_date = ?, breeding_status = ?, notes = ? WHERE id = ?",
      params = list(
        as.character(new_end_date),
        new_status,
        new_notes,
        id
      )
    )
    # Get new values for audit
    new_row <- DBI::dbGetQuery(con, "SELECT * FROM breeding_history WHERE id = ?", params = list(id))
    # Find changed fields
    changed <- list()
    for (col in c("end_date", "breeding_status", "notes")) {
      if (!identical(old_row[[col]], new_row[[col]])) changed[[col]] <- list(before=old_row[[col]], after=new_row[[col]])
    }
    if (length(changed) > 0) log_audit_action(con, "breeding_history", "UPDATE", id, changed, user = 'system')
    DBI::dbDisconnect(con)
    removeModal()
    showNotification("Breeding event updated!", type = "message")
    breeding_history_rv$reload <- Sys.time()
  })
} 