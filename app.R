#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
rm(list=ls())

# Suppress package loading messages
suppressPackageStartupMessages({
  library(shiny)
  library(shinyFiles)
  library(tidyverse)
  library(DT)
  library(calendR)
  library(ggsci)
  library(plotly)
  library(markdown)
  library(shinydashboard)
  library(httr)
  library(jsonlite)
  library(base64enc)
})

# Helper function to shorten long file paths for display
shorten_path <- function(path, keep = 2) {
  parts <- strsplit(normalizePath(path), .Platform$file.sep)[[1]]
  n <- length(parts)
  if (n <= keep + 1) {
    return(path)
  }
  paste0(parts[1], .Platform$file.sep, "...", .Platform$file.sep, 
         paste(parts[(n-keep):n], collapse = .Platform$file.sep))
}

# --- Module sourcing for development ---
# NOTE: For hot-reloading during development, source() calls are moved inside the server function below.
# For production, move these source() calls back to the global section (outside server) for better performance.
source("Modules/audit_trail.R")
source("Modules/db_check.R")
source("Modules/modal_add_animal.R")
source("Modules/modal_mice_history.R")
source("Modules/tab_all_mice.R")
#source("Modules/tab_breeding.R")
source("Modules/tab_calendar_events.R")
#source("Modules/tab_deceased.R")
#source("Modules/tab_deleted.R")
source("Modules/tab_plugging.R")
source("Modules/validation.R")
source("Modules/analytics_tracker.R")

# Initialize audit trail
initialize_audit_trail()

# Add a global reactive value for timezone
user_timezone <- reactiveVal(Sys.timezone())

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$script(src = "scroll-position.js"),  # Include our custom JS file
    tags$script(HTML("
      // Custom message handler for updating body weight table
      Shiny.addCustomMessageHandler('updateBodyWeightTable', function(message) {
        var tableContainer = document.getElementById('body_weight_records_table');
        if (tableContainer) {
          tableContainer.innerHTML = message.html;
        }
        
        // Update the record count in the footer if it exists
        var footerText = document.querySelector('.modal-footer div:first-child');
        if (footerText && footerText.textContent.includes('Total records:')) {
          footerText.textContent = 'Total records: ' + message.record_count;
        }
        
        // Update plot section visibility
        if (message.has_records !== undefined) {
          var plotSection = document.querySelector('#body_weight_preview_plot_container');
          if (plotSection && plotSection.parentElement) {
            if (message.has_records) {
              // Show plot section if records exist
              plotSection.parentElement.style.display = 'block';
            } else {
              // Hide plot section if no records
              plotSection.parentElement.style.display = 'none';
            }
          }
        }
      });
      
      // Custom message handler for updating body weight plot container
      Shiny.addCustomMessageHandler('updateBodyWeightPlotContainer', function(message) {
        var plotContainer = document.getElementById('body_weight_preview_plot_container');
        if (plotContainer && plotContainer.parentElement) {
          if (message.show_plot) {
            // Show the entire plot section
            plotContainer.parentElement.style.display = 'block';
            // Trigger a resize event to make sure plotly redraws properly
            setTimeout(function() {
              window.dispatchEvent(new Event('resize'));
            }, 100);
          } else {
            // Hide the plot section if no records
            plotContainer.parentElement.style.display = 'none';
          }
        }
      });
    ")),
    tags$style(HTML("
      /* Modern CSS Reset and Base Styles */
      * {
        box-sizing: border-box;
      }
      
      body {
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        background-color: #f8f9fa;
        color: #333;
        line-height: 1.6;
      }
      
      /* Container and Layout Optimizations */
      .container-fluid {
        padding: 0px !important;
        max-width: 1400px;
        margin: 0px auto !important;
      }
      
      /* Modern Card-based Design */
      .well {
        background: white;
        border: 1px solid #e9ecef;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
        padding: 16px !important;
        margin-bottom: 16px !important;
        transition: box-shadow 0.2s ease;
      }
      
      .well:hover {
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      }
      
      /* Enhanced Navbar Styling - Real Tab Look */
      .navbar {
        background: #f8f9fa !important;
        border: none !important;
        margin-bottom: 0px !important;
        margin-top: 0px !important;
        padding: 0 !important;
        min-height: 20px !important;
        border-radius: 8px 8px 0 0 !important;
      }
      
      /* Remove default spacing from navbarPage */
      .navbar-page {
        margin: 0 !important;
        padding: 0 !important;
        height: auto !important;
        min-height: 10px !important;
        max-height: 20px !important;
      }
      
      /* Remove default spacing from fluidRow containing title */
      .fluid-row {
        margin: 0 !important;
        padding: 0 !important;
      }
      
      /* Remove default spacing from column containing title */
      .col-sm-12 {
        margin: 0 !important;
        padding: 0 !important;
      }
      
      .navbar-nav {
        display: flex !important;
        flex-direction: row !important;
        width: 100% !important;
        margin: 0 !important;
        padding: 0 !important;
        justify-content: flex-start !important;
      }
      
      .navbar-nav > li {
        flex: 0 0 auto !important;
        margin: 0 !important;
        border-right: 1px solid #dee2e6 !important;
      }
      
      .navbar-nav > li:last-child {
        border-right: none !important;
      }
      
      .navbar-nav > li > a {
        font-size: 18px !important;
        padding: 8px 12px !important;
        line-height: 1.2 !important;
        font-weight: 500 !important;
        color: #6c757d !important;
        transition: all 0.3s ease;
        border-radius: 0 !important;
        margin: 0 !important;
        text-align: center !important;
        background: #f8f9fa !important;
        border: none !important;
        position: relative !important;
        display: flex !important;
        align-items: center !important;
        justify-content: center !important;
        text-decoration: none !important;
        min-width: 120px !important;
      }
      
      .navbar-nav > li > a:hover {
        background-color: #e9ecef !important;
        color: #495057 !important;
        transform: none !important;
        box-shadow: inset 0 -3px 0 #5F9EA0 !important;
      }
      
      .navbar-nav > li.active > a {
        background-color: white !important;
        color: #2c3e50 !important;
        font-weight: 600 !important;
        box-shadow: inset 0 -3px 0 #5F9EA0, -3px -3px 6px rgba(0,0,0,0.15), 3px -3px 6px rgba(0,0,0,0.15) !important;
        border-bottom: 3px solid #5F9EA0 !important;
        position: relative !important;
      }
      
      .navbar-nav > li.active > a::after {
        content: '';
        position: absolute;
        bottom: -2px;
        left: 0;
        right: 0;
        height: 2px;
        background: white;
        z-index: 1;
      }
      
      /* Tab content area styling */
      .tab-content {
        background: white !important;
        border: 1px solid #dee2e6 !important;
        border-top: none !important;
        border-radius: 0 0 8px 8px !important;
        padding: 10px !important;
        width: 100% !important;
        margin: 0 !important;
      }
      
      .tab-pane {
        background: transparent !important;
      }
      
      /* Hide navbar-header space completely */
      .navbar-header {
        display: none !important;
        width: 0 !important;
        height: 0 !important;
        padding: 0 !important;
        margin: 0 !important;
        overflow: hidden !important;
      }
      
      /* Alternative: hide the entire navbar-brand area */
      .navbar-brand {
        display: none !important;
        width: 0 !important;
        height: 0 !important;
        padding: 0 !important;
        margin: 0 !important;
        overflow: hidden !important;
      }
      
      /* Modern Button Styling */
      .btn {
        border-radius: 6px !important;
        font-weight: 500 !important;
        padding: 8px 16px !important;
        font-size: 14px !important;
        transition: all 0.3s ease;
        border: none !important;
        cursor: pointer;
      }
      
      .btn:hover {
        transform: translateY(-1px);
        box-shadow: 0 4px 8px rgba(0,0,0,0.15);
      }
      
      .btn-primary {
        background: linear-gradient(135deg, #5F9EA0 0%, #4682B4 100%) !important;
        color: white !important;
      }
      
      .btn-success {
        background: linear-gradient(135deg, #4caf50 0%, #388e3c 100%) !important;
        color: white !important;
      }
      
      .btn-warning {
        background: linear-gradient(135deg, #ff9800 0%, #f57c00 100%) !important;
        color: white !important;
      }
      
      .btn-danger {
        background: linear-gradient(135deg, #f44336 0%, #d32f2f 100%) !important;
        color: white !important;
      }
      
      .btn-info {
        background: linear-gradient(135deg, #2196f3 0%, #1976d2 100%) !important;
        color: white !important;
      }
      
      /* Enhanced Form Controls */
      .form-control {
        border: 2px solid #e9ecef !important;
        border-radius: 6px !important;
        padding: 8px 12px !important;
        font-size: 14px !important;
        height: auto !important;
        transition: border-color 0.3s ease, box-shadow 0.3s ease;
      }
      
      .form-control:focus {
        border-color: #5F9EA0 !important;
        box-shadow: 0 0 0 3px rgba(95, 158, 160, 0.1) !important;
      }
      
      .form-group {
        margin-bottom: 16px !important;
      }
      
      /* Enhanced DataTables */
      .dataTables_wrapper {
        margin-top: 16px !important;
      }
      
      .dataTables_wrapper .dataTables_length,
      .dataTables_wrapper .dataTables_filter,
      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate {
        margin: 8px 0 !important;
      }
      
      .dataTables_wrapper .dataTables_length select,
      .dataTables_wrapper .dataTables_filter input {
        border: 1px solid #ddd;
        border-radius: 4px;
        padding: 4px 8px;
      }
      
      /* Enhanced Table Styling */
      .table {
        border-collapse: separate;
        border-spacing: 0;
        border-radius: 8px;
        overflow: hidden;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
      }
      
      .table thead th {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        border-bottom: 2px solid #dee2e6;
        font-weight: 600;
        color: #2c3e50;
        padding: 12px 8px;
      }
      
      .table tbody tr:hover {
        background-color: #f8f9fa;
        transition: background-color 0.2s ease;
      }
      
      .table tbody td {
        padding: 10px 8px;
        border-bottom: 1px solid #e9ecef;
        vertical-align: middle;
      }
      
      /* Enhanced Pagination */
      .dataTables_paginate .paginate_button {
        border-radius: 4px !important;
        margin: 0 2px !important;
        transition: all 0.3s ease !important;
      }
      
      .dataTables_paginate .paginate_button:hover {
        background: linear-gradient(135deg, #5F9EA0 0%, #4682B4 100%) !important;
        color: white !important;
        border-color: #5F9EA0 !important;
      }
      
      .dataTables_paginate .paginate_button.current {
        background: linear-gradient(135deg, #5F9EA0 0%, #4682B4 100%) !important;
        color: white !important;
        border-color: #5F9EA0 !important;
      }
      
      /* Status Indicators */
      .status-indicator {
        display: inline-block;
        width: 12px;
        height: 12px;
        border-radius: 50%;
        margin-right: 8px;
        vertical-align: middle;
        box-shadow: 0 1px 3px rgba(0,0,0,0.2);
      }
      
      .status-free { background-color: #4caf50; }
      .status-busy { background-color: #ff9800; }
      .status-deceased { background-color: #f44336; }
      .status-unknown { background-color: #9e9e9e; }
      
      /* Loading States */
      .loading-overlay {
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;
        background: rgba(255,255,255,0.8);
        display: flex;
        align-items: center;
        justify-content: center;
        z-index: 1000;
        border-radius: 8px;
      }
      
      .spinner {
        width: 40px;
        height: 40px;
        border: 4px solid #f3f3f3;
        border-top: 4px solid #5F9EA0;
        border-radius: 50%;
        animation: spin 1s linear infinite;
      }
      
      @keyframes spin {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }
      
      /* Enhanced Typography */
      h1, h2, h3, h4, h5, h6 {
        color: #2c3e50;
        font-weight: 600;
        margin-bottom: 12px;
      }
      
      h2 { font-size: 2em; }
      h3 { font-size: 1.6em; }
      h4 { font-size: 1.3em; }
      
      /* Alert and Notification Styling */
      .alert {
        border-radius: 6px;
        border: none;
        padding: 12px 16px;
        margin-bottom: 16px;
      }
      
      .alert-success {
        background-color: #d4edda;
        color: #155724;
        border-left: 4px solid #28a745;
      }
      
      .alert-warning {
        background-color: #fff3cd;
        color: #856404;
        border-left: 4px solid #ffc107;
      }
      
      .alert-danger {
        background-color: #f8d7da;
        color: #721c24;
        border-left: 4px solid #dc3545;
      }
      
      .alert-info {
        background-color: #d1ecf1;
        color: #0c5460;
        border-left: 4px solid #17a2b8;
      }
      
      /* Responsive Grid Improvements */
      .row {
        margin-left: -8px !important;
        margin-right: -8px !important;
      }
      
      .col-sm-2, .col-sm-3, .col-sm-4, .col-sm-6, .col-sm-8, .col-sm-10, .col-sm-12 {
        padding-left: 8px !important;
        padding-right: 8px !important;
      }
      
      /* Modal Enhancements */
      .modal-content {
        border-radius: 8px;
        border: none;
        box-shadow: 0 10px 30px rgba(0,0,0,0.2);
      }
      
      .modal-header {
        background: linear-gradient(135deg, #87CEEB 0%, #5F9EA0 100%);
        color: white;
        border-radius: 8px 8px 0 0;
        border-bottom: none;
      }
      
      .modal-title {
        font-weight: 600;
      }
      
      /* Search Panel Enhancements */
      .search-panel {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        border: 1px solid #dee2e6;
        border-radius: 8px;
        padding: 16px;
        margin-bottom: 16px;
      }
      
      /* Action Button Group */
      .action-buttons {
        display: flex;
        gap: 8px;
        flex-wrap: wrap;
        align-items: center;
      }
      
      /* Footer Styling */
      .app-footer {
        background: #f8f9fa;
        border-top: 1px solid #dee2e6;
        padding: 12px 16px;
        margin-top: 32px;
        font-size: 0.9em;
        color: #6c757d;
      }
      
      /* Responsive Design */
      @media (max-width: 768px) {
        .container-fluid {
          padding: 8px !important;
        }
        
        .navbar-nav > li > a {
          font-size: 14px !important;
          padding: 8px 12px !important;
        }
        
        .btn {
          font-size: 13px !important;
          padding: 6px 12px !important;
        }
        
        .action-buttons {
          flex-direction: column;
          align-items: stretch;
        }
        
        .action-buttons .btn {
          margin-bottom: 4px;
        }
      }
      
      /* Hide Plugging Status Diagram and text on small screens */
      @media (max-width: 1500px) {
        .plugging-status-diagram-container {
          display: none !important;
        }
      }
      
      /* Custom Scrollbar */
      ::-webkit-scrollbar {
        width: 8px;
      }
      
      ::-webkit-scrollbar-track {
        background: #f1f1f1;
        border-radius: 4px;
      }
      
      ::-webkit-scrollbar-thumb {
        background: #c1c1c1;
        border-radius: 4px;
      }
      
      ::-webkit-scrollbar-thumb:hover {
        background: #a8a8a8;
      }
      
      /* Welcome page button styling */
      #welcome_search_btn {
        background: linear-gradient(135deg, rgba(76, 175, 80, 0.7) 0%, rgba(56, 142, 60, 0.7) 100%) !important;
        color: white !important;
        border: none !important;
        border-radius: 12px !important;
        font-weight: 600 !important;
        font-size: 1.8em !important;
        box-shadow: 0 6px 20px rgba(76, 175, 80, 0.2) !important;
        transition: all 0.3s ease !important;
      }
      
      #welcome_search_btn:hover {
        background: linear-gradient(135deg, rgba(69, 160, 73, 0.8) 0%, rgba(46, 125, 50, 0.8) 100%) !important;
        transform: translateY(-3px) !important;
        box-shadow: 0 8px 25px rgba(76, 175, 80, 0.3) !important;
      }
      
      #welcome_add_animals_btn {
        background: linear-gradient(135deg, rgba(33, 150, 243, 0.7) 0%, rgba(25, 118, 210, 0.7) 100%) !important;
        color: white !important;
        border: none !important;
        border-radius: 12px !important;
        font-weight: 600 !important;
        font-size: 1.8em !important;
        box-shadow: 0 6px 20px rgba(33, 150, 243, 0.2) !important;
        transition: all 0.3s ease !important;
      }
      
      #welcome_add_animals_btn:hover {
        background: linear-gradient(135deg, rgba(30, 136, 229, 0.8) 0%, rgba(21, 101, 192, 0.8) 100%) !important;
        transform: translateY(-3px) !important;
        box-shadow: 0 8px 25px rgba(33, 150, 243, 0.3) !important;
      }
    "))
  ),
  fluidRow(
    column(12, div(
      style = 'display: flex; justify-content: space-between; align-items: center; margin-bottom: 0px; padding: 20px 0;',
      h2('Mouse Mating Tracking System', style = "margin: 0; margin-bottom: 5px; font-size: 2.8em; color: #2c3e50; font-weight: 700; padding: 5px 0;"),
      div(
        style = 'display: flex; align-items: center; gap: 12px;',
        actionButton('set_timezone_btn', '🌍 Set Timezone', icon = icon('globe'), 
                    style = 'padding: 8px 16px; font-size: 14px; border-radius: 6px;'),
        uiOutput('global_lock_ui')
      )
    ))
  ),
  div(
    navbarPage(
      title = NULL,
      id = "tabs",
      tabPanel("🏠 Home", 
        div(
          style = "position: relative; display: flex; flex-direction: column; align-items: center; justify-content: center; min-height: 60vh; background: linear-gradient(135deg, rgba(135, 206, 235, 0.6) 0%, rgba(95, 158, 160, 0.6) 100%); border-radius: 12px; padding: 48px; overflow: hidden;",
          # Version number in top-left corner
          div(
            style = "position: absolute; top: 20px; left: 20px; z-index: 3;",
            uiOutput("version_info_btn_ui")
          ),
          div(
            class = "plugging-status-diagram-container",
            div(
              style = "position: absolute; bottom: 20px; right: 190px; z-index: 2; text-align: center;",
              h4("Plugging Status Flow", style = "margin: 0; color: white; font-weight: 600; font-size: 1.1em;")
            ),
            tags$img(
              src = "Plugging_status_Diagram.svg",
              style = "position: absolute; bottom: 60px; right: 0px; height: 400px; width: auto; opacity: 0.85; pointer-events: none; z-index: 1;",
              alt = "Plugging Status Diagram"
            )
          ),
          div(
            style = "position: relative; z-index: 2; width: 100%; display: flex; flex-direction: column; align-items: flex-start; justify-content: center; margin-left: 0px;",
            h3("Welcome to the Mouse Mating Tracking System", style = "text-align: left; font-size: 2.5em; color: white; margin-bottom: 32px; margin-left: 0px; font-weight: 700;"),
            div(
              style = "display: flex; justify-content: center; gap: 24px; flex-wrap: wrap; margin-bottom: 32px; margin-left: 120px;",
              actionButton("welcome_search_btn", "🔍 Search Animals", 
                          style = "font-size: 1.5em; padding: 20px 40px; background: linear-gradient(135deg, #4caf50 0%, #388e3c 100%); color: white; border: none; border-radius: 12px; font-weight: 600; box-shadow: 0 6px 20px rgba(76, 175, 80, 0.3); transition: all 0.3s ease;"),
              actionButton("welcome_add_animals_btn", "➕ Add Animals", 
                          style = "font-size: 1.5em; padding: 20px 40px; background: linear-gradient(135deg, #2196f3 0%, #1976d2 100%); color: white; border: none; border-radius: 12px; font-weight: 600; box-shadow: 0 6px 20px rgba(33, 150, 243, 0.3); transition: all 0.3s ease;")
            ),
            div(
              style = "margin-left: 40px; margin-bottom: 16px; max-width: 690px;",
              HTML('
                <span style="color: white; font-size: 1.1em; line-height: 1.4;">
                  This tool is designed to track mouse plugging records and summarize the current and past plugging histories, which is missing from the current webtools used by Animal Facility of the UoB. The defalut workflow for plugging procedure used in this tool is illustrated in the diagram (right).
                  <br><br>
                  <span style="color: white; font-size: 1em; line-height: 1.4;">
                    This project is licensed under the BSD 3-Clause License. The source code is available in 
                    <a href="https://github.com/sqianglan/MouseTracking.git" target="_blank" style="color: white; text-decoration: underline;">GitHub.</a>
                  </span>
                  <br><br>
                  <span style="color: white; font-size: 1em; line-height: 1.4;">
                    This tool is still under development. Any feedback is welcome.
                    <br>
                    <br>
                    The database is for test only, and will be reset every time opening the tool or refreshing the page.
                  </span>
                </span>
              ')
            )
          )
        )
      ),
      tabPanel("🐭 All Mice", all_mice_tab_ui()),
      tabPanel("🐭⚤🐭 Plugging", plugging_tab_ui()),
    ),
    div(
      style = "display: grid; grid-template-columns: auto 1fr auto; align-items: center; background: #f8f9fa; border-top: 1px solid #dee2e6; padding: 12px 16px; margin-top: 32px; border-radius: 0 0 8px 8px; margin-left: 0px; margin-right: 50px;",
      div(
        style = "font-size: 0.9em; color: #6c757d; font-style: italic; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;",
        paste("📊 Database:", shorten_path(DB_PATH))
      ),
      div(
        HTML('💬 Please contact <a href="mailto:qiang.lan@bristol.ac.uk" style="color: #5F9EA0; text-decoration: underline; font-weight: 500;">Qiang Lan</a>, University of Bristol for any inquiries.'),
        style = "text-align: center; font-size: 0.85em; color: #6c757d;"
      ),
      div(
        style = "font-size: 0.8em; color: #6c757d; font-style: italic; text-align: right;",
        "Copyright (c) 2025 Qiang Lan"
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Log visitor data in background
  tryCatch({
    log_visitor(session, use_github = TRUE)
  }, error = function(e) {
    message("Failed to log visitor: ", e$message)
  })
  
  # --- Hot-reloading modules for development ---
  # NOTE: These source() calls are inside server() for hot-reloading on browser refresh.
  # For production, move them back outside server() for better performance.
  # source("Modules/audit_trail.R", local = TRUE)
  # source("Modules/db_check.R", local = TRUE)
  # source("Modules/modal_mice_history.R", local = TRUE)
  # source("Modules/tab_all_mice.R", local = TRUE)
  # source("Modules/tab_plugging.R", local = TRUE)
  # initialize_enhanced_audit_trail()
  
  # Initialize reactive values for import data (minimal for module communication)
  import_data <- reactiveValues(
    df = NULL,
    mappings = NULL,
    available_columns = NULL,
    field_suggestions = NULL,
    parsed_df = NULL,
    comparison_data = NULL,
    exact_matches = NULL,
    import_duplicates = NULL,
    db_conflicts = NULL
  )
  
  # Initialize all_mice_table reactive value
  all_mice_table <- reactiveVal(NULL)
  
  # Global refresh trigger for cross-module data updates
  global_refresh_trigger <- reactiveVal(Sys.time())
  
  # Shared plugging state for cross-module communication
  shared_plugging_state <- reactiveValues(
    reload = NULL,
    viewing_id = NULL,
    editing_id = NULL,
    confirming_id = NULL
  )
  
  # Global lock system for deletion protection
  global_lock_state <- reactiveValues(
    is_locked = TRUE  # Start locked by default
  )
  
  # Global lock UI
  output$global_lock_ui <- renderUI({
    if (global_lock_state$is_locked) {
      actionButton(
        "unlock_system_btn", 
        "🔒 System Locked", 
        icon = icon("lock"), 
        class = "btn-warning",
        style = "background: linear-gradient(135deg, #ff9800 0%, #f57c00 100%); color: white; border: none; font-weight: bold; padding: 8px 16px; font-size: 14px; border-radius: 6px;"
      )
    } else {
      actionButton(
        "lock_system_btn", 
        "🔓 System Unlocked", 
        icon = icon("unlock"), 
        class = "btn-success",
        style = "background: linear-gradient(135deg, #4caf50 0%, #388e3c 100%); color: white; border: none; font-weight: bold; padding: 8px 16px; font-size: 14px; border-radius: 6px;"
      )
    }
  })
  
  # Unlock system
  observeEvent(input$unlock_system_btn, {
    showModal(modalDialog(
      title = "🔓 Unlock System",
      size = "s",
      tagList(
        div(
          style = "text-align: center; margin-bottom: 20px;",
          icon("exclamation-triangle", style = "font-size: 3em; color: #ff9800;")
        ),
        div(
          style = "margin-bottom: 15px;",
          tags$strong("Warning: Unlocking the system will enable deletion functions.")
        ),
        div(
          style = "margin-bottom: 15px;",
          "This includes:",
          tags$ul(
            tags$li("Delete plugging records"),
            tags$li("Delete mouse records"),
            tags$li("Bulk delete operations")
          )
        ),
        div(
          style = "margin-bottom: 15px;",
          tags$strong("Note: Euthanasia and Empty Plug operations are always allowed as they are legitimate procedures.")
        ),
        div(
          style = "background-color: #fff3cd; border: 1px solid #ffeaa7; padding: 10px; border-radius: 5px;",
          tags$strong("Please ensure you have proper authorization before unlocking.")
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_unlock_btn", "Unlock System", class = "btn-warning")
      )
    ))
  })
  
  # Confirm unlock
  observeEvent(input$confirm_unlock_btn, {
    global_lock_state$is_locked <- FALSE
    showNotification("🔓 System unlocked! Deletion functions are now visible.", type = "warning", duration = 4)
    removeModal()
  })
  
  # Lock system
  observeEvent(input$lock_system_btn, {
    global_lock_state$is_locked <- TRUE
    showNotification("🔒 System locked! Deletion functions are now hidden.", type = "message", duration = 4)
  })
  
  # Function to check if system is locked (for use in modules)
  is_system_locked <- function() {
    global_lock_state$is_locked
  }
  
  # Load all mice data initially
  observe({
    con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
    all_data <- DBI::dbGetQuery(con, paste0("SELECT * FROM ", TABLE_NAME, " ORDER BY asu_id"))
    DBI::dbDisconnect(con)
    all_mice_table(all_data)
  })
  
  all_mice_tab_server(input, output, session, all_mice_table, is_system_locked, global_refresh_trigger, shared_plugging_state)
  #breeding_tab_server(input, output, session)
  plugging_tab_server(input, output, session, is_system_locked, global_refresh_trigger, all_mice_table, shared_plugging_state)
  #deceased_tab_server(input, output, session)
  #deleted_tab_server(input, output, session)

  db_status <- reactiveVal(FALSE)

  check_db_connection <- function() {
    tryCatch({
      con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
      DBI::dbDisconnect(con)
      TRUE
    }, error = function(e) FALSE)
  }

  observe({
    db_status(check_db_connection())
  })

  output$db_status_btn <- renderUI({
    btn_style <- "background-color: #4CAF50; color: white; border: none; font-size: 12px; padding: 2px 10px; height: 28px;"
    btn_style_red <- "background-color: #f44336; color: white; border: none; font-size: 12px; padding: 2px 10px; height: 28px;"
    if (db_status()) {
      actionButton("reconnect_db", "Connected", style = btn_style, icon = icon("database"))
    } else {
      actionButton("reconnect_db", "Not Connected", style = btn_style_red, icon = icon("exclamation-triangle"))
    }
  })

  # Function to read version from VERSION.md
  get_version_from_file <- function() {
    tryCatch({
      version_file_path <- "VERSION.md"
      if (file.exists(version_file_path)) {
        first_line <- readLines(version_file_path, n = 1)
        # Extract version text from "Version:" to " -"
        version_match <- regmatches(first_line, regexpr("Version:[^-]+", first_line))
        if (length(version_match) > 0) {
          # Remove "Version:" prefix and trim whitespace
          version_text <- trimws(gsub("^Version:\\s*", "", version_match))
          return(paste("Version:", version_text))
        }
      }
      return("Version: Unknown")  # fallback
    }, error = function(e) {
      return("Version: Error")  # fallback on error
    })
  }

  # Render version info button with dynamic version
  output$version_info_btn_ui <- renderUI({
    version_text <- get_version_from_file()
    actionButton("version_info_btn", version_text, 
                style = "background: rgba(255, 255, 255, 0.9); color: #2c3e50; border: none; border-radius: 6px; font-size: 0.9em; padding: 6px 12px; font-weight: 500; cursor: pointer; box-shadow: 0 2px 4px rgba(0,0,0,0.1);")
  })

  observeEvent(input$reconnect_db, {
    db_status(check_db_connection())
    showModal(modalDialog(
      title = "Database Connection",
      if (db_status()) {
        "Successfully connected to the database."
      } else {
        "Failed to connect to the database."
      },
      easyClose = TRUE
    ))
  })

  # Add Animals Modal Logic
  observeEvent(input$welcome_add_animals_btn, {
    show_add_animals_modal()
  })

  # Show Single Entry Form
  observeEvent(input$add_single_entry_btn, {
    show_single_entry_form(DB_PATH, TABLE_NAME)
  })

  # Show Import from Excel UI
  observeEvent(input$add_import_excel_btn, {
    show_import_excel_modal()
  })

  # Handle Excel Import
  observeEvent(input$submit_import_excel_btn, {
    import_data <- handle_excel_import(input, import_data)
  })
  
  
  # Initialize modal add animal server module
  modal_add_animal_server(input, output, session, import_data, all_mice_table, global_refresh_trigger, DB_PATH, TABLE_NAME)

  # Handle Search Button
  observeEvent(input$welcome_search_btn, {
    # Get existing responsible persons from database for dropdown
    con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
    responsible_persons <- unique(DBI::dbGetQuery(con, paste0("SELECT DISTINCT responsible_person FROM ", TABLE_NAME, " WHERE responsible_person IS NOT NULL"))$responsible_person)
    DBI::dbDisconnect(con)
    
    showModal(modalDialog(
      title = "Search Animals",
      size = "l",
      fluidRow(
        column(6, textInput("search_asu_id", "ASU ID", placeholder = "Enter ASU ID (supports * and ? wildcards)")),
        column(6, textInput("search_animal_id", "Animal ID", placeholder = "Enter Animal ID (supports * and ? wildcards)"))
      ),
      fluidRow(
        column(6, selectInput("search_gender", "Gender", choices = c("", "Male", "Female"), selected = "")),
        column(6, textInput("search_breeding_line", "Breeding Line", placeholder = "Enter Breeding Line (supports * and ? wildcards)"))
      ),
      fluidRow(
        column(6, selectizeInput("search_responsible_person", "Responsible Person", 
                                choices = c("", responsible_persons), 
                                options = list(placeholder = "Select responsible person"))),
        column(6, selectInput("search_stock_category", "Stock Category", choices = c("", "Experiment", "Breeding", "Charles River"), selected = ""))
      ),
      fluidRow(
        column(6, selectInput("search_status", "Status", choices = c("Both", "Live", "Deceased"), selected = "Live")),
        column(6, div()) # Empty column for spacing
      ),
      div(
        style = "margin-top: 15px; font-size: 12px; color: #666;",
        "Use * for multiple characters and ? for single character wildcards"
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("execute_search_btn", "Search", style = "background-color: #1976d2; color: white; border: none;")
      )
    ))
  })

  # Handle Search Execution
  observeEvent(input$execute_search_btn, {
    # Build SQL query based on search criteria
    where_conditions <- c()
    
    if (!is.null(input$search_asu_id) && input$search_asu_id != "") {
      asu_pattern <- gsub("\\*", "%", gsub("\\?", "_", input$search_asu_id))
      where_conditions <- c(where_conditions, paste0("asu_id LIKE '", asu_pattern, "'"))
    }
    
    if (!is.null(input$search_animal_id) && input$search_animal_id != "") {
      animal_pattern <- gsub("\\*", "%", gsub("\\?", "_", input$search_animal_id))
      where_conditions <- c(where_conditions, paste0("animal_id LIKE '", animal_pattern, "'"))
    }
    
    if (!is.null(input$search_gender) && input$search_gender != "") {
      where_conditions <- c(where_conditions, paste0("gender = '", input$search_gender, "'"))
    }
    
    if (!is.null(input$search_breeding_line) && input$search_breeding_line != "") {
      breeding_pattern <- gsub("\\*", "%", gsub("\\?", "_", input$search_breeding_line))
      where_conditions <- c(where_conditions, paste0("breeding_line LIKE '", breeding_pattern, "'"))
    }
    
    if (!is.null(input$search_responsible_person) && input$search_responsible_person != "") {
      where_conditions <- c(where_conditions, paste0("responsible_person = '", input$search_responsible_person, "'"))
    }
    
    if (!is.null(input$search_stock_category) && input$search_stock_category != "") {
      where_conditions <- c(where_conditions, paste0("stock_category = '", input$search_stock_category, "'"))
    }
    
    # Add status filter
    if (!is.null(input$search_status) && input$search_status != "Both") {
      if (input$search_status == "Live") {
        where_conditions <- c(where_conditions, "status != 'Deceased'")
      } else if (input$search_status == "Deceased") {
        where_conditions <- c(where_conditions, "status = 'Deceased'")
      }
    }
    
    # Build the complete query
    if (length(where_conditions) == 0) {
      query <- paste0("SELECT * FROM ", TABLE_NAME, " ORDER BY asu_id")
    } else {
      query <- paste0("SELECT * FROM ", TABLE_NAME, " WHERE ", paste(where_conditions, collapse = " AND "), " ORDER BY asu_id")
    }
    
    # Execute search and update the all_mice_table reactive value
    con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
    search_results <- tryCatch({
      DBI::dbGetQuery(con, query)
    }, error = function(e) {
      data.frame()
    })
    DBI::dbDisconnect(con)
    
    # Update the all_mice_table reactive value with search results
    all_mice_table(search_results)
    
    # Close the search modal
    removeModal()
    
    # Switch to All Mice tab (with emoji)
    updateTabsetPanel(session, "tabs", selected = "🐭 All Mice")
    
    # Show a brief notification about search results
    if (nrow(search_results) == 0) {
      showNotification("No animals found matching your search criteria.", type = "warning", duration = 3)
    } else {
      showNotification(paste("Found", nrow(search_results), "animals matching your search criteria."), type = "message", duration = 3)
    }
    })

  # Server logic for timezone selection
  observeEvent(input$set_timezone_btn, {
    showModal(modalDialog(
      title = 'Select Timezone',
      selectInput('timezone_select', 'Timezone', choices = OlsonNames(), selected = user_timezone()),
      footer = tagList(
        modalButton('Cancel'),
        actionButton('confirm_timezone_btn', 'Set Timezone', class = 'btn-primary')
      )
    ))
  })
  observeEvent(input$confirm_timezone_btn, {
    req(input$timezone_select)
    user_timezone(input$timezone_select)
    removeModal()
    showNotification(paste('Timezone set to', input$timezone_select), type = 'message')
  })

  # Server logic for version info modal
  observeEvent(input$version_info_btn, {
    # Read VERSION.md file
    version_content <- tryCatch({
      version_file_path <- "VERSION.md"
      if (file.exists(version_file_path)) {
        readLines(version_file_path, warn = FALSE)
      } else {
        c("# Version Information", "Version file not found.")
      }
    }, error = function(e) {
      c("# Version Information", "Error reading version file.")
    })
    
    # Convert markdown to HTML
    version_html <- tryCatch({
      markdown::markdownToHTML(text = paste(version_content, collapse = "\n"), 
                              fragment.only = TRUE)
    }, error = function(e) {
      paste(version_content, collapse = "<br>")
    })
    
    showModal(modalDialog(
      title = "Version Information",
      size = "l",
      div(
        style = "line-height: 1.6;",
        HTML(version_html)
      ),
      footer = modalButton("Close")
    ))
  })


}

# Run the application 
shinyApp(ui = ui, server = server)
