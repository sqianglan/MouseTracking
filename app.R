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
            actionButton("version_info_btn", "Version: beta 1.10", 
                        style = "background: rgba(255, 255, 255, 0.9); color: #2c3e50; border: none; border-radius: 6px; font-size: 0.9em; padding: 6px 12px; font-weight: 500; cursor: pointer; box-shadow: 0 2px 4px rgba(0,0,0,0.1);")
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
  
  # Initialize reactive values for import data
  import_data <- reactiveValues(
    df = NULL,
    mappings = NULL,
    available_columns = NULL,
    field_suggestions = NULL
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
    showModal(modalDialog(
      title = "Add Animals",
      div(
        actionButton("add_single_entry_btn", "Single Entry", style = "margin-right: 16px; background-color: #90caf9; color: #222; border: none; font-size: 1.1em; padding: 8px 24px;"),
        actionButton("add_import_excel_btn", "Import from Excel", style = "background-color: #a5d6a7; color: #222; border: none; font-size: 1.1em; padding: 8px 24px;"),
        style = "display: flex; justify-content: center; gap: 16px; margin-top: 12px; margin-bottom: 12px;"
      ),
      footer = modalButton("Close")
    ))
  })

  # Show Single Entry Form
  observeEvent(input$add_single_entry_btn, {
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
  })

  # Show Import from Excel UI
  observeEvent(input$add_import_excel_btn, {
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
  })

  # Handle Excel Import
  observeEvent(input$submit_import_excel_btn, {
    req(input$import_excel_file)
    file_path <- input$import_excel_file$datapath
    library(readxl)
    df <- tryCatch(readxl::read_excel(file_path), error = function(e) NULL)
    if (is.null(df)) {
      showModal(modalDialog(title = "Import Error", "Failed to read Excel file.", easyClose = TRUE))
      return()
    }
    
    # Get column mappings with suggestions
    mapping_result <- tryCatch(parse_excel_to_mice_stock(df), error = function(e) NULL)
    if (is.null(mapping_result)) {
      showModal(modalDialog(title = "Import Error", "Failed to analyze Excel file columns.", easyClose = TRUE))
      return()
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
    
    # Add reactive observer for real-time mapping validation
    observeEvent({
      # React to any changes in mapping dropdown selections
      col_names <- names(import_data$df)
      lapply(1:length(col_names), function(i) {
        input[[paste0("mapping_col_", i)]]
      })
    }, {
      if (!is.null(import_data$df)) {
        col_names <- names(import_data$df)
        validation_result <- validate_column_mappings(input, length(col_names))
        
        # Update warning area with current validation status
        output$mapping_warnings <- renderUI({
          if (!validation_result$is_valid) {
            create_duplicate_mapping_warning(validation_result$duplicate_fields)
          } else {
            NULL
          }
        })
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
  })
  
  # Handle "Go Back to Fix Mappings" button
  observeEvent(input$go_back_to_mapping, {
    # Close current modal and reopen the column mapping modal
    removeModal()
    
    # Recreate the column mapping UI with current data
    if (!is.null(import_data$df)) {
      mapping_result <- list(
        mappings = import_data$mappings,
        available_columns = import_data$available_columns,
        field_suggestions = import_data$field_suggestions
      )
      
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
      
      # Re-add reactive observer for real-time mapping validation
      observeEvent({
        # React to any changes in mapping dropdown selections
        col_names <- names(import_data$df)
        lapply(1:length(col_names), function(i) {
          input[[paste0("mapping_col_", i)]]
        })
      }, {
        if (!is.null(import_data$df)) {
          col_names <- names(import_data$df)
          validation_result <- validate_column_mappings(input, length(col_names))
          
          # Update warning area with current validation status
          output$mapping_warnings <- renderUI({
            if (!validation_result$is_valid) {
              create_duplicate_mapping_warning(validation_result$duplicate_fields)
            } else {
              NULL
            }
          })
        }
      }, ignoreNULL = FALSE, ignoreInit = TRUE)
    }
  })
  
  # Handle mapping confirmation
  observeEvent(input$confirm_mappings_btn, {
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
      return()
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
      return()
    }
    
    # Apply mappings and create data frame
    parsed_df <- tryCatch(
      apply_mappings_and_create_df(import_data$df, confirmed_mappings, input$import_stock_category), 
      error = function(e) NULL
    )
    
    if (is.null(parsed_df)) {
      showModal(modalDialog(title = "Import Error", "Failed to parse Excel data with confirmed mappings.", easyClose = TRUE))
      return()
    }
    
    # Check for duplicates and conflicts using function from db_check.R
    duplicate_check <- check_duplicates_and_conflicts(parsed_df)
    
    # Show duplicates in UI if any found
    if (duplicate_check$has_duplicates) {
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
              uiOutput("duplicate_action_ui")
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
      
      # Store data for processing
      import_data$parsed_df <- parsed_df
      import_data$comparison_data <- duplicate_check$comparison_data
      import_data$exact_matches <- duplicate_check$exact_matches
      import_data$import_duplicates <- duplicate_check$import_duplicates
      import_data$db_conflicts <- duplicate_check$db_conflicts
      return()
    }
    
    # No duplicates, import directly
    result <- import_data_to_db(parsed_df)
    if (result) {
      showModal(modalDialog(title = "Import Success", "Animals imported successfully!", easyClose = TRUE))
    } else {
      showModal(modalDialog(title = "Import Error", "Failed to import animals. Please check for duplicate or invalid ASU IDs.", easyClose = TRUE))
    }
  })

  output$duplicate_action_ui <- renderUI({
    req(import_data$comparison_data)
    
    DT::dataTableOutput("duplicate_conflicts_table", width = "100%")
  })
  
  output$duplicate_conflicts_table <- DT::renderDataTable({
    req(import_data$comparison_data)
    
    # Create table data with highlighted differences
    display_data <- data.frame(
      ASU_ID = import_data$comparison_data$ASU_ID,
      stringsAsFactors = FALSE
    )
    
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
    
    # Add comparison columns
    display_data$Animal_ID <- mapply(create_comparison_cell, 
                                    import_data$comparison_data$Import_Animal_ID, 
                                    import_data$comparison_data$DB_Animal_ID)
    
    display_data$Gender <- mapply(create_comparison_cell, 
                                 import_data$comparison_data$Import_Gender, 
                                 import_data$comparison_data$DB_Gender)
    
    display_data$DoB <- mapply(create_comparison_cell, 
                              import_data$comparison_data$Import_DoB, 
                              import_data$comparison_data$DB_DoB)
    
    display_data$Breeding_Line <- mapply(create_comparison_cell, 
                                        import_data$comparison_data$Import_Breeding_Line, 
                                        import_data$comparison_data$DB_Breeding_Line)
    
    display_data$Genotype <- mapply(create_comparison_cell, 
                                   import_data$comparison_data$Import_Genotype, 
                                   import_data$comparison_data$DB_Genotype)
    
    # Add action column
    display_data$Action <- sapply(1:nrow(import_data$comparison_data), function(i) {
      as.character(selectInput(
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
    })
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = "400px",
        dom = 't',
        ordering = FALSE,
        columnDefs = list(
          list(width = '80px', targets = 0),   # ASU_ID
          list(width = '120px', targets = 1),  # Animal_ID
          list(width = '80px', targets = 2),   # Gender
          list(width = '100px', targets = 3),  # DoB
          list(width = '140px', targets = 4),  # Breeding_Line
          list(width = '140px', targets = 5),  # Genotype
          list(width = '180px', targets = 6)   # Action
        )
      ),
      escape = FALSE,
      rownames = FALSE,
      class = 'cell-border stripe hover compact',
      colnames = c('ASU ID', 'Animal ID', 'Gender', 'Date of Birth', 'Breeding Line', 'Genotype', 'Action')
    )
  })
  
  # Reactive values for custom ASU ID modal
  custom_asu_data <- reactiveValues(
    records = NULL,
    custom_asu_ids = list()
  )
  
  # Render custom ASU ID modal UI
  output$custom_asu_id_ui <- renderUI({
    req(custom_asu_data$records)
    
    lapply(1:length(custom_asu_data$records), function(i) {
      record <- custom_asu_data$records[[i]]
      
      div(
        style = "border: 1px solid #dee2e6; margin: 15px 0; padding: 20px; border-radius: 8px; background-color: #f8f9fa;",
        
        # Header
        fluidRow(
          column(6,
            h5(paste("Original ASU ID:", record$asu_id), style = "color: #495057; font-weight: 600;")
          ),
          column(6,
            h5(paste("Action:", record$action), style = "color: #007bff; font-weight: 600;")
          )
        ),
        
        # Record details
        div(
          style = "margin: 15px 0; padding: 15px; background-color: white; border-radius: 5px;",
          h6("Record Details:", style = "color: #6c757d; margin-bottom: 10px;"),
          fluidRow(
            column(4, 
              strong("Animal ID:"), br(), 
              span(record$import_data$Import_Animal_ID, style = "color: #495057;")
            ),
            column(4, 
              strong("Gender:"), br(), 
              span(record$import_data$Import_Gender, style = "color: #495057;")
            ),
            column(4, 
              strong("Breeding Line:"), br(), 
              span(record$import_data$Import_Breeding_Line, style = "color: #495057;")
            )
          ),
          br(),
          fluidRow(
            column(6, 
              strong("Genotype:"), br(), 
              span(record$import_data$Import_Genotype, style = "color: #495057;")
            ),
            column(6, 
              strong("Date of Birth:"), br(), 
              span(record$import_data$Import_DoB, style = "color: #495057;")
            )
          )
        ),
        
        # New ASU ID input
        fluidRow(
          column(8,
            textInput(
              paste0("custom_asu_", i),
              "New ASU ID:",
              value = record$default_new_asu_id,
              width = "100%"
            )
          ),
          column(4,
            br(),
            actionButton(
              paste0("check_asu_", i),
              "Check Availability",
              class = "btn-outline-primary",
              style = "margin-top: 5px; width: 100%;"
            )
          )
        ),
        
        # Status message
        div(
          id = paste0("asu_status_", i),
          style = "margin-top: 10px; min-height: 20px;"
        )
      )
    })
  })
  
  # Handle ASU ID availability check
  observe({
    req(custom_asu_data$records)
    
    lapply(1:length(custom_asu_data$records), function(i) {
      observeEvent(input[[paste0("check_asu_", i)]], {
        new_asu_id <- input[[paste0("custom_asu_", i)]]
        
        if (is.null(new_asu_id) || trimws(new_asu_id) == "") {
          output[[paste0("asu_status_", i)]] <- renderUI({
            div(
              class = "alert alert-warning",
              style = "padding: 8px; margin: 0;",
              icon("exclamation-triangle"), " Please enter an ASU ID"
            )
          })
          return()
        }
        
        # Check if ASU ID already exists in database
        con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
        existing <- DBI::dbGetQuery(con, "SELECT asu_id FROM mice_stock WHERE asu_id = ?", params = list(trimws(new_asu_id)))
        DBI::dbDisconnect(con)
        
        # Check if ASU ID is used by other records in this batch
        other_records_using <- sapply(1:length(custom_asu_data$records), function(j) {
          if (i != j) {
            other_asu <- input[[paste0("custom_asu_", j)]]
            return(!is.null(other_asu) && trimws(other_asu) == trimws(new_asu_id))
          }
          return(FALSE)
        })
        
        if (nrow(existing) > 0) {
          output[[paste0("asu_status_", i)]] <- renderUI({
            div(
              class = "alert alert-danger",
              style = "padding: 8px; margin: 0;",
              icon("times-circle"), " ASU ID already exists in database"
            )
          })
        } else if (any(other_records_using)) {
          output[[paste0("asu_status_", i)]] <- renderUI({
            div(
              class = "alert alert-warning",
              style = "padding: 8px; margin: 0;",
              icon("exclamation-triangle"), " ASU ID is used by another record in this batch"
            )
          })
        } else {
          output[[paste0("asu_status_", i)]] <- renderUI({
            div(
              class = "alert alert-success",
              style = "padding: 8px; margin: 0;",
              icon("check-circle"), " ASU ID is available"
            )
          })
        }
      })
    })
  })
  
  # Handle cancel custom ASU ID
  observeEvent(input$cancel_custom_asu, {
    removeModal()
    # Show the original duplicates modal again
    showModal(modalDialog(
      title = "Duplicate Detection Results",
      size = "xl",
      div(
        # Show exact matches if any
        if (nrow(import_data$exact_matches) > 0) {
          div(
            h5("Identical Records (will be skipped):"),
            renderTable(import_data$exact_matches, striped = TRUE, bordered = TRUE),
            style = "margin-bottom: 20px;"
          )
        },
        # Show differences if any
        if (nrow(import_data$comparison_data) > 0) {
          div(
            h5("Records with Differences:"),
            uiOutput("duplicate_action_ui")
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
          if (nrow(import_data$comparison_data) > 0) {
            paste("Please select actions for", nrow(import_data$comparison_data), "conflict(s)")
          } else {
            "All records are identical and will be skipped"
          }
        ),
        div(
          style = "display: flex; gap: 10px;",
          modalButton("Cancel"),
          if (nrow(import_data$comparison_data) > 0) {
            actionButton("process_duplicates", "Process Selected Actions", class = "btn-primary", style = "font-weight: 600;")
          } else NULL
        )
      )
    ))
  })
  
  # Handle confirm custom ASU ID
  observeEvent(input$confirm_custom_asu, {
    # Validate all ASU IDs
    all_valid <- TRUE
    error_messages <- c()
    
    # Collect all custom ASU IDs
    custom_asu_ids <- list()
    for (i in 1:length(custom_asu_data$records)) {
      new_asu_id <- trimws(input[[paste0("custom_asu_", i)]])
      custom_asu_ids[[i]] <- new_asu_id
      
      if (new_asu_id == "") {
        all_valid <- FALSE
        error_messages <- c(error_messages, paste("Record", i, ": ASU ID cannot be empty"))
      }
    }
    
    # Check for duplicates within the batch
    if (length(unique(custom_asu_ids)) != length(custom_asu_ids)) {
      all_valid <- FALSE
      error_messages <- c(error_messages, "Duplicate ASU IDs found within the batch")
    }
    
    # Check database for existing ASU IDs
    if (all_valid) {
      con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
      for (i in 1:length(custom_asu_ids)) {
        existing <- DBI::dbGetQuery(con, "SELECT asu_id FROM mice_stock WHERE asu_id = ?", params = list(custom_asu_ids[[i]]))
        if (nrow(existing) > 0) {
          all_valid <- FALSE
          error_messages <- c(error_messages, paste("ASU ID", custom_asu_ids[[i]], "already exists in database"))
        }
      }
      DBI::dbDisconnect(con)
    }
    
    if (!all_valid) {
      showModal(modalDialog(
        title = "Validation Error",
        div(
          class = "alert alert-danger",
          h5("Please fix the following issues:"),
          tags$ul(
            lapply(error_messages, function(msg) tags$li(msg))
          )
        ),
        footer = modalButton("OK")
      ))
      return()
    }
    
    # All valid, proceed with processing
    removeModal()
    
    # Create modified user actions with custom ASU IDs
    user_actions <- list()
    custom_asu_map <- list()
    
    # Build user actions from original selections
    for (i in 1:nrow(import_data$comparison_data)) {
      action_input <- input[[paste0("action_", i)]]
      user_actions[[paste0("action_", i)]] <- if (is.null(action_input)) "Skip" else action_input
    }
    
    # Map custom ASU IDs to their records
    for (i in 1:length(custom_asu_data$records)) {
      record <- custom_asu_data$records[[i]]
      custom_asu_map[[record$asu_id]] <- custom_asu_ids[[i]]
    }
    
    # Process duplicates with custom ASU IDs
    result <- process_duplicates_with_custom(
      import_data$parsed_df, 
      import_data$comparison_data, 
      import_data$import_duplicates, 
      import_data$db_conflicts, 
      user_actions,
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
      } else {
        showModal(modalDialog(title = "Import Error", "Failed to import animals.", easyClose = TRUE))
      }
    } else {
      showModal(modalDialog(title = "Import Cancelled", "No records to import after processing duplicates.", easyClose = TRUE))
    }
  })

  output$duplicate_comparison_table <- DT::renderDataTable({
    req(import_data$comparison_data)
    
    # Create action dropdowns for each row
    actions <- lapply(1:nrow(import_data$comparison_data), function(i) {
      asu_id <- import_data$comparison_data$ASU_ID[i]
      selectInput(
        paste0("action_", i),
        label = NULL,
        choices = c("Skip", "Modify", "Overwrite", "Keep Both"),
        selected = "Skip",
        width = "100px"
      )
    })
    
    # Add action column to the data
    display_data <- import_data$comparison_data
    display_data$Action <- sapply(actions, function(x) as.character(x))
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 100,
        scrollX = TRUE,
        dom = 't'
      ),
      selection = 'none',
      escape = FALSE
    )
  })

  # Handle Import with Duplicates Skipped
  observeEvent(input$import_skip_duplicates, {
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
    } else {
      showModal(modalDialog(title = "Import Error", "Failed to import animals.", easyClose = TRUE))
    }
  })

  # Handle Process Duplicates
  observeEvent(input$process_duplicates, {
    req(import_data)
    
    # Get user actions from the selectInputs
    user_actions <- list()
    modify_records <- list()
    keep_both_records <- list()
    
    # Debug: Show all available inputs that match our pattern
    input_names <- names(reactiveValuesToList(input))
    action_inputs <- input_names[grepl("^action_", input_names)]
    cat("DEBUG: Available action inputs:", paste(action_inputs, collapse = ", "), "\n")
    
    for (i in 1:nrow(import_data$comparison_data)) {
      action_input <- input[[paste0("action_", i)]]
      action <- if (is.null(action_input)) "Skip" else action_input
      user_actions[[paste0("action_", i)]] <- action
      
      # Debug: log what actions we're getting
      cat("DEBUG: Row", i, "input[[paste0('action_', i)]]:", action_input, "final action:", action, "\n")
      
      
      # Collect records that need custom ASU ID input
      if (action == "Modify" || action == "Keep Both") {
        row_data <- import_data$comparison_data[i, ]
        record_info <- list(
          index = i,
          asu_id = row_data$ASU_ID,
          action = action,
          import_data = row_data,
          default_new_asu_id = paste0(row_data$ASU_ID, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
        )
        
        if (action == "Modify") {
          modify_records[[length(modify_records) + 1]] <- record_info
        } else {
          keep_both_records[[length(keep_both_records) + 1]] <- record_info
        }
      }
    }
    
    # If there are records that need custom ASU ID, show modal
    if (length(modify_records) > 0 || length(keep_both_records) > 0) {
      custom_asu_data$records <- c(modify_records, keep_both_records)
      custom_asu_data$custom_asu_ids <- list()
      
      # Initialize default ASU IDs
      for (i in 1:length(custom_asu_data$records)) {
        custom_asu_data$custom_asu_ids[[i]] <- custom_asu_data$records[[i]]$default_new_asu_id
      }
      
      removeModal()
      showModal(modalDialog(
        title = "Custom ASU ID Assignment",
        size = "l",
        div(
          h4("Records requiring new ASU IDs:", style = "color: #495057; margin-bottom: 20px;"),
          uiOutput("custom_asu_id_ui")
        ),
        footer = tagList(
          actionButton("cancel_custom_asu", "Cancel", class = "btn-secondary"),
          actionButton("confirm_custom_asu", "Confirm & Process", class = "btn-primary")
        )
      ))
      return()
    }
    
    # No custom ASU IDs needed, process normally
    removeModal()
    result <- process_duplicates(
      import_data$parsed_df, 
      import_data$comparison_data, 
      import_data$import_duplicates, 
      import_data$db_conflicts, 
      user_actions
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
      } else {
        showModal(modalDialog(title = "Import Error", "Failed to import animals.", easyClose = TRUE))
      }
    } else {
      showModal(modalDialog(title = "Import Cancelled", "No records to import after processing duplicates.", easyClose = TRUE))
    }
  })

  # Handle Single Entry Submission
  observeEvent(input$submit_single_entry_btn, {
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
      return()
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
      return()
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
      return()
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
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, DATETIME('now'), DATETIME('now'))",
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
      
      # Refresh the all_mice_table
      con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
      all_data <- DBI::dbGetQuery(con, paste0("SELECT * FROM ", TABLE_NAME, " ORDER BY asu_id"))
      DBI::dbDisconnect(con)
      all_mice_table(all_data)
    } else {
      showModal(modalDialog(title = "Error", "Failed to add animal. Please check your input.", easyClose = TRUE))
    }
  })
  
  # Handle proceeding with warnings
  observeEvent(input$proceed_with_warnings, {
    removeModal()
    # Trigger the submission again
    input$submit_single_entry_btn
  })

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
