# start_app.R
# Startup script for Mouse Management System
# This script checks for required packages and starts the application

cat("=== Mouse Management System ===\n")
cat("Starting application...\n\n")

# Check if required packages are installed
required_packages <- c(
  "shiny",
  "shinyFiles", 
  "tidyverse",
  "DT",
  "calendR",
  "ggsci",
  "DBI",
  "RSQLite",
  "jsonlite",
  "stringr",
  "lubridate",
  "ggplot2",
  "dplyr"
)

# Function to check if package is installed
check_package <- function(package_name) {
  require(package_name, character.only = TRUE, quietly = TRUE)
}

# Check all required packages
missing_packages <- c()
for (package in required_packages) {
  if (!check_package(package)) {
    missing_packages <- c(missing_packages, package)
  }
}

# If packages are missing, run installation script
if (length(missing_packages) > 0) {
  cat("Missing packages detected:\n")
  cat(paste("  -", missing_packages), sep = "\n")
  cat("\nRunning package installation...\n")
  
  # Source the installation script
  source("install_packages.R")
  
  # Check again after installation
  still_missing <- c()
  for (package in missing_packages) {
    if (!check_package(package)) {
      still_missing <- c(still_missing, package)
    }
  }
  
  if (length(still_missing) > 0) {
    cat("\n✗ Some packages could not be installed:\n")
    cat(paste("  -", still_missing), sep = "\n")
    cat("\nPlease run 'install_packages.R' manually and try again.\n")
    stop("Missing required packages")
  }
}

cat("✓ All required packages are available\n")
cat("Starting Mouse Management System...\n\n")

# Start the Shiny application
shiny::runApp() 