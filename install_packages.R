# install_packages.R
# Script to check and install all necessary packages for Mouse Management System
# Run this script before starting the application

cat("=== Mouse Management System - Package Installation ===\n")
cat("Checking and installing required packages...\n\n")

# List of required packages
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

# Function to install packages if not already installed
install_if_missing <- function(package_name) {
  if (!require(package_name, character.only = TRUE, quietly = TRUE)) {
    cat("Installing", package_name, "...\n")
    install.packages(package_name, dependencies = TRUE, quiet = TRUE)
    
    # Check if installation was successful
    if (require(package_name, character.only = TRUE, quietly = TRUE)) {
      cat("✓", package_name, "installed successfully\n")
    } else {
      cat("✗ Failed to install", package_name, "\n")
      return(FALSE)
    }
  } else {
    cat("✓", package_name, "already installed\n")
  }
  return(TRUE)
}

# Install all required packages
cat("Checking required packages:\n")
cat("==========================\n")

all_installed <- TRUE
for (package in required_packages) {
  if (!install_if_missing(package)) {
    all_installed <- FALSE
  }
}

cat("\n==========================\n")
if (all_installed) {
  cat("✓ All packages installed successfully!\n")
  cat("You can now run the Mouse Management System.\n")
} else {
  cat("✗ Some packages failed to install.\n")
  cat("Please check the error messages above and try again.\n")
}

cat("\nTo start the application, run:\n")
cat("shiny::runApp()\n") 