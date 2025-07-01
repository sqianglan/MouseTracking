# Mouse Management System

A comprehensive web-based application built with R Shiny for tracking and managing mouse colony plugging records in animal research facilities.

## Quick Start

### Option 1: Automatic Setup (Recommended)
```r
# Run the startup script - it will check and install packages automatically
source("start_app.R")
```

### Option 2: Manual Setup
```r
# First, install required packages
source("install_packages.R")

# Then start the application
shiny::runApp()
```

## Required Packages

The system requires the following R packages:
- `shiny` - Web application framework
- `shinyFiles` - File input handling
- `tidyverse` - Data manipulation and visualization
- `DT` - Interactive data tables
- `calendR` - Calendar visualization
- `ggsci` - Scientific color palettes
- `DBI` - Database interface
- `RSQLite` - SQLite database driver
- `jsonlite` - JSON data handling
- `stringr` - String manipulation
- `lubridate` - Date/time handling
- `ggplot2` - Plotting
- `dplyr` - Data manipulation

## Features

- **Home Dashboard**: Welcome interface with visual workflow diagram
- **All Mice Management**: Comprehensive search, filtering, and bulk operations
- **Plugging Management**: Complete breeding pair and plugging event tracking
- **Event Calendar**: Interactive calendar visualization with Apple-style design
- **Advanced Search & Import**: Single entry and Excel import with duplicate detection
- **Security & Data Integrity**: Global lock system and comprehensive audit trail

## System Requirements

- R 4.0 or higher
- Modern web browser with JavaScript enabled
- SQLite database (automatically created)

## Database

The system uses SQLite for data storage. The database file will be automatically created in the application directory when first run.

## Copyright

**Copyright (c) 2025 Qiang Lan. All rights reserved.**

For inquiries regarding licensing or usage rights, please contact qiang.lan@bristol.ac.uk.

## Contact

- **Developer**: Qiang Lan
- **Institution**: University of Bristol
- **Email**: qiang.lan@bristol.ac.uk

---

*This system addresses the critical gap in current Animal Facility webtools by providing dedicated mouse breeding and plugging record management.* 