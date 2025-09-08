#!/bin/bash

# Determine if we're running from an .app bundle (Platypus packaging)
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# Check if we're inside a .app bundle
if [[ "$SCRIPT_DIR" == *.app/Contents/* ]]; then
    # We're in a .app bundle - set database location to same folder as .app file
    APP_BUNDLE_DIR="$(dirname "$(dirname "$SCRIPT_DIR")")"
    APP_PARENT_DIR="$(dirname "$APP_BUNDLE_DIR")"
    export MOUSE_DB_DIR="$APP_PARENT_DIR"
    echo "Running in packaged mode - database will be in: $APP_PARENT_DIR"
else
    # We're running in development mode - use the script's directory
    export MOUSE_DB_DIR="$SCRIPT_DIR"
    echo "Running in development mode - database will be in: $SCRIPT_DIR"
fi

export MOUSE_DB_NAME="mice_colony.db"

# Alternative: Use command line argument if provided
# Usage: ./run_shiny.sh /path/to/database
if [ $# -eq 1 ]; then
    export MOUSE_DB_DIR="$1"
    echo "Using custom database location: $1"
fi

# Alternative: Read from config file if it exists
# Check for the config file in the expected location based on mode
if [[ "$SCRIPT_DIR" == *.app/Contents/* ]]; then
    # Packaged mode - look for config in .mousemanagement_DontRemove relative to app location
    CONFIG_FILE="$APP_PARENT_DIR/.mousemanagement_DontRemove/config/current_db.conf"
else
    # Development mode - look for config in .mousemanagement_DontRemove relative to script
    CONFIG_FILE="$SCRIPT_DIR/.mousemanagement_DontRemove/config/current_db.conf"
fi

if [ -f "$CONFIG_FILE" ]; then
    export MOUSE_DB_DIR="$(dirname "$(cat "$CONFIG_FILE" | tr -d '\n')")"
    echo "Using database location from config file: $MOUSE_DB_DIR"
fi

# Path to Rscript (adjust if needed)
RSCRIPT="/usr/local/bin/Rscript"

# Install required packages if install_packages.R exists
if [ -f "install_packages.R" ]; then
    echo "Checking and installing required R packages..."
    $RSCRIPT install_packages.R
fi

# Run the Shiny app
$RSCRIPT -e "shiny::runApp('$(dirname "$0")', launch.browser=TRUE)"
