#!/bin/bash

# Set database directory and name for packaging/production
export MOUSE_DB_DIR="$HOME/Documents/MouseManagement/database"
export MOUSE_DB_NAME="mice_colony.db"

# Alternative: Use command line argument if provided
# Usage: ./run_shiny.sh /path/to/database
if [ $# -eq 1 ]; then
    export MOUSE_DB_DIR="$1"
fi

# Alternative: Read from config file if it exists
if [ -f "db_config.txt" ]; then
    export MOUSE_DB_DIR="$(cat db_config.txt | tr -d '\n')"
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
