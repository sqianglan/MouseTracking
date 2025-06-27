   #!/bin/bash


# Path to Rscript (adjust if needed)
RSCRIPT="/usr/local/bin/Rscript"

# Run the Shiny app
$RSCRIPT -e "shiny::runApp('$(dirname "$0")', launch.browser=TRUE)"
