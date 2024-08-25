#!/bin/bash

echo "Please select an option:"
echo "1) Run Shiny App"
echo "2) (Future) Run RStudio Server"

read -p "Enter the number: " option

if [ "$option" == "1" ]; then
    echo "Starting Shiny App..."
    R -e "shiny::runApp('/app/dev/dev_app.R', port=3838, host='0.0.0.0')"
elif [ "$option" == "2" ]; then
    echo "RStudio Server integration will be implemented soon."
else
    echo "Invalid option. Please select 1 or 2."
fi
