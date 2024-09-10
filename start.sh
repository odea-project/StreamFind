#!/bin/bash

echo "Please select an option:"
echo "1) Run Shiny App"
echo "2) Run RStudio Server"
echo "3) Run Both Shiny App and RStudio Server"

read -p "Enter the number: " option

if [ "$option" == "1" ]; then
    echo "Starting Shiny App..."
    R -e "shiny::runApp('/app/dev/dev_app.R', port=3838, host='0.0.0.0')"
elif [ "$option" == "2" ]; then
echo "Starting RStudio Server..."
    rstudio-server start
    sleep 5
    tail -f /dev/null
elif [ "$option" == "3" ]; then
    echo "Starting both Shiny App and RStudio Server..."
    rstudio-server start
    R -e "shiny::runApp('/app/dev/dev_app.R', port=3838, host='0.0.0.0')" &
    tail -f /dev/null
else
    echo "Invalid option. Please select 1 or 2."
fi
