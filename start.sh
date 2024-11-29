#!/bin/bash

echo "Please select an option:"
echo "1) Run Shiny App"
echo "2) Run RStudio Server"
echo "3) Run Both Shiny App and RStudio Server"

read -p "Enter the number: " option

if [ "$option" == "1" ]; then
    echo "Starting Shiny App..."
    R -e "StreamFind::run_app(options = list(port=3838, host='0.0.0.0'))"
elif [ "$option" == "2" ]; then
    echo "Starting RStudio Server..."
    rstudio-server start
    sleep 5
    tail -f /dev/null
elif [ "$option" == "3" ]; then
    echo "Starting both Shiny App and RStudio Server..."
    rstudio-server start &
    R -e "StreamFind::run_app(options = list(port=3838, host='0.0.0.0'))" &
    wait # Ensures that both processes are properly handled
else
    echo "Invalid option. Please select 1, 2, or 3."
fi