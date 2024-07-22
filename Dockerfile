# Use a base R image with the required R version
FROM rocker/r-ver:4.3.0

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    g++ \
    make \
    libz-dev

# Install required R packages
RUN R -e "install.packages(c('remotes', 'patRoon', 'learnr', 'shiny'))"
RUN R -e "remotes::install_github('odea-project/StreamFind', dependencies=TRUE)"
RUN R -e "remotes::install_github('odea-project/StreamFindData')"

# Copy the project files into the container
COPY . /app

# Set the working directory
WORKDIR /app

# Expose the Shiny app port
EXPOSE 3838

# Run the Shiny app
CMD ["R", "-e", "shiny::runApp('/app/dev/dev_app.R', port=3838, host='0.0.0.0')"]
