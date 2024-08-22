# Use an official R base image
FROM r-base:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libnetcdf-dev \
    default-jdk \
    libmagick++-dev \
    libglu1-mesa-dev \
    libgl1-mesa-dev \
    libudunits2-dev \
    libgdal-dev

# Install Shiny
RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"

# Install BiocManager and remotes
RUN R -e "install.packages(c('BiocManager', 'remotes'), repos='http://cran.rstudio.com/')"

# Install StreamFind
RUN R -e "BiocManager::install('odea-project/StreamFind', dependencies = TRUE)"

# Install StreamFindData
RUN R -e "BiocManager::install('odea-project/StreamFindData')"

# Install patRoon dependencies
RUN R -e "BiocManager::install(c('ncdf4', 'rJava', 'magick', 'mzR', 'rcdklibs', 'MSnbase', 'rcdk', 'xcms', 'CAMERA'), dependencies = TRUE)"

# Install patRoon
RUN R -e "BiocManager::install('rickhelmus/patRoon', dependencies = TRUE)"

# Set the working directory in the container
WORKDIR /app

# Copy your app file into the container
COPY dev/dev_app.R /app/app.R

# Expose port 3838 (default for Shiny)
EXPOSE 3838

# Command to run the Shiny app
CMD ["R", "-e", "shiny::runApp('/app/app.R', host = '0.0.0.0', port = 3838)"]