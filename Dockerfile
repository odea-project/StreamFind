# Use RStudio as base image
FROM rocker/rstudio:4

# Set the working directory
WORKDIR /app

# Install system libraries required for R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libpng-dev \
    libmagick++-dev \
    zlib1g-dev \
    default-jdk \
    libnetcdf-dev

# Install BiocManager and remotes
RUN R -e "install.packages(c('BiocManager', 'remotes'), repos='http://cran.rstudio.com/')"

# Install patRoon dependencies
RUN R -e "BiocManager::install(c('ncdf4', 'rJava', 'magick', 'mzR', 'rcdklibs', 'MSnbase', 'rcdk', 'xcms', 'CAMERA'), dependencies = TRUE)"

# Install patRoon
RUN R -e "BiocManager::install('rickhelmus/patRoon', dependencies = TRUE)"

# Install StreamFind
RUN R -e "BiocManager::install('odea-project/StreamFind', dependencies = TRUE)"

# Install StreamFindData
RUN R -e "BiocManager::install('odea-project/StreamFindData')"

# Copy the start.sh script to the container
COPY start.sh /start.sh

# Give execute permission to the script
RUN chmod +x /start.sh

ENTRYPOINT ["/start.sh"]
