# Use RStudio as base image
FROM rocker/rstudio:4

# Set the working directory
WORKDIR /app

# Install system libraries required for R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    zlib1g-dev \
    openjdk-11-jdk \
    libnetcdf-dev \
    libpng-dev \
    libmagick++-dev \
    r-cran-rjava \
    r-cran-xml \
    libcairo2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev

# Set CRAN and Bioconductor repositories
RUN R -e "options(repos = c(CRAN = 'https://p3m.dev/cran/latest'))"

# Install remotes package and patRoonInst from GitHub
RUN R -e "install.packages('remotes', repos='https://p3m.dev/cran/latest')"
RUN R -e "remotes::install_github('rickhelmus/patRoonInst')"

# Install patRoon directly
RUN R -e "patRoonInst::install()"

# Install BiocManager if not already installed, then install StreamFind and StreamFindData
RUN R -e "if (!require('BiocManager', quietly = TRUE)) install.packages('BiocManager', repos='https://p3m.dev/cran/latest')"
RUN R -e "BiocManager::install('odea-project/StreamFind', dependencies = TRUE)"
RUN R -e "BiocManager::install('odea-project/StreamFindData', dependencies = TRUE)"

# Copy the start.sh script to the container
COPY start.sh /start.sh

# Give execute permission to the script
RUN chmod +x /start.sh

ENTRYPOINT ["/start.sh"]
