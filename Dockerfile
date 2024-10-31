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
    libfreetype6-dev \
    libglpk40

# Set CRAN and Bioconductor repositories
#RUN R -e "options(repos = c(CRAN = 'https://p3m.dev/cran/latest'))"

# Install remotes package and patRoonInst from GitHub
RUN R -e "install.packages('remotes')"
RUN R -e "install.packages('BiocManager')"
RUN R -e "BiocManager::install('ropls')"
RUN R -e "BiocManager::install('CAMERA')"
RUN R -e "BiocManager::install('sneumann/Rdisop')"
RUN R -e "remotes::install_github('cran/InterpretMSSpectrum@1.3.3')"
RUN R -e "remotes::install_github('cbroeckl/RAMClustR@e005614')"
RUN R -e "remotes::install_github('blosloos/enviPick')"
RUN R -e "remotes::install_github('blosloos/nontargetData')"
RUN R -e "remotes::install_github('blosloos/nontarget')"
RUN R -e "remotes::install_github('rickhelmus/KPIC2')"
RUN R -e "remotes::install_github('cysouw/qlcMatrix')"
RUN R -e "remotes::install_github('rickhelmus/cliqueMS')"
RUN R -e "BiocManager::install('BiocStyle')"
RUN R -e "BiocManager::install('Rgraphviz')"
RUN R -e "remotes::install_github('souravc83/fastAdaboost')"
RUN R -e "remotes::install_github('KelseyChetnik/MetaClean')"
RUN R -e "remotes::install_github('KelseyChetnik/MetaCleanData')"
RUN R -e "remotes::install_github('berlinguyinca/spectra-hash', subdir='splashR')"
RUN R -e "remotes::install_github('kruvelab/MS2Tox@main')"
RUN R -e "remotes::install_github('drewszabo/MS2Quant@main')"
RUN R -e "remotes::install_github('rickhelmus/patRoonData')"
RUN R -e "remotes::install_github('rickhelmus/patRoonExt')"
#RUN R -e "remotes::install_github('rickhelmus/patRoon@master')"
RUN R -e "remotes::install_github('rickhelmus/patRoonInst')"
RUN R -e "patRoonInst::install()"
RUN R -e "BiocManager::install('odea-project/StreamFindData', dependencies = TRUE)"
RUN R -e "BiocManager::install('odea-project/StreamFind', dependencies = TRUE)"

# Copy the start.sh script to the container
#COPY start.sh /start.sh

# Give execute permission to the script
#RUN chmod +x /start.sh

#ENTRYPOINT ["/start.sh"]

ENTRYPOINT ["bash", "-c", " \
echo 'Please select an option:'; \
echo '1) Run Shiny App'; \
echo '2) Run RStudio Server'; \
echo '3) Run Both Shiny App and RStudio Server'; \
read -p 'Enter the number: ' option; \
if [ \"$option\" == \"1\" ]; then \
    echo 'Starting Shiny App...'; \
    R -e \"StreamFind::run_app(options = list(port=3838, host='0.0.0.0'))\"; \
elif [ \"$option\" == \"2\" ]; then \
    echo 'Starting RStudio Server...'; \
    rstudio-server start; \
    sleep 5; \
    tail -f /dev/null; \
elif [ \"$option\" == \"3\" ]; then \
    echo 'Starting both Shiny App and RStudio Server...'; \
    rstudio-server start & \
    R -e \"StreamFind::run_app(options = list(port=3838, host='0.0.0.0'))\" & \
    wait; \
else \
    echo 'Invalid option. Please select 1, 2, or 3.'; \
fi"]

