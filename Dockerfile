FROM  patroonorg/patroonrs AS building

ENV DEBIAN_FRONTEND=noninteractive
ENV PROJECT_DIR /home/rstudio/streamfind
USER root

RUN mkdir -p ${PROJECT_DIR}
COPY . ${PROJECT_DIR}
WORKDIR ${PROJECT_DIR}

FROM building

## Install streamFind and dependencies ##
RUN R < ./R/000_install_streamFind_and_dependencies_for_docker.R --no-save

# Run RStudio in Browser
RUN /init

USER rstudio