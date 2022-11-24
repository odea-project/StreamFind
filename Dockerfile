FROM  rocker/rstudio:4.2.2

ENV DEBIAN_FRONTEND=noninteractive
ENV PROJECT_DIR /home/streamFind

RUN mkdir -p ${PROJECT_DIR}
COPY . ${PROJECT_DIR}
WORKDIR ${PROJECT_DIR}

## Install streamFind and dependencies ##
RUN R < ./dev/000_install_streamFind_and_dependencies.R --no-save