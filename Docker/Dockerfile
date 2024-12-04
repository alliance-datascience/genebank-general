FROM ubuntu:focal-20241011
LABEL Name=qualityscoredocker Version=0.0.1


WORKDIR /home

ARG DEBIAN_FRONTEND=noninteractive
ENV TZ=America/New_York
ENV WORK_DIR=/home/input_data
ENV SCRIPT_DIR=/home

COPY scripts/package_installers/install_packages.sh /home/install_packages.sh
COPY input_data /home/input_data
COPY scripts/quality_score/02_genesys_quality_score_v2.R /home/02_genesys_quality_score_v2.R
COPY scripts/api/REST_api.R /home/REST_api.R
COPY scripts/api/run_main.R /home/run_main.R

RUN /home/install_packages.sh

RUN R -e "install.packages('Rcpp')"
RUN R -e "install.packages('terra', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('classInt', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('sf'   , repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('sodium'   , repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('plumber'   , repos='http://cran.rstudio.com/')"
RUN R -e "suppressMessages(if(!require(pacman)){install.packages('pacman');library(pacman)}else{library(pacman)})"
RUN R -e "pacman::p_load(data.table, tidyverse, sf, geodata, dplyr, stringr, stringi, readr, terra, vroom, httr, jsonlite, geosphere, readxl, plumber)"

EXPOSE 8081

ENTRYPOINT ["Rscript", "run_main.R"]

