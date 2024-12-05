FROM rocker/r-ver:4.4
LABEL Name=qualityscoredocker Version=0.0.1


WORKDIR /home

ARG DEBIAN_FRONTEND=noninteractive
ENV WORK_DIR=/home/input_data
ENV SCRIPT_DIR=/home

# Copy this first since it doesn't change
COPY input_data /home/input_data

RUN echo 'Dir::Cache::pkgcache ""; Dir::Cache::srcpkgcache "";' > /etc/apt/apt.conf.d/docker-clean && \
 apt-get clean && \
 apt-get update -qq && apt-get install -yq --no-install-recommends software-properties-common dirmngr && \
 add-apt-repository ppa:ubuntugis/ubuntugis-unstable -y && \
 apt-get update && \
 apt-get install -y --no-install-recommends \
 libgdal-dev \
 libgeos-dev \
 libproj-dev \
 libudunits2-dev \
 libxml2-dev \
 libcairo2-dev \
 libgit2-dev \
 default-libmysqlclient-dev \
 libpq-dev \
 libsasl2-dev \
 libsqlite3-dev \
 libssh2-1-dev \
 libxtst6 \
 libcurl4-openssl-dev \
 libharfbuzz-dev \
 libfribidi-dev \
 libfreetype6-dev \
 libpng-dev \
 libtiff5-dev \
 libjpeg-dev \
 unixodbc-dev \
 libsodium-dev\
 libssl-dev && \
 apt-get autoclean && \
 apt-get clean


RUN R -e "install.packages('Rcpp')"
RUN R -e "install.packages('terra', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('classInt', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('sf', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('sodium', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('plumber', repos='http://cran.rstudio.com/')"
RUN R -e "suppressMessages(if(!require(pacman)){install.packages('pacman');library(pacman)}else{library(pacman)})"
RUN R -e "pacman::p_load(data.table, tidyverse, sf, geodata, dplyr, stringr, stringi, readr, terra, vroom, httr, jsonlite, geosphere, readxl, plumber)"


COPY scripts/quality_score/02_genesys_quality_score_v2.R /home/02_genesys_quality_score_v2.R
COPY scripts/api/REST_api.R /home/REST_api.R
COPY scripts/api/run_main.R /home/run_main.R

EXPOSE 8081

ENTRYPOINT ["Rscript", "run_main.R"]

