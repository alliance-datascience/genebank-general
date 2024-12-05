# genebank-general
This repository contains all necesary scripts to calculate the quality score for geographical coordinates of accession passport data from [Genesys-pgr](https://www.genesys-pgr.org).

It also contains the scripts used to create the [Dashboard](https://andres159ciat.shinyapps.io/coord_quality_score/).

# Folder Estructure

## Scripts/r

This folder contains the necessary scripts for calculating the quality score using the Software `R (>= 4.2.0)`. main scripts are `00_download_genesys_passport_data.R`for retrieving data using the genesys-pgr api. `01_copy_from_s3` for downloading input data required for the score calculation from AWS s3 bucket and `02_genesys_quality_score_v2.R` which compute the score.

## r-shiny/coord_quality_score

This folder holds the scripts in `R (>= 4.2.0)` for the interactive dashboard built using the `Shiny (v 1.8.0)` library, it contains two main files which are `ui.R` for the user interface/frontend and `server.R` for the backend. Within the folder `www` are necessary functions to support the dashboard and the database for the quality score results in a SQLite format.

## Docker

Dockerfile for building docker container with software requirements for calculating the quality score.

# Description

Proposed workflow are ilustrated in the next diagram:

![pipeline-img](https://raw.githubusercontent.com/alliance-datascience/genebank-general/blob/dev/images/quality_score_pipeline.png)

## Genesys passport data

Script `00_download_genesys_passport_data.R` download data from [Genesys-pgr](https://www.genesys-pgr.org) using their own API. Passport data is downloaded for 11 CGIAR institution which are:

* BEL084
* CIV033
* COL003
* ETH013
* IND002
* KEN023
* LBN002
* MEX002
* NGA039
* PER001
* PHL001

data comes with the same format as specified by Genesys.

## Verification fields

Nine aspects were selected to assess the quality of the passport data for an accession, the more of this aspects are acomplished the higer quality will be, verification fields are described below:

* **ORIGCTY**: wheter or not an accession has reported the country of origin.
* **Missing Coordinates**: wheter an accession has coordinates.
* **Zero Coordinates**: wheter an accession has values of 0 in either latitude or longitude.
* **Accession in Sea or Coast line**: wheter the coordinates falls in oceans.
* **Georeferenced to a centroid**: wheter the coordinate was georeferenced to a Biodiversity institution, city, county or country centroid.
* **Accessions per coordinate**: wheter an accession belong to a group of more than 25 different acessions with the same coordinate.
* **Number of decimal places**: wheter the coordinates has more than two decimal places.
* **Missmatch ORIGCTY**: wheter an accession country of origin does not match the country where the coordinates falls.
* **Bordering Country**: wheter an accession with **Missmatch ORIGCTY** has their reported country of origin in a bordering country.
* **Elevation difference**: wheter the elevation reported in the passport data has a difference less than 150 meters with respect to the [STRM-elevation](https://srtm.csi.cgiar.org/) of the coordinate.
* **Collection site description**: wheter or not an accession has the collsite description available.
* **Admon level 1**: wheter an accession collsite description match the first administrative level GADM name where the coordinate falls.
* **Admon level 2**: wheter an accession collsite description match the second administrative level GADM name where the coordinate falls.


## Input data for quality score

To calculate the quality score, data was collected from the following external sources:


* **Country administrative levels 1 and 2 (GADM)**:The dataset of Global Administradive Areas, is a high-resolution database that map the administrative areas of all the countries around the world, this includes boundaries for all the levels of division for each one, at any period of time.  The version 3.6 was used and was downloaded using the `geodata` R library using the function `geodata::gadm`.
* **STRM-elevation**: It is a topographic database of the Earth at one kilometer resolution, created by the Shuttle Radar Topography Mission, and which has digital information about land elevations at a near-global scale, from 56°S to 60°N [STRM-CGIAR](https://srtm.csi.cgiar.org/).
* **Biodiversity institutions, City, County and Country centroids**:This dataset is a dataframe of central coordinates compiled from multiple sources.  It includes data from registered biodiversity institutions, as provided by the  `coordinateCleaner::institutions` R-library (for more details, see the library documentation). Centroids data for Cities and Counties was sourced from the **Weighted Centroid of the world largest cities** ([Github](https://gist.github.com/Fil/17fc857c3ce36bf8e21ddefab8bc9af4)). Countries were obtained from two different sources: Google maps ([link](https://developers.google.com/public-data/docs/canonical/countries_csv)) and World country centroids ([GitHub](https://github.com/gavinr/world-countries-centroids/blob/master/dist)). 
* **Country Borders**: Dataframe with countries ISO3 code and their respective bordering neighbour country ISO3 code (csv format).
* **Data dictionary**: Data dictionary with description of each variable used for quality score calculation (xlsx format).
* **Decision tree**: Dataframe with the different combinations of paths/branches for the verification fields. 












