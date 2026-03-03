#SCRIPT TO GET THE COUNTRY, DEPARMENT, CITY AND INSTITUTION CENTROIDS FILE
#AUTHOR: ANDRES CAMILO MENDEZ
#YEAR:2024


require(pacman)
pacman::p_load(tidyverse, CoordinateCleaner)


ciudad_cent  <- read.csv("C:/Users/acmendez/Downloads/world_cities_centroid.csv")
country_cent <- read.csv("C:/Users/acmendez/Downloads/world_countries_centroids.csv") 
depart_cent  <- read.csv("C:/Users/acmendez/Downloads/world_departments_centroids.csv")


df1 <- ciudad_cent %>% 
  dplyr::select(NAME = Urban.Agglomeration, LATITUDE = Latitude, LONGITUDE = Longitude) %>% 
  dplyr::mutate(TYPE = "city") 

df2 <- country_cent %>% 
  dplyr::select(NAME = COUNTRY, LATITUDE = latitude, LONGITUDE = longitude) %>% 
  dplyr::mutate(TYPE = "country")

df3 <- depart_cent %>% 
  dplyr::select(NAME = name, LATITUDE  = centroid.lat, LONGITUDE = centroid.lon) %>% 
  dplyr::mutate(TYPE = "department")

df4 <- CoordinateCleaner::institutions %>% 
  dplyr::select(NAME = name, LATITUDE  = decimalLatitude, LONGITUDE = decimalLongitude) %>% 
  dplyr::mutate(TYPE = "institution")

bind_rows(df1, df2, df3, df4) %>% 
  tidyr::drop_na(LATITUDE, LONGITUDE) %>% 
  write.csv(., "C:/Users/acmendez/Downloads/centroid_checks_df.csv", row.names =FALSE)
