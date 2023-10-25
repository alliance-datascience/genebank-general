# Clean occurrence data for distribution analysis 
# Maria Victoria Diaz
# Alliance of Bioversity and CIAT
#..........................................
#..........................................
# Packages ####

suppressMessages(if(!require(pacman)){install.packages("pacman");library(pacman)}else{library(pacman)})
pacman::p_load(data.table, maptools,raster,rgdal, dismo, rgeos, sf, here, geodata, dplyr)
  
  

set_here()



# Function

cleaning_process<-function(data, wrld){
  
 
  cat("Reading institute passport data...", "\n")
  
  data <- read.csv(data)
  
  names(data)[which(names(data) %in% c("DECLATITUDE", "DECLONGITUDE"))]<- c("Latitude", "Longitude")
  
  NAS_data <- data[!complete.cases(data[,c("Latitude", "Longitude")]),]
  
  COMPLETE_data <- data[complete.cases(data[,c("Latitude", "Longitude")]),]
  

  cat("Identifying the accessions with location issues...", "\n")
  
    
  #############################################################################
  ##### check if countries are the same or the accessions are over the sea ####
  #############################################################################
  
  
  resultado <- st_join(st_as_sf(COMPLETE_data, coords = c("Longitude", "Latitude"), crs = st_crs(wrld)), wrld); rm(wrld)
  
  COMPLETE_data<- data.frame(COMPLETE_data, countries_gadm = resultado$GID_0); rm(resultado)
    
  COMPLETE_data$country_issue<-case_when(
      
      COMPLETE_data$ORIGCTY == COMPLETE_data$countries_gadm ~ "OK",
      COMPLETE_data$ORIGCTY != COMPLETE_data$countries_gadm ~ "NO_MATCH",
      is.na(COMPLETE_data$countries_gadm)  ~ "SEA",
      .default =  "SEA"
      
    )
    
  #################################
  ##### check zero coordinates ####
  #################################
    
  COMPLETE_data$zero_coords<-FALSE
  COMPLETE_data$zero_coords[which(COMPLETE_data$Longitude == 0 | COMPLETE_data$Latitude == 0)] <- TRUE
    
    
  ####################################
  ##### check integer coordinates ####
  ####################################
    
  COMPLETE_data$no_decimals<- FALSE
  COMPLETE_data$no_decimals[which(is.integer(COMPLETE_data$Longitude) | is.integer(COMPLETE_data$Latitude))] <- TRUE
    
    
  #########################################
  ##### check data without collSite #######
  #########################################
    
    
  COMPLETE_data$no_collsite <- FALSE
  COMPLETE_data$no_collsite[is.na(COMPLETE_data$COLLSITE)] <- TRUE
  
    
  NAS_data$no_collsite <- FALSE
  NAS_data$no_collsite[is.na(NAS_data$COLLSITE)] <- TRUE
  
  
  
  cat("Returning data", "\n")
  
  return(list(YES_coordinates  = COMPLETE_data, NO_coordinates = NAS_data))
    
    
}

##### test ####

files = list.files(here("data"), pattern = ".csv", full.names = T)

gadm <- st_read(here("data/gadm/gadm36_shp/gadm36.shp"))


check <- cleaning_process(data = files[1], wrld = gadm)

si <- check$YES_coordinates
no <- check$NO_coordinates














