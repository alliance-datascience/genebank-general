# Clean occurrence data for distribution analysis 
# Maria Victoria Diaz
# Alliance of Bioversity and CIAT
#..........................................
#..........................................
# Packages ####
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
  
  
  cat("Identifying the accessions with location issues...", "\n")
  
  
  #########################################
  ##### check data without collSite #######
  #########################################
  
  
  data$collsite <- TRUE
  data$collsite[is.na(data$COLLSITE)] <- FALSE
  
  
  ##############################################
  ##### Separate data with no coodinates #######
  ##############################################
  
  data$Location_available <-NA
  
  data$Location_available[!complete.cases(data[,c("Latitude", "Longitude")])]<-FALSE
  
  data$Location_available[complete.cases(data[,c("Latitude", "Longitude")])]<-TRUE
  
  NAS_data <- data[which(data$Location_available == FALSE),]
  
  COMPLETE_data <- data[which(data$Location_available == TRUE),]
  
  
  #################################
  ##### check zero coordinates ####
  #################################
  
  COMPLETE_data$zero_coords<-FALSE
  COMPLETE_data$zero_coords[which(COMPLETE_data$Longitude == 0 | COMPLETE_data$Latitude == 0)] <- TRUE
  
  
  ####################################
  ##### check integer coordinates ####
  ####################################
  
  COMPLETE_data$decimals<- FALSE
  COMPLETE_data$decimals[which(is.decimal(COMPLETE_data$Longitude) | is.decimal(COMPLETE_data$Latitude))] <- TRUE
  
  
  
  
  #############################################################################
  ##### check if countries are the same or the accessions are over the sea ####
  #############################################################################
  
  points<-st_as_sf(COMPLETE_data, coords = c("Longitude", "Latitude"), crs = st_crs(wrld))
  
  resultado <- st_join(points, wrld); rm(wrld)
  
  COMPLETE_data$suggested_country <- resultado$GID_0
  
  COMPLETE_data$country_issue<-case_when(
    
    COMPLETE_data$ORIGCTY == COMPLETE_data$suggested_country ~ "OK",
    COMPLETE_data$ORIGCTY != COMPLETE_data$suggested_country ~ "NO_MATCH",
    is.na(COMPLETE_data$suggested_country)  ~ "SEA",
    .default =  "SEA"
    
  )
  ############################################################
  ##### check if the collSite is same as it's reported #######
  ############################################################
  
  
  COMPLETE_data <- data.frame(COMPLETE_data, collsite_gadm = paste(resultado$NAME_5, resultado$NAME_4, resultado$NAME_3, resultado$NAME_2, resultado$NAME_1, sep = ", "));# rm(resultado)
  COMPLETE_data$collsite_gadm<-gsub("NA,", "", COMPLETE_data$collsite_gadm)
  COMPLETE_data$collsite_gadm<-gsub(" ", "", COMPLETE_data$collsite_gadm)
  COMPLETE_data$collsite_gadm[which(COMPLETE_data$collsite_gadm == "NA")]<- NA
  
  
  #############################################
  ## Check duplicated latitude or longitude ###
  #############################################
  
  
  no_sea <- which(COMPLETE_data$country_issue != "SEA")
  sea<-which(COMPLETE_data$country_issue == "SEA")
  
  
  COMPLETE_data_sea <- COMPLETE_data[sea,]
  
  COMPLETE_data_no_sea <- COMPLETE_data[no_sea,]
  
  countries <-unique(COMPLETE_data_no_sea$suggested_country)
  
  
  x <- lapply(1:length(countries), function(i){
    
    cat(i, "\n")
    
    
    select_c <- COMPLETE_data_no_sea[which(COMPLETE_data_no_sea$suggested_country == countries[i]),]
    
    select_c <-select_c[order(select_c$Latitude, decreasing = F),]
    
    select_c$Latitude_pattern <-FALSE
    select_c$Longitude_pattern <-FALSE
    
    select_c[which(duplicated(select_c$Latitude) & !duplicated(select_c$Longitude)),"Latitude_pattern"]<-TRUE
    select_c[which(duplicated(select_c$Longitude) & !duplicated(select_c$Latitude)),"Longitude_pattern"]<-TRUE
    
    return(select_c)
    
  })
  
  
  COMPLETE_data_no_sea<-do.call(rbind, x)
  COMPLETE_data_sea$Longitude_pattern <-NA
  COMPLETE_data_sea$Latitude_pattern <-NA
  
  
  ######################
  ##### altitude #######
  ######################
  
  
  e <- elevation_global(0.5, "tmp/")
  x <-raster::extract( e, COMPLETE_data_no_sea[, c('Longitude', 'Latitude')])
  
  COMPLETE_data_no_sea$elev_suggested <- x[,2]
  
  COMPLETE_data_sea$elev_suggested <-NA
  
  COMPLETE_data_def <- rbind(COMPLETE_data_no_sea, COMPLETE_data_sea)
  
  
  
  cat("Returning data", "\n")
  
  return(list(location_available  = COMPLETE_data_def, location_no_available = NAS_data))
  
  
}






##### test ####

#files = list.files(here("data"), pattern = ".csv", full.names = T)
#gadm1<- st_read(here("data/gadm/gadm36_shp/gadm36.shp"))
#check <- cleaning_process(data = files[3], wrld = gadm1)
#s<-check$location_available
#n <- check$location_no_available






