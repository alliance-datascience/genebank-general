# Clean occurrence data for distribution analysis 
# Maria Victoria Diaz, Andres Mendez
# 2023
# Alliance of Bioversity and CIAT

suppressMessages(if(!require(pacman)){install.packages("pacman");library(pacman)}else{library(pacman)})
pacman::p_load(data.table, maptools, sf, here, geodata, dplyr, stringr, stringi, 
               terra, vroom, httr, jsonlite, stringr, cld3)


count_decimals <- function(number){
  dif <- abs(number) - floor(abs(number))
  dif <-  sapply(dif, format, scientific =F)
  to_ret <- ifelse(dif == "0", 0, 
                   ifelse(dif == "NA", NA, nchar(gsub("0.", "", dif )) ) )
  return(to_ret)
}


lon_pattern_id <- function(df){
  
    ids <- sapply(unique(df$DECLATITUDE), function(lat){
      pos_lat <- which(df$DECLATITUDE == lat )
      # df[df$DECLATITUDE == lat, c("id","GADM_GID_0", "check_country_match", "DECLATITUDE", "DECLONGITUDE")]
      if(length(pos_lat) > 1){
        unq_lon <- unique(df$DECLONGITUDE[pos_lat])
        if(length(unq_lon) > 1){
          to_ret <- df$id[pos_lat]
        }else{
          to_ret <- numeric(0)
        }
      }
      
      return(to_ret)
      
    })
    
  ids <- unique(unlist(ids))
    return(ids)
  
}

lat_pattern_id <- function(df){
  
  ids <- sapply(unique(df$DECLONGITUDE), function(lon){
    pos_lon <- which(df$DECLONGITUDE == lon )
    # df[df$DECLATITUDE == lat, c("id","GADM_GID_0", "check_country_match", "DECLATITUDE", "DECLONGITUDE")]
    if(length(pos_lon) > 1){
      unq_lon <- unique(df$DECLATITUDE[pos_lon])
      if(length(unq_lon) > 1){
        to_ret <- df$id[pos_lon]
      }else{
        to_ret <- numeric(0)
      }
    }
    
    return(to_ret)
    
  })
  
  ids <- unique(unlist(ids))
  return(ids)
  
}
#set_here()



# Function

shp_wrld     <- terra::vect("C:/Users/acmendez/Downloads/gadm36.gpkg")
data_pth <- "C:/Users/acmendez/Downloads/genesys-accessions-COL003.csv"

cleaning_process<-function(data_pth, shp_wrld){
  
  
  cat("Reading institute passport data...", "\n")
  
  data <- read.csv(data_pth)
  data$id <- 1:nrow(data)
  #names(data)[which(names(data) %in% c("DECLATITUDE", "DECLONGITUDE"))]<- c("Latitude", "Longitude")
  
  
  cat("Identifying the accessions with location issues...", "\n")
  
  
  #########################################
  ##### check data without collSite #######
  #########################################
  
  
  data$check_collsite <- TRUE
  data$check_collsite[is.na(data$COLLSITE)] <- FALSE
  
  
  ##############################################
  ##### Separate data with no coodinates #######
  ##############################################
  
  data$check_location_available <- NA
  
  data$check_location_available[!complete.cases(data[,c("DECLATITUDE", "DECLONGITUDE")])] <- FALSE
  
  data$check_location_available[complete.cases(data[,c("DECLATITUDE", "DECLONGITUDE")])] <- TRUE
  
  NAS_data <- data[which(data$check_location_available == FALSE),]
  
  COMPLETE_data <- data[which(data$check_location_available == TRUE),]
  
  COMPLETE_data <- GADM_extraction(df = COMPLETE_data,
                                   shp_dir = NULL,#"//alliancedfs.alliance.cgiar.org/cimmyt-aidi$/others",
                                   shp = shp_wrld)
  
  #################################
  ##### check zero coordinates ####
  #################################
  
  COMPLETE_data$check_zero_coords<-FALSE
  COMPLETE_data$check_zero_coords[which(COMPLETE_data$DECLONGITUDE == 0 | COMPLETE_data$DECLATITUDE == 0)] <- TRUE
  
  
  ####################################
  ##### check integer coordinates ####
  ####################################
  
  ### better to look for number of decimal places 
  COMPLETE_data$check_decimals_lon<- NA
  COMPLETE_data$check_decimals_lat<- NA
  
  COMPLETE_data$check_decimals_lon <- count_decimals(COMPLETE_data$DECLONGITUDE) 
  COMPLETE_data$check_decimals_lat <- count_decimals(COMPLETE_data$DECLATITUDE)
  
  #COMPLETE_data$check_decimals[which(is_decimal(COMPLETE_data$DECLONGITUDE) | is_decimal(COMPLETE_data$DECLATITUDE))] <- TRUE
  

  #############################################################################
  ##### check if countries are the same or the accessions are over the sea ####
  #############################################################################
  # system.time({
  #   points<-st_as_sf(COMPLETE_data, coords = c("DECLONGITUDE", "DECLATITUDE"), crs = st_crs(wrld))
  #   
  #   resultado <- st_join(points, wrld); rm(wrld)
  #   #360 segundos - 6 minutos
  # })
  #COMPLETE_data$check_suggested_country <- resultado$GID_0
  
  COMPLETE_data$check_country_match <- dplyr::case_when(
    COMPLETE_data$ORIGCTY == COMPLETE_data$GADM_GID_0 ~ "OK",
    COMPLETE_data$ORIGCTY != COMPLETE_data$GADM_GID_0 ~ "NO_MATCH",
    is.na(COMPLETE_data$GADM_GID_0)  ~ "SEA",
    .default =  "SEA"
  )
  ############################################################
  ##### check if the collSite is same as it's reported #######
  ############################################################
  
  
  #COMPLETE_data <- data.frame(COMPLETE_data, collsite_gadm = paste(resultado$NAME_5, resultado$NAME_4, resultado$NAME_3, resultado$NAME_2, resultado$NAME_1, sep = ", "));# rm(resultado)
  #COMPLETE_data$collsite_gadm<-gsub("NA,", "", COMPLETE_data$collsite_gadm)
  #COMPLETE_data$collsite_gadm<-gsub(" ", "", COMPLETE_data$collsite_gadm)
  #COMPLETE_data$collsite_gadm[which(COMPLETE_data$collsite_gadm == "NA")]<- NA
  
  
  #############################################
  ## Check duplicated DECLATITUDE or DECLONGITUDE ###
  #############################################
  
  
  coords_patterns <- lapply(unique(COMPLETE_data$GADM_GID_0), function(iso){
    
    df <- COMPLETE_data[COMPLETE_data$GADM_GID_0 == iso & COMPLETE_data$check_country_match == "OK" & !is.na(COMPLETE_data$GADM_GID_0), ]
    
    
    to_ret <- list(lon_pattern = lon_pattern_id(df),
                   lat_pattern = lat_pattern_id(df))
    
    return(to_ret)
  })
  
  lon_pattern_pos <- unique(unlist(sapply(coords_patterns, function(lst){lst$lon_pattern})))
  lat_pattern_pos <- unique(unlist(sapply(coords_patterns, function(lst){lst$lat_pattern})))
  
  COMPLETE_data$check_lon_pattern <- FALSE
  COMPLETE_data$check_lat_pattern <- FALSE
  
  COMPLETE_data$check_lon_pattern[COMPLETE_data$id %in% lon_pattern_pos] <- TRUE
  COMPLETE_data$check_lat_pattern[COMPLETE_data$id %in% lat_pattern_pos] <- TRUE
  
  ######## aquiiiii voyyyyyyyyyyyyyyyyyyyyyy de aqui para abajo ########3
  
  # to_plot <- COMPLETE_data %>% 
  #   dplyr::filter(check_lon_pattern | check_lat_pattern) %>% 
  #   dplyr::distinct(DECLATITUDE, DECLONGITUDE, .keep_all = T) 
  # 
  # leaflet() %>% 
  #   addTiles() %>% 
  #   # addPolygons(data = c_shp,
  #   #             weight = 1,
  #   #             fill = F) %>% 
  #   addCircleMarkers(
  #     lng = to_plot$DECLONGITUDE,
  #     lat = to_plot$DECLATITUDE,
  #     radius = 4,
  #     weight = 1,
  #     stroke = F,
  #     fillOpacity = 1
  #   )
  # 
  
  
  # no_sea <- which(COMPLETE_data$check_country_issue != "SEA")
  # sea<-which(COMPLETE_data$check_country_issue == "SEA")
  # 
  # 
  # COMPLETE_data_sea <- COMPLETE_data[sea,]
  # 
  # COMPLETE_data_no_sea <- COMPLETE_data[no_sea,]
  # 
  # countries <-unique(COMPLETE_data_no_sea$GADM_GID_0)
  # 
  # 
  # x <- lapply(1:length(countries), function(i){
  #   
  #   cat(i, "\n")
  #   
  #   
  #   select_c <- COMPLETE_data_no_sea[which(COMPLETE_data_no_sea$GADM_GID_0 == countries[i]),]
  #   
  #   select_c <-select_c[order(select_c$DECLATITUDE, decreasing = F),]
  #   
  #   select_c$DECLATITUDE_pattern <-FALSE
  #   select_c$DECLONGITUDE_pattern <-FALSE
  #   
  #   select_c[which(duplicated(select_c$DECLATITUDE) & !duplicated(select_c$DECLONGITUDE)),"DECLATITUDE_pattern"]<-TRUE
  #   select_c[which(duplicated(select_c$DECLONGITUDE) & !duplicated(select_c$DECLATITUDE)),"DECLONGITUDE_pattern"]<-TRUE
  #   
  #   return(select_c)
  #   
  # })
  # 
  # 
  # COMPLETE_data_no_sea<-do.call(rbind, x)
  # COMPLETE_data_sea$DECLONGITUDE_pattern <-NA
  # COMPLETE_data_sea$DECLATITUDE_pattern <-NA
  # 
  
  ######################
  ##### altitude #######
  ######################
  
  
  p_no_sea <- points[no_sea,]
  
  COMPLETE_data_no_sea$elev_suggested <- lapply(1:nrow(COMPLETE_data_no_sea), function(i){
    
    cat(i, "\n")
    
    
    print(COMPLETE_data_no_sea$suggested_country[i])
    
    
    if(!is.na(COMPLETE_data$suggested_country[i])){
      
      if(COMPLETE_data$suggested_country[i] != "XKO" ){
        
        l <- terra::extract( geodata::elevation_30s(country = COMPLETE_data$suggested_country[i], path = './tmpr'), p_no_sea[i,] )
        
      }else{
        
        
        l <-data.frame(ID = NA,XKO_elv_msk = NA)
      }
      
      
    }else{
      
      l <-data.frame(ID = NA,elv_msk = NA)
      
    }
    
    #pares de coordenadas cuántos tienen sitios de colecta diftes
    #mismo accesion ID con difte año de recibido está bien
    
    
    
    return(l[,2])
    
    
    
    
  })
  
  COMPLETE_data_sea$elev_suggested <-NA
  
  COMPLETE_data_def <- rbind(COMPLETE_data_no_sea, COMPLETE_data_sea)
  
  
  
  cat("Returning data", "\n")
  
  return(list(location_available  = COMPLETE_data_def, location_no_available = NAS_data))
  
  
}






##### test ####

#files = list.files(here("data"), pattern = ".csv", full.names = T)
#wrld<- st_read("C:/Users/acmendez/Downloads/gadm36.gpkg")#st_read(here("data/gadm/gadm36_shp/gadm36.shp"))
#check <- cleaning_process(data = files[3], wrld = gadm1)
#s<-check$location_available
#n <- check$location_no_available





