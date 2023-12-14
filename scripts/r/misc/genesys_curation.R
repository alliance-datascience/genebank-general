# Check for geographical coordinates issues for Gensys institute data
# Maria Victoria Diaz, Andres Mendez
# 2023
# Alliance of Bioversity and CIAT

suppressMessages(if(!require(pacman)){install.packages("pacman");library(pacman)}else{library(pacman)})
pacman::p_load(data.table, maptools, sf, here, geodata, dplyr, stringr, stringi, sp,
               terra, vroom, httr, jsonlite, stringr, cld3, geosphere, readxl)



##########################
### FUNCTIONS ###########
########################
#'Function to download country sahpefile using geodata package
#' @param iso (character) Three letter country iso code
#' @param out (charcter) path to folder where to download country shapefile
#' @return Country shapefile in terra::vect format
get_iso_shapefile <- function(iso = 'KEN', out = NULL){
  
  out_dir <-  paste0(out, "/", iso)
  out_file <- paste0(out_dir, "/", iso,".shp")
  
  if(!file.exists(out_file)){
    levels <- 5:1
    for(i in 1:length(levels)){
      tryCatch(expr = {
        shp <- geodata::gadm(country = iso, level = levels[i], path = tempdir(), resolution = 1)
        terra::writeVector(shp, out_file)
        break
      },
      error = function(e){
        cat(paste0("Getting GADM level ",levels[i]," failed... Trying a higher level\n"))
        return("\n")
      })
    }
    
  }else{
    shp <- terra::vect(x = out_file)
  }
  
  
  return(shp)
}

#' Function to standardize text columns
#' @param str (character)  unstandardized string
#' @return string without special charcters, lowercas and no accents
string_std <- function(str){
  #remover caracteres especiales y signos de puntuacion
  #remover espacios multiples
  
  cl_site <- stringr::str_replace_all(str, "[^[:alnum:]]+", " ")  
  cl_site <- stringr::str_replace_all(cl_site, "[[:punct:]]+", " ") 
  cl_site <- stringr::str_squish(cl_site)
  cl_site <- stringr::str_to_lower(cl_site)
  cl_site <- stringi::stri_trans_general(cl_site, "Latin-ASCII") #remover acentos
  #dividir vectores por cada espacio
  to_ret <- cl_site#stringr::str_split(string = cl_site, pattern = "\\s")
  
  return((to_ret))
}

#' Function to standardize text columns from GADM data
#' @param str (character)  unstandardized string
#' @return string without special charcters, lowercas and no accents
string_std_GADM <- function(str){
  cl_site <- stringr::str_replace_all(str, "[[:punct:]]+", " ") 
  cl_site <- stringr::str_squish(cl_site)
  cl_site <- stringr::str_to_lower(cl_site)
  cl_site <- stringi::stri_trans_general(cl_site, "Latin-ASCII") #remover acentos
  #dividir vectores por cada espacio
  to_ret <- cl_site#stringr::str_split(string = cl_site, pattern = "\\s")
  
  return((to_ret))
}


#' Function to extract information of GADM country administrative levels based on coordinate
#' @param iso3 (character) Three letter country iso Code
#' @param lat (numeric) decimal latitude
#' @param lng (numeric) decimal longitude
#' @param df (data.frame) genesys data.frame
#' @param shp (spatVect) world GADM shapefile
#' @param shp_dir (character) path to shapefile folder
#' @return Data.frame with GADM data extracted for coordinate and standardized columns names

GADM_extraction <- function(df = NULL, 
                            shp = NULL,
                            shp_dir = NULL){
  
  stopifnot("data.frame is null" = !is.null(df))
  stopifnot("Required columns names not present in data" = all(c("ORIGCTY", "DECLATITUDE", "DECLONGITUDE") %in% names(df)))
  
  iso3 <- unique(df$ORIGCTY)
  #user can load country shapefile or Wordl shapefile
  if(is.null(shp)){
    stopifnot('shp_dir and iso3 must be specified' = all(!is.null(shp_dir), !is.null(iso3)))
    stopifnot("Multiple iso3 code when shp is null" = length(iso3) == 1)
    
    shp <- get_iso_shapefile(iso = iso3, out = shp_dir)
  }
  
  cog_coord <- matrix(c( df$DECLONGITUDE, df$DECLATITUDE), ncol = 2) %>% 
    terra::vect(., type = 'points', crs = terra::crs(shp))
  
  #plot(shp);points(cog_coord, col = "red")
  
  res <- terra::relate(cog_coord, shp, relation = "intersects", pairs = TRUE, na.rm = F)
  shp <- terra::as.data.frame(shp)
  gc()
  stopifnot("error in terra::relate" = ncol(res)==2)
  #df$shp_id <- shp$UID[res[,2]]
  names(shp) <- paste0("GADM_", names(shp))
  pos_shp <- grepl(pattern = 'GADM_GID_0|GADM_COUNTRY|GADM_NAME_[0-9]|GADM_VARNAME_[0-9]',names(shp))
  #to_ret <- base::merge(df, aa, by.x = "shp_id",by.y = "GADM_UID", all.x = T, all.y = F)
  to_ret<- data.frame(df, shp[res[,2], pos_shp])
  
  if(any(is.nan(res[,2]))){
    warning("coordinates out of shapefile were found")
  }
  ##############################
  ## text cleaning proccess ###
  ############################
  
  pos <- grepl(pattern = 'GADM_COUNTRY|GADM_NAME_[0-9]|GADM_VARNAME_[0-9]',names(to_ret))
  to_ret[, pos] <- apply(to_ret[, pos], 2, string_std_GADM)
  
  #usar NAME_ y VARNAME_ para crear una unica variable para comparar
  #\\b (boundary word) sirve para que el grepl o el str_detect busque el match exacto
  admon_level <- 4
  new_vars <- lapply(1:admon_level, function(i){
    nm  <- paste0("GADM_NAME_",i)
    vnm <- paste0("GADM_VARNAME_",i)
    new_var <- apply(to_ret[, c(nm, vnm)], 1, function(vec){
      cond <- ifelse(is.na(nchar(vec)), '' , vec)
      vec <- gsub(pattern ="\\|" , replacement = "\\\\b\\|\\\\b", vec)
      if(all(is.na(vec))){
        vec <- ""
      }else if(!any(nchar(cond) == 0) ){
        vec <- paste0("\\b", vec, "\\b")
        vec <- paste0(vec, collapse = "|")
      }else if(!all(nchar(cond) == 0) ){
        vec <- vec[nchar(cond) != 0]
        vec <- paste0("\\b", vec, "\\b")
      }else{
        vec <- ""
      }
      
      return((vec))
    }) 
    new_var <- unlist(new_var)
    
    return(new_var)
    
  })
  
  stopifnot("text length differs from each other" = all(sapply(new_vars, length) == nrow(to_ret)))
  
  names(new_vars) <- paste0("std_NAME_", 1:4)
  
  to_ret <- data.frame( to_ret, do.call(data.frame, new_vars))
  
  
  to_ret$shp_id <- NULL
  if(nrow(to_ret) == 0){
    to_ret <- NA
  }
  
  return(to_ret) 
}

#' Function to call google maps geocoding api for foward and reverse geocoding
#' @param address_text (character) address or place name to be geocoded
#' @param lat (numeric) decimal latitude for reverse geocoding
#' @param lng (numeric) decimal longitude for reverse geocoding
#' @param iso2 (character) tow letter ISO 3166-1 country code
#' @param geocode_type one of forward for address geocoding or 'reverse' for reverse geocoding
#' @param language (character) language in which results must be returned
#' @param gg_key (character) Secret google maps services API key
#' @return Data.frame with geocoded data returned by the google maps geocoding API
geocode <- function(address_text = NULL, 
                    lat = NULL, 
                    lng = NULL, 
                    iso2 = NULL, 
                    geocode_type = c('foward', "reverse"), 
                    language = "en",
                    gg_key = NULL){
  
  stopifnot("geocode_type must be of length 1" = length(geocode_type) == 1)
  stopifnot("geocode_type must be one of foward or reverse" = geocode_type %in% c('foward', "reverse"))
  stopifnot("Invlaid Google maps api key" = !is.null(gg_key))
  
  endpoint <- "https://maps.googleapis.com/maps/api/geocode/json?"
  
  if(geocode_type == 'foward'){
    #foward geocoding
    stopifnot("addrest_text is null" = !is.null(address_text))
    stopifnot("addrest_text is null" = !is.null(iso2))
    
    address_text <- gsub("[[:punct:]]", "", address_text)
    address_text <- gsub("[[:blank:]]+", "+", address_text)
    address_final <- URLencode(tolower(address_text))
    
    
    request <- paste0(endpoint, 
                      "address=",address_final, 
                      "&components=country:", iso2, 
                      "&language=", language,
                      "&key=", gg_key)  
    
    
  }else{
    #reverse geocoding
    stopifnot("Lat and lng must be provided" = all(c(!is.null(lat), !is.null(lng))))
    
    latlon <- paste0(lat, ",", lng)
    
    request <- paste0(endpoint, 
                      "latlng=",latlon, 
                      "&language=", language,
                      "&key=", gg_key)  
    
    
    
  }
  
  response <- httr::GET(url = request)
  if(httr::http_status(response)$category == "Success"){
    raw_info <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))$results
    
  }else{
    warning(httr::http_status(response)$message)
    raw_info <- NA
  }
  
  return(raw_info)
  
}



#' Function to calculate some quality scores based on GADM data
#' @param GADM_df (data.frame) output dataframe from  GADM_extraction function
#' @return (data.frame) with quality scores as columns 
GADM_quality_score <- function(GADM_df = NULL){
  
  cols_to_check <- c("std_NAME_1", "std_NAME_2", "std_NAME_3", "std_NAME_4", "std_collsite" )
  stopifnot("Required cols not present in data"= all(cols_to_check %in% names(GADM_df)))
  admon_level <- max(as.numeric(stringr::str_extract(cols_to_check, pattern = "[0-9]{1}")), na.rm = T)
  matchs <- lapply(1:admon_level, function(lvl){
    ps <- suppressWarnings(stringr::str_detect(GADM_df$std_collsite, GADM_df[, paste0("std_NAME_", lvl)]))
    
    
    ps <- ifelse(is.na(ps), FALSE, ps)
    
    to_ret <- ifelse(ps, paste0("Admin_level_", lvl), NA)
    
  })
  names(matchs) <- paste0("lvl_", 1:admon_level)
  matchs <- do.call(data.frame, matchs)
  
  GADM_df$GADM_adomlv_match <- apply(matchs, 1, function(vec){
    to_ret <- paste0(na.omit(vec), collapse =",")
    to_ret <- ifelse(nchar(to_ret) ==0, NA, to_ret)
  })
  
  GADM_df$GADM_max_admonlv <- rowSums(!is.na(GADM_df[, grepl("GADM_NAME_[1-4]", names(GADM_df))])  )
  
  GADM_df$GADM_admonlv_score <- apply(matchs, 1, function(vec){
    to_ret <- paste0(na.omit(vec), collapse =",")
    to_ret <- ifelse(nchar(to_ret) ==0, NA, to_ret)
    to_ret <- as.numeric(unlist(stringr::str_extract_all(to_ret, "[0-9]{1}" )))
    to_ret <- sum(to_ret, na.rm = T)
    return(to_ret)
  })
  
  
  GADM_df$GADM_max_admonlv_score <- sapply(GADM_df$GADM_max_admonlv, function(i){
    if(!any(is.na(i))){
      to_ret <- sum(seq(1:i))
    }else{
      to_ret <- NA
    }
    return(to_ret)
  })
  
  
  
  GADM_df$GADM_quality_score <- GADM_df$GADM_admonlv_score/GADM_df$GADM_max_admonlv_score
  
  return(GADM_df)
  
}



#' Function to get the number of decimal places for DECLATITUDE and DECLONGITUDE
#' @param number (numeric) numeric vector
#' @return 
#' Numeric vector of the number of decimal places
count_decimals <- function(number){
  dif <- abs(number) - floor(abs(number))
  dif <-  sapply(dif, format, scientific =F)
  to_ret <- ifelse(dif == "0", 0, 
                   ifelse(dif == "NA", NA, nchar(gsub("0\\.", "", dif )) ) )
  return(to_ret)
}


#' Function to identify straight lines patter in longitude
#' @param df (data.frame) data.frame for ISO country
#' @return Logical vector indicating coordinates with straight line pattern
lon_pattern_id <- function(df){
  
    ids <- sapply(unique(df$DECLATITUDE), function(lat){
      pos_lat <- which(df$DECLATITUDE == lat )
      # df[df$DECLATITUDE == lat, c("id","GADM_GID_0", "check_country_match", "DECLATITUDE", "DECLONGITUDE")]
      if(length(pos_lat) > 1){
        unq_lon <- unique(df$DECLONGITUDE[pos_lat])
        if(length(unq_lon) > 1){
          # only check for coordinates patterns between pairs of coordinates if distance is less than 100 Km
          dsts <- sp::spDists(x = cbind(df$DECLONGITUDE[pos_lat], df$DECLATITUDE[pos_lat]))
          pos_lat <- pos_lat[apply(dsts > 0 & dsts <= 0.89, 1, any )]
          to_ret <- df$id[pos_lat]
        }else{
          to_ret <- numeric(0)
        }
      }else{
        to_ret <- numeric(0)
      }
      
      ## 100 km distance
      #sp::spDists(x = rbind(c(-76.35588494853874, 3.504975766780184) , c(-76.10693302817242, 4.36508291060829) ))
      ## 50 km distance
      #sp::spDists(x = rbind(c(-76.35588494853874, 3.504975766780184) , c(-76.22990695145444, 3.9366495631055813) ))
      
      
      return(to_ret)
      
    })
    
  ids <- unique(unlist(ids))
    return(ids)
  
}

#' Function to identify straight lines patter in longitude
#' @param df (data.frame) data.frame for ISO country
#' @return Logical vector indicating coordinates with straight line pattern
lat_pattern_id <- function(df){
  
  ids <- sapply(unique(df$DECLONGITUDE), function(lon){
    pos_lon <- which(df$DECLONGITUDE == lon )
    # df[df$DECLATITUDE == lat, c("id","GADM_GID_0", "check_country_match", "DECLATITUDE", "DECLONGITUDE")]
    if(length(pos_lon) > 1){
      unq_lon <- unique(df$DECLATITUDE[pos_lon])
      if(length(unq_lon) > 1){
        # only check for coordinates patterns between pairs of coordinates if distance is less than 100 Km
        dsts <- sp::spDists(x = cbind(df$DECLONGITUDE[pos_lon], df$DECLATITUDE[pos_lon]))
        pos_lon <- pos_lon[apply(dsts > 0 & dsts <= 0.89, 1, any )]
        to_ret <- df$id[pos_lon]
      }else{
        to_ret <- numeric(0)
      }
    }else{
      to_ret <- numeric(0)
    }
    
    return(to_ret)
    
  })
  
  ids <- unique(unlist(ids))
  return(ids)
  
}
#set_here()

#' Function to get accession issues descriptive text
#' @param var_df (data.frame) with score selected variables
#' @param text_description (data.frame) data dictionary
#' @return character vector of descriptive text for each accession
get_text_desc <- function(var_df, text_description){
  
  text_description[text_description$variable %in% names(var_df), c("variable", "descriptive_issue_text" )]
  tmp1 <- lapply(names(var_df), function(nms){
    vr <- var_df[, grepl(nms, names(var_df))]
    txt <- unlist(text_description[text_description$variable == nms , "descriptive_issue_text"])
    to_ret <- ifelse(vr != 1, txt, "")
    return(to_ret)
  })
  
  tmp1 <- do.call(cbind, tmp1)
  
  to_ret <- apply(tmp1, 1, function(vec){
    vec <- vec[nchar(vec) > 0 ]
    paste("Issues found: ", paste0(vec, collapse = ", "))
  })
  
  return(to_ret)
  
}


#' Main Function to implement coordinates checking proccess for genesys data
#' @param data_pth (character) path to genesys institute data
#' @param shp_pth (character) path to world shapefile
#' @param elev_pth (character) path to elevetion raster
#' @return List of dataframe containnig 
checking_process<-function(data_pth, shp_pth, elev_pth, data_dic_pth){
  #data_pth = "C:/Users/acmendez/Downloads/genesys-accessions-COL003.csv"
  #shp_pth = "C:/Users/acmendez/Downloads/gadm36.gpkg"
  #elev_pth = "C:/Users/acmendez/Downloads/elevation_30s.tif"
  #data_dic_pth = "D:/OneDrive - CGIAR/Desktop/GCA_data_dictionary.xlsx"
  #e <- geodata::elevation_global(0.5, tempdir())
  #terra::writeRaster(e, "C:/Users/acmendez/Downloads/elevation_30s.tif")
  elev_rst <- terra::rast(elev_pth)
  shp_wrld <- terra::vect(shp_pth)
  text_description <- readxl::read_excel(data_dic_pth)
  
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

  
 
  ######################
  ##### altitude #######
  ######################
  
 
  x <- terra::extract( elev_rst, COMPLETE_data[, c('DECLONGITUDE', 'DECLATITUDE')])
  
  COMPLETE_data$check_elev_suggested <-NA
  COMPLETE_data$check_elev_suggested <- x[,2]
  
  COMPLETE_data$check_elev_diff <- abs(COMPLETE_data$ELEVATION - COMPLETE_data$check_elev_suggested)
  COMPLETE_data$check_elev_cat  <- NA
  COMPLETE_data$check_elev_cat  <- ifelse(COMPLETE_data$check_elev_diff > 500, TRUE,  FALSE)
  
  #####################
  ## GADM scoring ####
  ###################
  
  COMPLETE_data$std_collsite <- string_std(COMPLETE_data$COLLSITE)
  
  COMPLETE_data <- GADM_quality_score(GADM_df = COMPLETE_data)
  
  COMPLETE_data$GADM_score_cats <- dplyr::case_when(
    COMPLETE_data$GADM_quality_score < 0.333 ~ "Low",
    COMPLETE_data$GADM_quality_score >= 0.333 & COMPLETE_data$GADM_quality_score < 0.6 ~ "Moderate",
    #output_df$GADM_quality_score_v2 > 0.5 & output_df$GADM_quality_score_v2 <= 0.8 ~ "Moderate-High",
    COMPLETE_data$GADM_quality_score >= 0.6 ~ "High" 
  )
  
  
  COMPLETE_data$final_score_raw <- (
    as.numeric(COMPLETE_data$check_collsite) +
      as.numeric(!COMPLETE_data$check_zero_coords)*0.5+
      as.numeric(COMPLETE_data$check_decimals_lon >= 2)*0.5+
      as.numeric(COMPLETE_data$check_decimals_lat >= 2)*0.5+
      as.numeric(COMPLETE_data$check_country_match == "OK")+
      as.numeric(!COMPLETE_data$check_lon_pattern)*0.5+
      as.numeric(!COMPLETE_data$check_lat_pattern)*0.5+
      as.numeric(!ifelse(is.na(COMPLETE_data$check_elev_cat), TRUE, COMPLETE_data$check_elev_cat))+
      as.numeric(COMPLETE_data$GADM_score_cats == "High")
    #ifelse(COMPLETE_data$GADM_score_cats == "High", 3, ifelse(COMPLETE_data$GADM_score_cats == "Moderate", 2, 1))
  ) * as.numeric(COMPLETE_data$check_country_match != "SEA")
  
  
  COMPLETE_data$final_score_cats <- dplyr::case_when(
    COMPLETE_data$final_score_raw <= 3 ~ "Low",
    COMPLETE_data$final_score_raw  > 3 & COMPLETE_data$final_score_raw <= 5 ~ "Moderate",
    COMPLETE_data$final_score_raw > 5 ~ "High")
  
  var_df = data.frame(check_collsite = as.numeric(COMPLETE_data$check_collsite) ,
                      check_zero_coords= as.numeric(!COMPLETE_data$check_zero_coords),
                      check_decimals_lon = as.numeric(COMPLETE_data$check_decimals_lon >= 2),
                      check_decimals_lat = as.numeric(COMPLETE_data$check_decimals_lat >= 2),
                      check_country_match = as.numeric(COMPLETE_data$check_country_match == "OK"),
                      check_lon_pattern = as.numeric(!COMPLETE_data$check_lon_pattern),
                      check_lat_pattern = as.numeric(!COMPLETE_data$check_lat_pattern),
                      check_elev_cat = as.numeric(!ifelse(is.na(COMPLETE_data$check_elev_cat), TRUE, COMPLETE_data$check_elev_cat)),
                      GADM_score_cats = as.numeric(COMPLETE_data$GADM_score_cats == "High"),
                      check_country_match_sea = as.numeric(COMPLETE_data$check_country_match != "SEA")
  )
  
  COMPLETE_data$issue_txt_desc <- get_text_desc(var_df, text_description)
  
  cat("Returning data", "\n")
  
  #COMPLETE_data <- dplyr::bind_rows(COMPLETE_data, NAS_data)
  #COMPLETE_data <- COMPLETE_data[order(COMPLETE_data$id), ]
  
  return(list(location_available = COMPLETE_data, missing_coords = NAS_data ))
  
  
}


out_checks <- checking_process(data_pth = "C:/Users/acmendez/Downloads/genesys_downloaded_institutions_data (1).csv", 
                             shp_pth = "C:/Users/acmendez/Downloads/gadm36.gpkg", 
                             elev_pth  = "C:/Users/acmendez/Downloads/elevation_30s.tif",
                             data_dic_pth = "D:/OneDrive - CGIAR/Desktop/GCA_data_dictionary.xlsx")

final_df = out_checks$location_available
#check_collsite = FALSE -> 0 , TRUE -> 1
#check_zero_coords = TRUE -> 0, FALSE -> 1
#check_decimals_lon >= 2 -> 1, check_decimals_lon == 0 -> 0
#check_decimals_lat >= 2 -> 1, check_decimals_lat == 0 -> 0
#check_country_match == OK -> 1, check_country_match == NO_MATCH -> 0, check_country_match == SEA -> 0
#check_lon_pattern == TRUE -> 0, check_lon_pattern == FALSE -> 1
#check_lat_pattern == TRUE -> 0, check_lat_pattern == FALSE -> 1
#check_elev_cat == TRUE -> 0, check_elev_cat == NA -> 0, check_elev_cat == FALSE -> 1
#GADM_score_cats == Low -> 0, GADM_score_cats == Moderate -> 0, GADM_score_cats == High -> 1
# total de nueve variables, si a cada una se le asigna un valor de 1 entonces el score maximo sumaria 9
#el minimo seria 0


var_to_use <- c("check_collsite", "check_zero_coords", "check_decimals_lon", 
                "check_decimals_lat", "check_country_match", "check_lon_pattern", "check_lat_pattern",
                "check_elev_cat", "GADM_score_cats")




write.csv(final_df, "C:/Users/acmendez/Downloads/CIAT_checks_all.csv", row.names = F)


#table(final_df$final_score_raw, final_df$GADM_score_cats)
#table(final_score_cats, final_df$GADM_score_cats)
#table(final_df$final_score_cats)

final_df[final_score_raw == 5, c("id","DECLONGITUDE", "DECLATITUDE",var_to_use)] %>% View



