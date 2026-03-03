# Check for geographical coordinates issues for Gensys institute data
# Maria Victoria Diaz, Andres Mendez
# 2023
# Alliance of Bioversity and CIAT

suppressMessages(if(!require(pacman)){install.packages("pacman");library(pacman)}else{library(pacman)})
pacman::p_load(data.table, sf, here, geodata, dplyr, stringr, stringi,writexl, arrow, readr,
               terra, vroom, httr, jsonlite, stringr, cld3, geosphere, readxl, paws, paws.storage, here)

##CHECCCCKK ACCESION ACCNUMB 135

here::here()

##########################
### FUNCTIONS ###########
########################

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


#' Function to get the number of decimal places for LATITUD and LONGITUD
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


#' Function to extract information of GADM country administrative levels based on coordinate
#' @param iso3 (character) Three letter country iso Code
#' @param lat (numeric) decimal latitude
#' @param lng (numeric) decimal longitude
#' @param df (data.frame) genesys data.frame
#' @param shp (spatVect) world GADM shapefile
#' @param shp_lst_pth (character) path/paths to shapefile folder
#' @return Data.frame with GADM data extracted for coordinate and standardized columns names
#shp_dir<-list.files("//alliancedfs.alliance.cgiar.org/cimmyt-aidi$/others", recursive = T, full.names = T, pattern  = ".shp$")
GADM_extraction <- function(df = NULL, 
                            shp = NULL,
                            shp_lst_pth = NULL){
  
  stopifnot("data.frame is null" = !is.null(df))
  stopifnot("Required columns names not present in data" = all(c("PAIS_ORIGEN", "LATITUD", "LONGITUD") %in% names(df)))
  
  #iso3 <- unique(df$ORIGCTY)
  #user can load country shapefile or Wordl shapefile
  
  
  cog_coord <- terra::vect( matrix(c( df$LONGITUD, df$LATITUD), ncol = 2),
                            type = 'points', 
                            crs =  terra::crs("epsg:4326"))
  
  #plot(shp);points(cog_coord, col = "red")
  
  if(!is.null(shp_lst_pth)){
    
    res <- lapply(shp_lst_pth, function(pth){
      cat("Getting data from: ", basename(pth), "\n")
      shp <- terra::vect(pth)
      res <- terra::relate(cog_coord, shp, relation = "intersects", pairs = TRUE, na.rm = F)
      shp <- terra::as.data.frame(shp)
      names(shp) <- paste0("GADM_", names(shp))
      pos_shp <- grepl(pattern = 'GADM_GID_0|GADM_COUNTRY|GADM_NAME_[0-9]|GADM_VARNAME_[0-9]',names(shp))
      #to_ret <- base::merge(df, aa, by.x = "shp_id",by.y = "GADM_UID", all.x = T, all.y = F)
      to_ret<- data.frame(id = df$id, shp[res[,2], pos_shp])
      to_ret <- to_ret[complete.cases(to_ret$GADM_GID_0), ]
      return(to_ret)
    })
    
    res <- res[sapply(res, nrow)>0]
    res <- dplyr::bind_rows(res)
    row.names(res) <- NULL
    
    to_ret <- base::merge(df, res, by = "id", all.x = T, all.y = F)
    
    
  }else{
    res <- terra::relate(cog_coord, shp, relation = "intersects", pairs = TRUE, na.rm = F)
    shp <- terra::as.data.frame(shp)
    gc()
    stopifnot("error in terra::relate" = ncol(res)==2)
    #df$shp_id <- shp$UID[res[,2]]
    names(shp) <- paste0("GADM_", names(shp))
    pos_shp <- grepl(pattern = 'GADM_GID_0|GADM_COUNTRY|GADM_NAME_[0-9]|GADM_VARNAME_[0-9]',names(shp))
    #to_ret <- base::merge(df, aa, by.x = "shp_id",by.y = "GADM_UID", all.x = T, all.y = F)
    to_ret<- data.frame(df, shp[res[,2], pos_shp])
  }
  
  
  
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
    
    aa <- ifelse(ps, paste0("Admin_level_", lvl), NA)
    
    bb <-  GADM_df[ifelse(ps, ps, NA), paste0("std_NAME_", lvl)]
    bb <- gsub("\\\\b", "",bb)
    
    to_ret <- list(logical = aa, matched_names = bb)
    
  })
  names(matchs)  <- paste0("lvl_", 1:admon_level)
  adm_lvl_matchs <- do.call(data.frame, lapply(matchs, function(lst)lst$matched_names) ) 
  names(adm_lvl_matchs) <- paste0("admon_lvl_", 1:ncol(adm_lvl_matchs), "_matched")
  matchs         <- do.call(data.frame, lapply(matchs, function(lst)lst$logical) )
  
  GADM_df <- data.frame(GADM_df, adm_lvl_matchs)
  
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


#' Function to get accession issues descriptive text
#' @param var_df (data.frame) with score selected variables
#' @param text_description (data.frame) data dictionary
#' @return character vector of descriptive text for each accession
get_text_desc <- function(var_df, text_description){
  
  #text_description[text_description$variable %in% names(var_df), c("variable", "descriptive_issue_text" )]
  tmp1 <- lapply(names(var_df), function(nms){
    vr <- var_df[, grepl(paste0("\\b",nms, "\\b"), names(var_df)) ]
    txt <- unlist(text_description[text_description$variable == nms , "descriptive_issue_text"])
    to_ret <- ifelse(vr != 1, txt , "")
    return(to_ret)
  })
  #140264  
  tmp1 <- do.call(cbind, tmp1)
  
  to_ret <- apply(tmp1, 1, function(vec){
    vec <- vec[nchar(vec) > 0 ]
    if(any(grepl("Geographical Coordinate in SEA", vec))){
      vec <- "Geographical Coordinate in SEA" 
    }
    paste("Issues found: ", paste0(vec, collapse = ", "))
  })
  
  return(to_ret)
  
}



#' Function to identify accession with empty roads distance
#' @param id_lonlat_av (character) Vector of id_lonlat for existing coordinates
#' @param df (data.frame) accession data.frame 
#' @return Numeric vector of missing id
id_empty_roads_dist <- function(id_lonlat_av, df){
  
  #roads_db_path = "//alliancedfs.alliance.cgiar.org/cimmyt-aidi$/acc_roads_dist_db.parquet"
  
  roads_dist_db_id <- unlist(id_lonlat_av)
  
  missing_id <- unlist(df[!df$id_lonlat %in% roads_dist_db_id,  "id_lonlat"])
  
  return(missing_id)
}


#' internal function to compute geodesic distance from points to nearest roads
#' @param iso (character) accession roads distance database full path
#' @param df (data.frame) Lon, Lat data.frame
#' @param av_iso (character) character vector of avilable iso3 country code
#' @return data.frame with geodesic distance from roads to neares accession
calc_road_dist <- function(iso, df, av_iso){
  cat(">>> calculating distances to roads for: ", iso, "\n")
  if(!is.na(iso)){
    
    coords <- base::subset(df, GADM_GID_0 == iso, select =  c("LONGITUD", "LATITUD", "id_lonlat")) 
    coords <- coords[!duplicated(data.frame(coords$LONGITUD, coords$LATITUD)),]
    
    stopifnot("missng values found. " = !any(is.na(coords$LONGITUD)) )
    if(any(grepl(iso, av_iso)) ){
      roads_shp_pth <- av_gpkg[grepl(iso, av_iso)] #use av_countries
      
      #shp <- lapply(roads_shp_pth, terra::vect)
      shp <- terra::vect(roads_shp_pth)
      
      
      spatial_points <- terra::vect(coords,  crs = terra::crs(shp), geom = c("LONGITUD", "LATITUD") )
      
      dists <- terra::nearest(spatial_points, shp)
      
      
      coords$dist_to_roads <- round(dists$distance, 0 )
      
    }else{
      
      coords$dist_to_roads <- NA
    }
  }else{
    coords <- base::subset(df, is.na(GADM_GID_0), select =  c("LONGITUD", "LATITUD", "id_lonlat")) 
    coords <- coords[!duplicated(data.frame(coords$LONGITUD, coords$LATITUD)),]
    coords$dist_to_roads <- NA
  }
  
  return(coords)
} 


#' Function to compute geodesic distance from points to nearest roads
#' @param df (data.frame) Lon, Lat data.frame
#' @param roads_cnt_desc (data.frame) world roads shapefiles data dictionary
#' @param roads_db_path (character) accession roads distance database full path
#' @param roads_root_pth (character) path to roads root dir
#' @param doPar (logical) whether to use parallel processing
#' @return Numeric vector of distances to the nearest road

dist_to_roads <- function(df, roads_root_pth, roads_db_path, doPar = F){
  
  
  stopifnot("acc_roads_dist_db.parquet does not exists." = file.exists(roads_db_path))
  
  dists_df <- arrow::read_parquet(roads_db_path)
  df$id_lonlat <- paste0(df$DECLONGITUDE,":",df$DECLATITUD)
  
  av_gpkg <- list.files(roads_root_pth, full.names = T, pattern = ".gpkg$")
  av_iso <- gsub("_OSM_roads.gpkg", "",basename(av_gpkg))
  
  missing_id <-  id_empty_roads_dist(dists_df$id_lonlat, df)
  
  if(length(missing_id) != 0){
    
    tmp_df <- df[df$id_lonlat %in% missing_id, ]
    
    av_countries <- unique(tmp_df$GADM_GID_0)
    
    dists_df_to_add <- lapply(av_countries, calc_road_dist, df = tmp_df, av_iso = av_iso )
    dists_df_to_add <- do.call(rbind, dists_df_to_add)
    
    dists_df <- rbind(dists_df, dists_df_to_add)
    
    
  }
  
  dists_df <- dists_df[, c("id_lonlat", "dist_to_roads")]
  
  #aa <- base::merge(df, dists_df, by = "id_lonlat", all.x = T, sort = F)
  df <- dplyr::left_join(df, dists_df, by = "id_lonlat")
  
  return(df$dist_to_roads)
}



#' Function to identify accession with coordinates in country, departmnet and city centroid
#' @param centroid_df (data.frame) dataframe of centroids Coordinates
#' @param df (data.frame) Lon, Lat, type data.frame
#' @param buffer_rad (numeric) buffer radius 
#' @return Logical data.frame indicating if accesion is away from centroid (FALSE) 

dist_to_centroids <- function(centroid_df, df, buffer_rad){
  
  
  #centroid_df sources:
  #world-country-centroids: https://gavinr.com/open-data/world-countries-centroids/
  #world-cities-centroids: https://gist.github.com/Fil/17fc857c3ce36bf8e21ddefab8bc9af4
  #world_departments-centroids: CoordinateCleaner::countryref
  #world_institutions-centroids: CoordinateCleaner::institutions
  
  #centroid_df = read_csv("C:/Users/acmendez/Downloads/centroid_checks_df.csv")
  #df = vroom::vroom("D:/OneDrive - CGIAR/Documents/genesys_quality_score.csv")
  #buffer_rad = 1000
  
  occ <- terra::vect(df, c("LONGITUD", "LATITUD"), crs = "EPSG:4326")
  
  checks <- lapply(unique(centroid_df$TYPE), function(tp){
    print(tp)
    df_type <- centroid_df[centroid_df$TYPE == tp, ]
    
    cent_bf <- terra::buffer(terra::vect(df_type, c("LONGITUDE", "LATITUDE"), crs = "EPSG:4326"), width = buffer_rad)
    
    ext_data <- terra::extract(cent_bf, occ)
    ext_data <- ext_data[!duplicated(ext_data[, 1]),]
    
    
    to_ret <- data.frame( "var1" =  !is.na(ext_data[, 2]))
    names(to_ret) <- paste0("check_centroid_", tp)
    
    return(to_ret)
  })
  
  return(do.call(cbind, checks)) 
  
}



#' Main Function to implement coordinates checking proccess for genesys data V2.0, new 
#' way of compute priority score
#' @param data_pth (character) path to genesys institute data
#' @param shp_pth (character) path/paths to  shapefiles
#' @param elev_pth (character) path to elevetion raster
#' @param cnt_code_pth (character) path to country code roads description file
#' @param roads_db_path (character) accession roads distance database full path
#' @param centroids_db_path (character) Centroids database full path
#' @param out_dir_pth (character) full output dir path 
#' @return List of dataframe containnig 
checking_process_v2<-function(data_pth, 
                              shp_pth, 
                              elev_pth, 
                              slope_pth, 
                              roads_root_pth,
                              data_dict_url,
                              roads_db_path,
                              centroids_db_path,
                              out_dir_pth){
  #data_pth = "C:/Users/acmendez/Downloads/genesys_downloaded_institutions_data (1).csv"
  #shp_pth = list.files("//alliancedfs.alliance.cgiar.org/cimmyt-aidi$/others", recursive = T, full.names = T, pattern  = ".shp$")
  #elev_pth = "C:/Users/acmendez/Downloads/elevation_30s.tif"
  #slope_pth = "C:/Users/acmendez/Downloads/slope_col.tif"
  #roads_root_pth = "//alliancedfs.alliance.cgiar.org/cimmyt-aidi$/OSM_roads"
  #cnt_code_pth = "C:/Users/acmendez/Downloads/country_roads_codes.xlsx"
  #data_dict_url = "https://github.com/alliance-datascience/genebank-general/blob/dev/scripts/r/misc/GCA_data_dictionary.xlsx"
  
  #e <- geodata::elevation_global(0.5, tempdir())
  #terra::writeRaster(e, "C:/Users/acmendez/Downloads/elevation_30s.tif")
  elev_rst <- terra::rast(elev_pth)
  slope_rst <- terra::rast(slope_pth)
  shp_paths <- shp_pth #terra::vect(shp_pth)
  centroid_df <- readr::read_csv(centroids_db_path)
  roads_cnt_desc <- readxl::read_excel(cnt_code_pth)
  
  
  
  #data_dic_pth <- tempfile(pattern = ".xlsx")
  #download.file(data_dict_url, data_dic_pth)
  #stopifnot("Download failed for Data dictionary" = file.exists(data_dic_pth))
  data_dic_pth <- data_dict_url
  
  text_description <- readxl::read_excel(data_dic_pth)
  
  
  cat("Reading institute passport data...", "\n")
  
  data <- read.csv(data_pth)
  data$id <- 1:nrow(data)
  #names(data)[which(names(data) %in% c("LATITUD", "LONGITUD"))]<- c("Latitude", "Longitude")
  
  
  cat("Identifying the accessions with location issues...", "\n")
  
  
  #########################################
  ##### check data without Ubicacion #####
  #########################################
  
  data$COLLSITE <- paste(stringr::str_replace_na(data$DEPARTAMENTO, replacement = ""), 
                                  stringr::str_replace_na(data$MUNICIPIO, replacement = ""),
                                  stringr::str_replace_na(data$UBICACION, replacement = ""), sep = " ")
  data$COLLSITE <- stringr::str_trim(data$COLLSITE)
  data$COLLSITE <- ifelse(nchar(data$COLLSITE) == 0, NA, data$COLLSITE)
  
  data$check_collsite <- TRUE
  data$check_collsite[is.na(data$COLLSITE)] <- FALSE
  
  
  ##############################################
  ##### Separate data with no coodinates #######
  ##############################################
  
  data$check_location_available <- NA
  
  data$check_location_available[!complete.cases(data[,c("LATITUD", "LONGITUD")])] <- FALSE
  
  data$check_location_available[complete.cases(data[,c("LATITUD", "LONGITUD")])] <- TRUE
  
  NAS_data <- data[which(data$check_location_available == FALSE),]
  
  COMPLETE_data <- data[which(data$check_location_available == TRUE),]
  
  rm(data)
  
  COMPLETE_data <- GADM_extraction(df = COMPLETE_data,
                                   shp_lst_pth = shp_paths,#"//alliancedfs.alliance.cgiar.org/cimmyt-aidi$/others",
                                   shp = NULL)
  
  #################################
  ##### check zero coordinates ####
  #################################
  
  COMPLETE_data$check_zero_coords<-FALSE
  COMPLETE_data$check_zero_coords[which(COMPLETE_data$LONGITUD == 0 | COMPLETE_data$LATITUD == 0)] <- TRUE
  
  
  ### better to look for number of decimal places 
  COMPLETE_data$check_decimals_lon<- NA
  COMPLETE_data$check_decimals_lat<- NA
  
  COMPLETE_data$check_decimals_lon <- count_decimals(COMPLETE_data$LONGITUD) 
  COMPLETE_data$check_decimals_lat <- count_decimals(COMPLETE_data$LATITUD)
  
  
  
  ########################
  ##### elevation #######
  ######################
  
  
  x <- terra::extract( elev_rst, COMPLETE_data[, c('LONGITUD', 'LATITUD')])
  
  COMPLETE_data$check_elev_suggested <-NA
  COMPLETE_data$check_elev_suggested <- x[,2]
  
  COMPLETE_data$check_elev_diff <- abs(COMPLETE_data$ALTITUD - COMPLETE_data$check_elev_suggested)
  COMPLETE_data$check_elev_cat  <- NA
  COMPLETE_data$check_elev_cat  <- ifelse(COMPLETE_data$check_elev_diff > 150, TRUE,  FALSE)
  
  
  #####################
  #### Slope #########
  ##################
  
  x <- terra::extract( slope_rst, COMPLETE_data[, c('LONGITUD', 'LATITUD')])
  
  COMPLETE_data$check_slope_suggested <-NA
  COMPLETE_data$check_slope_suggested <- round(x[,2],0)
  
  #####################
  ### Centroids ######
  ###################
  
  centroids <- dist_to_centroids(centroid_df = centroid_df ,
                                 df = COMPLETE_data, 
                                 buffer_rad = 1000)
  
  COMPLETE_data <- cbind(COMPLETE_data, centroids)
  
  #####################
  ## GADM scoring ####
  ###################
  
  COMPLETE_data$std_collsite <- string_std(COMPLETE_data$COLLSITE)
  
  COMPLETE_data <- GADM_quality_score(GADM_df = COMPLETE_data)
  
  #elastic score (sugerencia de curador de cassava juan jose)
  COMPLETE_data$GADM_quality_score <- dplyr::case_when(
    COMPLETE_data$GADM_quality_score == 0 ~ 0,
    COMPLETE_data$GADM_quality_score > 0 & COMPLETE_data$GADM_quality_score < 0.333 ~ 1,
    COMPLETE_data$GADM_quality_score >= 0.333 & COMPLETE_data$GADM_quality_score < 0.6 ~ 2,
    #output_df$GADM_quality_score_v2 > 0.5 & output_df$GADM_quality_score_v2 <= 0.8 ~ "Moderate-High",
    COMPLETE_data$GADM_quality_score >= 0.6 ~ 3 
  )
  
  COMPLETE_data$GADM_score_cats <- dplyr::case_when(
    COMPLETE_data$GADM_quality_score == 0 ~ "None",
    COMPLETE_data$GADM_quality_score == 1 ~ "Low",
    COMPLETE_data$GADM_quality_score == 2 ~ "Partial",
    #output_df$GADM_quality_score_v2 > 0.5 & output_df$GADM_quality_score_v2 <= 0.8 ~ "Moderate-High",
    COMPLETE_data$GADM_quality_score == 3 ~ "High" 
  )
  
  ############################
  ### COORDS on SEA #########
  ##########################
  
  COMPLETE_data$check_country_match <- ifelse(!is.na(COMPLETE_data$GADM_GID_0), 1, 0)
  
  
  ############################
  #### DISTANCE TO ROADS ####
  ##########################
  
  
  # COMPLETE_data$check_dist_to_roads <- dist_to_roads(
  #   df = COMPLETE_data[, c("LONGITUD", "LATITUD", "GADM_GID_0")] ,
  #   roads_root_pth = roads_root_pth,
  #   roads_db_path  = roads_db_path, 
  #   doPar = F)
  # 
  
  ###########################
  ##### MARKET ACCESSIONS ##
  #########################
  
  
  COMPLETE_data$check_market_acc <- grepl("retail|retailer|market", COMPLETE_data$std_collsite) & 
    !(grepl("^from .* market", COMPLETE_data$std_collsite) | 
        grepl("[0-9]* km [a-z]{1,2} of.*market|[0-9]* km [a-z]{1,2} from.*market",  COMPLETE_data$std_collsite))
  
  
  ##########################
  ### output categories ###
  #########################
  
  COMPLETE_data$check_centroid_cat <- apply(COMPLETE_data[, c("check_centroid_city", 
                                                              "check_centroid_country", 
                                                              "check_centroid_department", 
                                                              "check_centroid_institution")], 1, any)
  
  COMPLETE_data$check_centroid_cat <- ifelse(COMPLETE_data$check_centroid_cat, 
                                             "Georeferenced potential issue",
                                             ifelse(is.na(COMPLETE_data$check_centroid_cat),
                                                    NA, "Ok"))
  
  COMPLETE_data$check_precision_cat <- COMPLETE_data$check_zero_coords | COMPLETE_data$check_decimals_lon < 2 | COMPLETE_data$check_decimals_lat < 2 
  
  COMPLETE_data$check_precision_cat <- ifelse(COMPLETE_data$check_precision_cat, 
                                              "Coordinate precision issue",
                                              ifelse(is.na(COMPLETE_data$check_precision_cat), NA,
                                                     "Ok"))
  COMPLETE_data$check_admonlvl_cat <- ifelse(COMPLETE_data$GADM_score_cats == "Low" | COMPLETE_data$GADM_score_cats == "None", 
                                             "Country administrative level issue", 
                                             ifelse(is.na(COMPLETE_data$GADM_score_cats), NA, "Ok"))
  ###########################
  ### Final score ##########
  #########################
  
  
  var_to_use <- c("check_collsite", "check_elev_cat", "check_dist_to_roads", "GADM_score_cats")
  
  COMPLETE_data$final_score_raw <- (
    as.numeric(COMPLETE_data$check_collsite) +
      as.numeric(!ifelse(is.na(COMPLETE_data$check_elev_cat), TRUE, COMPLETE_data$check_elev_cat))+
      as.numeric(COMPLETE_data$check_precision_cat == "Ok" )+
      COMPLETE_data$GADM_quality_score
    #ifelse(COMPLETE_data$GADM_score_cats == "High", 3, ifelse(COMPLETE_data$GADM_score_cats == "Moderate", 2, 1))
  ) * COMPLETE_data$check_country_match
  
  
  ### solo el 325/10064 = 3.2% de las accesiones en colombia tienen DONORCODE
  ### solo el 2006/10064 = 19% de las accesiones en colombia tienen DONORNUMB
  ### solo el 3749/10064 = 37% de las accesiones en colombia tienen DONORNAME
  ### solo el 4138/10064 = 41% de las accesiones en colombia tienen OTHERNUMB
  
  
  COMPLETE_data$final_score_cats <- dplyr::case_when(
    COMPLETE_data$final_score_raw <= 2 ~ "High",
    COMPLETE_data$final_score_raw  >= 3 & COMPLETE_data$final_score_raw <= 4 ~ "Moderate",
    COMPLETE_data$final_score_raw >= 5 ~ "Low")
  
  var_df = data.frame(check_collsite          = as.numeric(COMPLETE_data$check_collsite) ,
                      check_elev_cat          = as.numeric(!ifelse(is.na(COMPLETE_data$check_elev_cat), TRUE, COMPLETE_data$check_elev_cat)), 
                      check_precision_cat     =  as.numeric(COMPLETE_data$check_precision_cat == "Ok" ),
                      GADM_score_cats         = as.numeric(COMPLETE_data$GADM_score_cats != "High"),
                      check_country_match_sea = as.numeric(COMPLETE_data$check_country_match != "SEA")
  )
  var_df$GADM_score_cats = ifelse(var_df$check_collsite == 0, 1, var_df$GADM_score_cats )
  
  COMPLETE_data$issue_txt_desc <- get_text_desc(var_df, text_description)
  
  
  #COMPLETE_data$complementary_cat <-  ifelse(COMPLETE_data$check_is_donated | COMPLETE_data$check_market_acc, "Collected in market/Donated", )
  
  nms_orig <- setdiff(names(COMPLETE_data), names(NAS_data))
  tmp_mtx <- matrix(NA, nrow = nrow(NAS_data), ncol = length(nms_orig))
  tmp_mtx <- as.data.frame(tmp_mtx)
  names(tmp_mtx) <- nms_orig
  
  tmp_mtx$issue_txt_desc <- "Issues found: Missing LATITUD or LONGITUD"
  tmp_mtx$final_score_raw <- 0
  tmp_mtx$final_score_cats <- "Low"
  tmp_mtx$check_country_match <- 0
  
  NAS_data <- cbind(NAS_data, tmp_mtx)
  
  COMPLETE_data <- rbind(COMPLETE_data, NAS_data)
  
  COMPLETE_data <- COMPLETE_data[order(COMPLETE_data$id),]
  
  cat("Returning data", "\n")
  
  #writexl::write_xlsx(final_df, here(out_dir_pth, "genesys_quality_score.xlsx"))
  write.csv(COMPLETE_data, here(out_dir_pth, "genesys_quality_score.csv"), row.names = F)
  
  
  return(COMPLETE_data)
  
  
}















