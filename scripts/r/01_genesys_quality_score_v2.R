# Check for geographical coordinates issues for Gensys institute data
# Maria Victoria Diaz, Andres Mendez
# 2023
# Alliance of Bioversity and CIAT

suppressMessages(if(!require(pacman)){install.packages("pacman");library(pacman)}else{library(pacman)})
pacman::p_load(data.table, sf, here, geodata, dplyr, stringr, stringi,writexl, arrow, readr,
               terra, vroom, httr, jsonlite, stringr, cld3, geosphere, readxl, paws, paws.storage, here)

##CHECCCCKK ACCESION ACCNUMB 135 - 
     
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


#' Function to extract information of GADM country administrative levels based on coordinate
#' @param df (data.frame) genesys data.frame
#' @param shp (spatVect) world GADM shapefile
#' @param shp_lst_pth (character) path/paths to shapefile folder
#' @return Data.frame with GADM data extracted for coordinate and standardized columns names
#shp_dir<-list.files("//alliancedfs.alliance.cgiar.org/cimmyt-aidi$/others", recursive = T, full.names = T, pattern  = ".shp$")
GADM_extraction <- function(df = NULL, 
                            shp = NULL,
                            shp_lst_pth = NULL){
  
  stopifnot("data.frame is null" = !is.null(df))
  stopifnot("Required columns names not present in data" = all(c("ORIGCTY", "DECLATITUDE", "DECLONGITUDE") %in% names(df)))
  
  #iso3 <- unique(df$ORIGCTY)
  #user can load country shapefile or Wordl shapefile
  
  
  cog_coord <- terra::vect( matrix(c( df$DECLONGITUDE, df$DECLATITUDE), ncol = 2),
                            type = 'points', 
                            crs =  terra::crs("epsg:4326"))
  
  #plot(shp);points(cog_coord, col = "red")
  
  if(!is.null(shp_lst_pth)){
    
    cog_coord$id <- df$id
    cog_coord <- terra::na.omit(cog_coord, geom = TRUE)
    
    res <- lapply(shp_lst_pth, function(pth){
      cat("Extracting data from: ", basename(pth), "\n")
      shp <- terra::vect(pth)
      if(length(unique(shp$GID_0)) > 1 ){
        shp$GID_0[grepl("[0-9]+", shp$GID_0)] <- unique(shp$GID_0[!grepl("[0-9]+", shp$GID_0)])
      }
      res <- terra::relate(cog_coord, shp, relation = "intersects", pairs = TRUE, na.rm = F)
      shp <- terra::as.data.frame(shp)
      names(shp) <- paste0("GADM_", names(shp))
      pos_shp <- grepl(pattern = 'GADM_GID_0|GADM_COUNTRY|GADM_NAME_[0-9]|GADM_VARNAME_[0-9]',names(shp))
      #to_ret <- base::merge(df, aa, by.x = "shp_id",by.y = "GADM_UID", all.x = T, all.y = F)
      to_ret<- data.frame(id = df[df$id %in% cog_coord$id, "id"], shp[res[,2], pos_shp])
      to_ret <- to_ret[complete.cases(to_ret$GADM_GID_0), ]
      return(to_ret)
    })
    
    res <- res[sapply(res, nrow)>0]
    res <- dplyr::bind_rows(res)
    row.names(res) <- NULL
    
    to_ret <- dplyr::left_join(df, res, by = "id")#base::merge(df, res, by = "id", all.x = T, all.y = F)
    
    
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
  
  occ <- terra::vect(df, c("DECLONGITUDE", "DECLATITUDE"), crs = "EPSG:4326")
  
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


#' Function to count number of different accession per each coordinate
#' @param df (data.frame) genesys data.frame
#' @return vector of same length as nrow(df) with counts

acc_per_coord <- function(df){
  
  temp_df <- dplyr::filter(df, !is.na(DECLATITUDE) & !is.na(DECLONGITUDE)) 
  temp_df <- dplyr::mutate(temp_df, id_latlng = paste0(DECLATITUDE, ";", DECLONGITUDE))
  temp_df <- dplyr::group_by(temp_df, INSTCODE, CROPNAME, id_latlng) 
  temp_df <- dplyr::add_count(temp_df, id_latlng, name = "check_count_acc_per_coord") 
  temp_df <- dplyr::ungroup(temp_df)
  temp_df <- temp_df[, c("id", "check_count_acc_per_coord")]
  
  df <- dplyr::left_join(df, temp_df)
  return(df$check_count_acc_per_coord)
  }


#' Function to match accession check fields to tree
#' @param tree (data.frame) tree of checks field for verification
#' @param df (data.frame) genesys dataframe with verification fields 
#' @return data.frame with the same number of rows as df containing tree paths and Quality score

scoring <- function(tree, tmp_df){
  
  vars_select <- c("check_ORIGCTY",
                   "check_location_available",
                   "check_zero_coords",
                   "check_sea_coast",
                   "check_centroid_cat",
                   "check_acc_count",
                   "check_decimals_cat",
                   "check_wrong_ORIGCTY",
                   "check_bordering_cnt",
                   "check_elev_cat",
                   "check_collsite",
                   "admon_lvl_1_matched",
                   "admon_lvl_2_matched" )
  
  tmp_df <- tmp_df[ , vars_select]
  
  stopifnot("missmatch in names" = all(names(tmp_df) %in% names(tree)))
  tmp_df$SCORE <- NA
  tmp_df$LI <- NA
  tmp_df$ROUTE <- NA
  
  drop_vars <- c( "SCORE","LI"  )
  pos_drop <- grep(paste0(drop_vars, collapse = "|"), names(tree))
  
  all_combs <- apply(tree[, -pos_drop], 1, function(rw){
    paste0(rw[!is.na(rw)], collapse = "")
    })
  
  stopifnot("combs are not unique" = all(table(all_combs) == 1))
  
  df_keys <- data.frame(tree[, drop_vars], all_combs)
  
  
  for(i in 1:nrow(tree)){
    cat("Processing: ", i, "\n")
    vars_names <- names(tree)[!is.na(tree[i, ] )]
    vars_names <- vars_names[-grep(paste0(drop_vars, collapse = "|"), vars_names)]
    
    tmp_df$combi = apply(tmp_df[, vars_names],1, paste0, collapse = "")
    
    tmp_df$SCORE[ tmp_df$combi == df_keys$all_combs[i] & is.na(tmp_df$SCORE) ] <- df_keys$SCORE[i]
    tmp_df$LI[ tmp_df$combi == df_keys$all_combs[i] & is.na(tmp_df$LI)] <- df_keys$LI[i]
    tmp_df$ROUTE[ tmp_df$combi == df_keys$all_combs[i] & is.na(tmp_df$ROUTE) ] <- paste0("R", i)
    
    
  }
  
  
  tmp_df$combi <- NULL
  
  return(tmp_df[, c(drop_vars, "ROUTE")])
  
}


#' Main Function to implement coordinates checking proccess for genesys data V2.0, new 
#' way of compute priority score
#' @param data_pth (character) path to genesys institute data
#' @param shp_pth (character) path/paths to  shapefiles
#' @param elev_pth (character) path to elevetion raster
#' @param centroids_db_path (character) Centroids database full path
#' @param out_dir_pth (character) full output dir path 
#' @param bordering_cnt_pth (character) full path to bordering countries list
#' @return List of dataframe containnig 
checking_process_v2<-function(data_pth, 
                              shp_pth, 
                              elev_pth, 
                              slope_pth, 
                              data_dict_url,
                              centroids_db_path,
                              tree_pth,
                              out_dir_pth,
                              bordering_cnt_pth){
  #data_pth = "C:/Users/acmendez/Downloads/genesys_downloaded_institutions_data (1).csv"
  #shp_pth = list.files("//alliancedfs.alliance.cgiar.org/cimmyt-aidi$/others", recursive = T, full.names = T, pattern  = ".shp$")
  #elev_pth = "C:/Users/acmendez/Downloads/elevation_30s.tif"
  #slope_pth = "C:/Users/acmendez/Downloads/slope_col.tif"
  #roads_root_pth = "//alliancedfs.alliance.cgiar.org/cimmyt-aidi$/OSM_roads"
  #cnt_code_pth = "C:/Users/acmendez/Downloads/country_roads_codes.xlsx"
  #data_dict_url = "https://github.com/alliance-datascience/genebank-general/blob/dev/scripts/r/misc/GCA_data_dictionary.xlsx"
  
  #e <- geodata::elevation_global(0.5, tempdir())
  #terra::writeRaster(e, "C:/Users/acmendez/Downloads/elevation_30s.tif")
  elev_rst       <- terra::rast(elev_pth)
  slope_rst      <- terra::rast(slope_pth)
  shp_paths      <- shp_pth #terra::vect(shp_pth)
  centroid_df    <- readr::read_csv(centroids_db_path)
  bordering_cnt  <- readr::read_csv(bordering_cnt_pth)
  tree_df        <- read.csv(tree_pth)
  tree_df        <- tree_df[, !grepl("\\bROUTE\\b|\\bcat_score\\b", names(tree_df))]
  #data_dic_pth <- tempfile(pattern = ".xlsx")
  #download.file(data_dict_url, data_dic_pth)
  #stopifnot("Download failed for Data dictionary" = file.exists(data_dic_pth))
  data_dic_pth <- data_dict_url
  
  text_description <- readxl::read_excel(data_dic_pth)
  
  
  cat("Reading institute passport data...", "\n")
  
  COMPLETE_data <- read.csv(data_pth)
  COMPLETE_data$id <- 1:nrow(COMPLETE_data)
  #names(data)[which(names(data) %in% c("DECLATITUDE", "DECLONGITUDE"))]<- c("Latitude", "Longitude")
  
  
  cat("Identifying the accessions with location issues...", "\n")
  
  
  #########################################
  ##### check data without collSite #######
  #########################################
  
  
  COMPLETE_data$check_collsite <- TRUE
  COMPLETE_data$check_collsite[is.na(COMPLETE_data$COLLSITE)] <- FALSE
  
  
  ##############################################
  ##### Separate data with no coodinates #######
  ##############################################
  
  COMPLETE_data$check_location_available <- NA
  COMPLETE_data$check_location_available[!complete.cases(COMPLETE_data[,c("DECLATITUDE", "DECLONGITUDE")])] <- FALSE
  COMPLETE_data$check_location_available[complete.cases(COMPLETE_data[,c("DECLATITUDE", "DECLONGITUDE")])] <- TRUE
  COMPLETE_data$check_location_available = as.numeric( COMPLETE_data$check_location_available)
  
  stopifnot("NA's in check_location_available" = !any(is.na(COMPLETE_data$check_location_available)))
  
  #NAS_data <- data[which(data$check_location_available == FALSE),]
  #COMPLETE_data <- data[which(data$check_location_available == TRUE),]
  
  COMPLETE_data <- GADM_extraction(df = COMPLETE_data,
                                   shp_lst_pth = shp_paths,#"//alliancedfs.alliance.cgiar.org/cimmyt-aidi$/others",
                                   shp = NULL)
  
  #################################
  ##### check zero coordinates ####
  #################################
  
  COMPLETE_data$check_zero_coords <- FALSE
  COMPLETE_data$check_zero_coords[which(COMPLETE_data$DECLONGITUDE == 0 | COMPLETE_data$DECLATITUDE == 0)] <- TRUE
  COMPLETE_data$check_zero_coords <- ifelse(COMPLETE_data$check_zero_coords, 0, 1  )
  #################################
  ##### DECIMAL PLACES ###########
  ###############################
  
  COMPLETE_data$check_decimals_lon<- NA
  COMPLETE_data$check_decimals_lat<- NA
  
  COMPLETE_data$check_decimals_lon <- count_decimals(COMPLETE_data$DECLONGITUDE) 
  COMPLETE_data$check_decimals_lat <- count_decimals(COMPLETE_data$DECLATITUDE)
  
  COMPLETE_data$check_decimals_cat = ifelse(COMPLETE_data$check_decimals_lon < 2 | COMPLETE_data$check_decimals_lat < 2, 0, 1)
  
  ##############################
  ##### BORDERING COUNTRIES ###
  #############################
  
  COMPLETE_data$check_wrong_ORIGCTY <-  ((COMPLETE_data$ORIGCTY != COMPLETE_data$GADM_GID_0) & !is.na(COMPLETE_data$GADM_GID_0))
  
  COMPLETE_data$check_bordering_cnt <- COMPLETE_data$check_wrong_ORIGCTY &
    (paste0(COMPLETE_data$ORIGCTY, "-", COMPLETE_data$GADM_GID_0) %in% paste0(bordering_cnt$country_code, "-", bordering_cnt$country_border_code))

  COMPLETE_data$check_wrong_ORIGCTY = ifelse(COMPLETE_data$check_wrong_ORIGCTY, 0, 1)
  COMPLETE_data$check_bordering_cnt = ifelse(COMPLETE_data$check_bordering_cnt, 0, 1)
  
  ########################
  ##### elevation #######
  ######################
  
  
  x <- terra::extract( elev_rst, COMPLETE_data[, c('DECLONGITUDE', 'DECLATITUDE')])
  
  COMPLETE_data$check_elev_suggested <-NA
  COMPLETE_data$check_elev_suggested <- x[,2]
  
  COMPLETE_data$check_elev_diff <- abs(COMPLETE_data$ELEVATION - COMPLETE_data$check_elev_suggested)
  COMPLETE_data$check_elev_cat  <- NA
  COMPLETE_data$check_elev_cat  <- ifelse(COMPLETE_data$check_elev_diff > 150, TRUE,  FALSE)
  COMPLETE_data$check_elev_cat  <- ifelse(COMPLETE_data$check_elev_cat | is.na(COMPLETE_data$check_elev_cat) , 0, 1)
  
  #####################
  #### Slope #########
  ##################
  
  x <- terra::extract( slope_rst, COMPLETE_data[, c('DECLONGITUDE', 'DECLATITUDE')])
  
  COMPLETE_data$check_slope_suggested <-NA
  COMPLETE_data$check_slope_suggested <- x[,2]
  
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
  
  
  ##########################
  ### accession per Coord ##
  #########################
  
  COMPLETE_data$check_acc_count <- acc_per_coord(COMPLETE_data)
  COMPLETE_data$check_acc_count <- ifelse(COMPLETE_data$check_acc_count > 25, 0, 1)
  
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
                          "check_centroid_Gmaps_country",
                          "check_centroid_department",
                          "check_centroid_institution")], 1, any)
  
  COMPLETE_data$check_centroid_cat <- ifelse(COMPLETE_data$check_centroid_cat, 
                                             "Georeferenced potential issue",
                                              ifelse(is.na(COMPLETE_data$check_centroid_cat),
                                                      NA, "Ok"))
  COMPLETE_data$check_centroid_cat = ifelse(COMPLETE_data$check_centroid_cat == "Ok", 1, 0)
  
 
  
  COMPLETE_data$check_sea_coast    = ifelse(is.na(COMPLETE_data$GADM_GID_0) & COMPLETE_data$check_location_available, 0, 1)
  COMPLETE_data$check_ORIGCTY      = ifelse( !is.na(COMPLETE_data$ORIGCTY), 1, 0)
  
  COMPLETE_data$admon_lvl_1_matched = dplyr::case_when(
    !is.na(COMPLETE_data$admon_lvl_1_matched ) & COMPLETE_data$check_location_available == 1 ~ 1,
    is.na(COMPLETE_data$admon_lvl_1_matched ) & COMPLETE_data$check_location_available == 1 ~ 0,
    .default = NA
  )
  COMPLETE_data$admon_lvl_2_matched = dplyr::case_when(
    !is.na(COMPLETE_data$admon_lvl_2_matched ) & COMPLETE_data$check_location_available == 1 ~ 1,
    is.na(COMPLETE_data$admon_lvl_2_matched ) & COMPLETE_data$check_location_available == 1 ~ 0,
    .default = NA
  )
  
  
  COMPLETE_data$check_collsite = ifelse(COMPLETE_data$check_collsite, 1, 0)
  
  
  # ###########################
  ### Final score ##########
  #########################
  
  
  decisions <- scoring(tree = tree_df, tmp_df = COMPLETE_data)
  
  COMPLETE_data <- cbind(COMPLETE_data, decisions)
  
  COMPLETE_data$quality_score = dplyr::case_when(
    COMPLETE_data$SCORE <= 4 ~ "Low",
    COMPLETE_data$SCORE > 4 & COMPLETE_data$SCORE <= 8 ~ "Moderate",
    COMPLETE_data$SCORE > 8 ~ "High",
    .default = NA)
  COMPLETE_data$quality_score = factor(COMPLETE_data$quality_score, levels = c("Low", "Moderate", "High"))
  
  COMPLETE_data$prescription = dplyr::case_when(
    COMPLETE_data$LI == 1 ~ "Hard",
    COMPLETE_data$LI == 2 ~ "Moderate",
    COMPLETE_data$LI == 3 ~ "Easy")
  COMPLETE_data$prescription = factor(COMPLETE_data$prescription, levels = c("Easy", "Moderate", "Hard"))
  
  #writexl::write_xlsx(final_df, here(out_dir_pth, "genesys_quality_score.xlsx"))
  write.csv(COMPLETE_data, here(out_dir_pth, "genesys_quality_score.csv"), row.names = F)
  
  
  return(COMPLETE_data)
  
  
}

system.time({
  final_df <- checking_process_v2(data_pth = here("quality_score_data","genesys_downloaded_institutions_data_07-24.csv"), 
                                  shp_pth = list.files(here("country_shps"), recursive = T, full.names = T, pattern  = ".shp$"), 
                                  elev_pth  = here("misc","elevation_30s.tif"),
                                  slope_pth = here("misc", "slope_30s.tif"),
                                  #roads_root_pth = here("roads_shapefile"),
                                  data_dict_url = here("quality_score_data", "GCA_data_dictionary.xlsx"),
                                  #roads_db_path = here("roads_dist_db", 'acc_roads_dist_db.parquet'),
                                  centroids_db_path = here("centroids_data", "centroid_checks_df.csv"),
                                  tree_pth = here("decision_tree", "decision_tree_v1.csv"),
                                  out_dir_pth = here("score_results"),
                                  bordering_cnt_pth = here("country_borders","country_borders.csv"))
  
})


View(final_df)
