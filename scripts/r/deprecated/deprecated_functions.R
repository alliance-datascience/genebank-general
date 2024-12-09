
#' internal function to compute geodesic distance from points to nearest roads
#' @param iso (character) accession roads distance database full path
#' @param df (data.frame) Lon, Lat data.frame
#' @param av_iso (character) character vector of avilable iso3 country code
#' @return data.frame with geodesic distance from roads to neares accession
calc_road_dist <- function(iso, df, av_iso){
  cat(">>> calculating distances to roads for: ", iso, "\n")
  if(!is.na(iso)){
    
    coords <- base::subset(df, GADM_GID_0 == iso, select =  c("DECLONGITUDE", "DECLATITUDE", "id_lonlat")) 
    coords <- coords[!duplicated(data.frame(coords$DECLONGITUDE, coords$DECLATITUDE)),]
    
    stopifnot("missng values found. " = !any(is.na(coords$DECLONGITUDE)) )
    if(any(grepl(iso, av_iso)) ){
      roads_shp_pth <- av_gpkg[grepl(iso, av_iso)] #use av_countries
      
      #shp <- lapply(roads_shp_pth, terra::vect)
      shp <- terra::vect(roads_shp_pth)
      
      
      spatial_points <- terra::vect(coords,  crs = terra::crs(shp), geom = c("DECLONGITUDE", "DECLATITUDE") )
      
      dists <- terra::nearest(spatial_points, shp)
      
      
      coords$dist_to_roads <- round(dists$distance, 0 )
      
    }else{
      
      coords$dist_to_roads <- NA
    }
  }else{
    coords <- base::subset(df, is.na(GADM_GID_0), select =  c("DECLONGITUDE", "DECLATITUDE", "id_lonlat")) 
    coords <- coords[!duplicated(data.frame(coords$DECLONGITUDE, coords$DECLATITUDE)),]
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
  df$id_lonlat <- paste0(df$DECLONGITUDE,":",df$DECLATITUDE)
  
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




#' Function to copy world Roads shapefiles files from S3 bucket to EC2 R-studio session
#' @param s3_clt (object):S3 client from paws.strograge call
#' @param out_dir_name (character): out directory name
#' @return NULL
copy_s3_roads_shp <- function( s3_clt, out_dir_name){
  
  
  out_dir <- here::here( out_dir_name)
  if(!dir.exists(out_dir)){dir.create(out_dir, recursive = T)}
  
  nms_fls <- s3_clt$list_objects(
    Bucket  = "genebanks",
    Prefix  = "zone=landing/source=OSM_roads/"
  )
  
  nms <- sapply(nms_fls$Contents, function(lst)lst$Key)
  nms <- nms[grepl(".gpkg$", nms)]
  #nms <- stringr::str_extract(nms, "country=[a-zA-Z]{3}")
  #nms <- stringr::str_replace(nms, "country=", "")
  #nms <- unique(nms)
  #nms <- na.omit(nms)
  
  
  for(c_nm in nms){
    
    cat("> Downloading data for: ", c_nm, "\n")
    
    
    b_nm_ext <- basename(c_nm)
    #b_nm <- gsub(".gpkg$", "", b_nm_ext)
    
    out_file <- here::here(out_dir, b_nm_ext)
    if(!file.exists(out_file)){
      
      s3_clt$download_file(
        Bucket = "genebanks",
        Key = c_nm,
        Filename = out_file
      )
      
    }else{
      cat(">>> file aready exists. \n")
    }
    
    
    
    
  }
  
  return(NULL)
}



#' Function to copy roads_dist_db from S3 bucket to EC2 R-studio session
#' @param s3_clt (object):S3 client from paws.strograge call
#' @param out_dir_name (character): out directory name
#' @return NULL
copy_s3_roads_dist_db_files <- function( s3_clt, out_dir_name){
  
  
  out_dir <- here::here( out_dir_name)
  
  nms_fls <- s3_clt$list_objects(
    Bucket  = "genebanks",
    Prefix  = "zone=landing/source=roads_dist_db/"
  )
  
  nms <- sapply(nms_fls$Contents, function(lst)lst$Key)
  nms <- nms[grepl(".parquet$", nms)]
  
  if(!dir.exists(out_dir)){dir.create(out_dir, recursive = T)}
  
  for(c_nm in nms){
    
    cat("> Downloading data for: ", c_nm, "\n")
    b_nm_ext <- basename(c_nm)
    
    out_file <- here::here(out_dir, b_nm_ext)
    
    if(!file.exists(out_file)){
      s3_clt$download_file(
        Bucket = "genebanks",
        Key = c_nm,
        Filename = out_file
      )
    }else{
      cat(">>> File already exists. \n")
    }
    
    
    
  }
  
  
  return(NULL)
  
}



############################
#### DISTANCE TO ROADS ####
##########################


# COMPLETE_data$check_dist_to_roads <- dist_to_roads(
#   df = COMPLETE_data[, c("DECLONGITUDE", "DECLATITUDE", "GADM_GID_0")] ,
#   roads_root_pth = roads_root_pth,
#   roads_db_path  = roads_db_path, 
#   doPar = F)



var_to_use <- c("check_collsite", "check_elev_cat", "GADM_score_cats")

COMPLETE_data$final_score_raw <- (
  as.numeric(COMPLETE_data$check_collsite) +
    as.numeric(!ifelse(is.na(COMPLETE_data$check_elev_cat), TRUE, COMPLETE_data$check_elev_cat))+
    #as.numeric(COMPLETE_data$check_dist_to_roads > 700 & !is.na(COMPLETE_data$check_dist_to_roads))+
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

var_df = data.frame(check_collsite = as.numeric(COMPLETE_data$check_collsite) ,
                    check_elev_cat = as.numeric(!ifelse(is.na(COMPLETE_data$check_elev_cat), TRUE, COMPLETE_data$check_elev_cat)), 
                    #check_dist_to_roads  = as.numeric(COMPLETE_data$check_dist_to_roads < 700 & !is.na(COMPLETE_data$check_dist_to_roads)),
                    GADM_score_cats = as.numeric(COMPLETE_data$GADM_score_cats != "High"),
                    check_country_match_sea = as.numeric(COMPLETE_data$check_country_match != "SEA")
)
var_df$GADM_score_cats = ifelse(var_df$check_collsite == 0, 1, var_df$GADM_score_cats )

COMPLETE_data$issue_txt_desc <- get_text_desc(var_df, text_description)


#COMPLETE_data$complementary_cat <-  ifelse(COMPLETE_data$check_is_donated | COMPLETE_data$check_market_acc, "Collected in market/Donated", )

nms_orig <- setdiff(names(COMPLETE_data), names(NAS_data))
tmp_mtx <- matrix(NA, nrow = nrow(NAS_data), ncol = length(nms_orig))
tmp_mtx <- as.data.frame(tmp_mtx)
names(tmp_mtx) <- nms_orig

tmp_mtx$issue_txt_desc <- "Issues found: Missing DECLATITUDE or DECLONGITUDE"
tmp_mtx$final_score_raw <- 0
tmp_mtx$final_score_cats <- "Low"
tmp_mtx$check_country_match <- 0


###########################################################
## WHICH ACESSION HAS COLLSITE OR ORIGCTY  in NA'S DATA ##
#########################################################
#to do here



# until here
NAS_data <- cbind(NAS_data, tmp_mtx)

COMPLETE_data <- rbind(COMPLETE_data, NAS_data)

COMPLETE_data <- COMPLETE_data[order(COMPLETE_data$id),]

cat("Returning data", "\n")