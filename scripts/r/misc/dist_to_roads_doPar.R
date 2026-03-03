df = readRDS("//alliancedfs.alliance.cgiar.org/cimmyt-aidi$/df_test_roads.rds")
roads_root_pth = "//alliancedfs.alliance.cgiar.org/cimmyt-aidi$/OSM_roads/gpkgs/"
options(future.globals.maxSize= 5242880000)


dist_to_roads <- function(df, roads_root_pth){
  
  av_countries <- unique(df$GADM_GID_0)
  df$id_lonlat <- paste0(df$DECLONGITUDE,":",df$DECLATITUDE)
  
  av_gpkg <- list.files(roads_root_pth, full.names = T, pattern = ".gpkg$")
  av_iso <- gsub("_OSM_roads.gpkg", "",basename(av_gpkg))
  
  
  dists_df <- lapply(av_countries, function(iso){
    cat(">>> calculating distances to roads for: ", iso, "\n")
    if(!is.na(iso)){
      
      coords <- base::subset(df, GADM_GID_0 == iso, select =  c("DECLONGITUDE", "DECLATITUDE", "id_lonlat")) 
      coords <- coords[!duplicated(data.frame(coords$DECLONGITUDE, coords$DECLATITUDE)),]
      
      stopifnot("missng values found. " = !any(is.na(coords$DECLONGITUDE)) )
      if(any(grepl(iso, av_iso)) ){
        roads_shp_pth <- av_gpkg[grepl(iso, av_iso)] #use av_countries
        #shp <- lapply(roads_shp_pth, terra::vect)
        
        if(nrow(coords) > 1500){
          
          ncore <- future::availableCores()
          cats  <- sort(sample(seq(1, floor(nrow(coords)/500)), size = nrow(coords), replace = T))#slice df in 500 rows chunks
          
          if(ncore >= (length(unique(cats))+5) ){ncore = length(unique(cats))}else{ncore = ncore-5}
          future::plan(multisession,  workers = ncore)
          

          df_chunked <- base::split(coords, cats)
          
          dst <- furrr::future_map(df_chunked, function(df_c){
            df_c <- base::as.data.frame(df_c)
            shp <- terra::vect(roads_shp_pth)
            
            spatial_points <- terra::vect(df_c,  crs = crs(shp), geom = c("DECLONGITUDE", "DECLATITUDE") )
            
            dists <- terra::nearest(spatial_points, shp)
            to_ret <- round(dists$distance, 0 )
            return(to_ret)
            
            
          })
          
          coords$dist_to_roads <- unlist(dst)
          future::plan(sequential)
          
        
        }else{
          
          shp <- terra::vect(roads_shp_pth)
          spatial_points <- terra::vect(coords,  crs = crs(shp), geom = c("DECLONGITUDE", "DECLATITUDE") )
          
          dists <- terra::nearest(spatial_points, shp)
          
          
          coords$dist_to_roads <- round(dists$distance, 0 )
        }
        
       
        
      }else{
        
        coords$dist_to_roads <- NA
      }
    }else{
      coords <- base::subset(df, is.na(GADM_GID_0), select =  c("DECLONGITUDE", "DECLATITUDE", "id_lonlat")) 
      coords <- coords[!duplicated(data.frame(coords$DECLONGITUDE, coords$DECLATITUDE)),]
      coords$dist_to_roads <- NA
    }
    gc()
    return(coords)
    
  })
  
  
  dists_df <- do.call(rbind, dists_df)
  dists_df <- dists_df[, c("id_lonlat", "dist_to_roads")]
  
  #aa <- base::merge(df, dists_df, by = "id_lonlat", all.x = T, sort = F)
  df <- dplyr::left_join(df, dists_df, by = "id_lonlat")
  
  return(df$dist_to_roads)
}