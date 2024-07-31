pacman::p_load(terra, sf, tidyverse, stringr, paws)



s3_clt <- paws.storage::s3(
  config = list(
    credentials = list(
      creds = list(access_key_id     = 'AKIATHPVGK3ZVLVSHEEF',
                   secret_access_key = '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')
    ),
    region = 'us-east-2'
  )
)

#'Function to pre-process OSM roads shapefiles
#'@param pth (character) Country shapefile folder path
#'@return spatVector
clean_roads <- function(pth){
  
  roads_shp_pth <- list.files(pth, full.names = T, pattern = ".shp$")
  shp <- shp <- terra::vect(roads_shp_pth)
  shp <- shp[!shp$fclass %in% c("residential", "service", "footway", "cycleway", "tertiary_link", "path", "pedestrian", "steps"), ]
  
  
  return(shp)
  #writeVector(shp,  "//alliancedfs.alliance.cgiar.org/cimmyt-aidi$/OSM_roads/south-america/colombia/gis_osm_roads_free_1.gpkg")
  
}


root_dir <- "//alliancedfs.alliance.cgiar.org/cimmyt-aidi$/OSM_roads" 
out_dir  <- "//alliancedfs.alliance.cgiar.org/cimmyt-aidi$/OSM_roads/gpkgs" 

roads_codes <- readxl::read_excel(paste(root_dir, "country_roads_codes.xlsx", sep = .Platform$file.sep))


fls <- roads_codes |> 
  dplyr::select(folder_path, ISO3)

iso3 <- unique(fls$ISO3)



lapply(iso3, function(iso){
  
  cat(">>>>Processing iso: ", iso, "\n")
  
  file_out_name <- paste0(iso, "_OSM_roads.gpkg")
  file_out_path <- paste(out_dir, file_out_name, sep = .Platform$file.sep)
  
  if(!file.exists(file_out_path)){
    
    pths <- fls |> 
      dplyr::filter(ISO3 == iso) |> 
      dplyr::pull(folder_path)
    
    if(length(pths) > 1){
      shp <- lapply(pths, function(k){
        cat(">>processing: ", k, "\n")
        to_ret <- clean_roads(k)
        return(to_ret)
      })
      shp = terra::vect(shp)
      
      
    }else{
      shp <- clean_roads(pths)
    }
    
    
    terra::writeVector(shp, file_out_path)
    
    cat(">Loading file to S3 bucket \n")
    s3_clt$put_object(Bucket = "genebanks",
                      StorageClass = "STANDARD",
                      Key = paste0("zone=landing/source=OSM_roads/country=", iso,"/", file_out_name), 
                      Body = file_out_path)
    
    
    
  }else{
    cat(">>file already exists. \n")
  }
  
  return("ok")
})








  







