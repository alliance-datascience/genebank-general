########################################################
# script to download country GADM shapefiles and upload to AWS s3 bucket 
# aunthor: andres camilo mendez
# 2023

require(pacman)
pacman::p_load(Rarr,paws,paws.storage,terra, godata)

s3_clt <- paws.storage::s3(
  config = list(
    credentials = list(
      creds = list(access_key_id     = 'AKIATHPVGK3ZVLVSHEEF',
                   secret_access_key = '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')
    ),
    region = 'us-east-2'
  )
)

lowest_gadm <- function(iso = 'KEN', out = NULL){
  
  levels <- 5:1
  for(i in 1:length(levels)){
    tryCatch(expr = {
      shp <- geodata::gadm(country = iso, level = levels[i], path = tempdir(), resolution = 1)
      terra::writeVector(shp, out)
      break
    },
    error = function(e){
      cat(paste0("Getting GADM level ",levels[i]," failed... Trying a higher level\n"))
      return("\n")
    })
  }
  
  return(shp)
}

get_iso_shapefile_aws <- function(iso, path, s3_clt){
  
  out_dir <-  paste0(path, "/", iso)
  out_file <- paste0(out_dir, "/", iso,".shp")
  if(!file.exists(out_file)){
    cat(iso, " country shapefile does not exists, downloading it... \n")
    dir.create(path = out_dir,  recursive = TRUE)
    shp <- lowest_gadm(iso = iso, out = out_file)
    stopifnot("Country shp not available" = !is.null(shp) )
    #adm <- grep(pattern = '^NAME_', x = names(shp), value = T)
    #shp@data$key <- tolower(do.call(paste, c(shp@data[,adm], sep="-")))
    #shp <- as(shp, 'SpatVector')
  } else {
    shp <- raster::shapefile(x = out_file)
    #adm <- grep(pattern = '^NAME_', x = names(shp), value = T)
    #shp@data$key <- tolower(do.call(paste, c(shp@data[,adm], sep="-")))
    #shp <- as(shp, 'SpatVector')
  }
  
  lapply(list.files(out_dir, full.names = T), function(i){
    
    fl_name <- basename(i)
    cat("     >> putting", fl_name, " on bukcet \n" )
    s3_clt$put_object(Bucket = "genebanks",
                      StorageClass = "STANDARD",
                      Key = paste0("zone=landing/shapefiles/country=",tolower(iso),"/", fl_name), 
                      Body = i)
    
  })
  
  return(shp)
}

shp_folder = "//alliancedfs.alliance.cgiar.org/cimmyt-aidi$/others"
iso_table <- read.table("https://gist.githubusercontent.com/tadast/8827699/raw/f5cac3d42d16b78348610fc4ec301e9234f82821/countries_codes_and_coordinates.csv", header = T, sep = ",")
iso_table <- data.frame(apply(iso_table, 2, gsub, pattern = " ", replacement = "", simplify = T))

shps <- lapply(iso_table$Alpha.3.code, function(iso3){
  cat(">>> Procesing: ", iso3, "\n")
  tryCatch(expr = {
    shp <- get_iso_shapefile_aws(iso = iso3, path = shp_folder, s3_clt)
  },
  error = function(e){
    return(NA)
  })
  
})




