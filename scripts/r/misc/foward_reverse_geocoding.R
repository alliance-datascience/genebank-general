if(require(pacman)){install.packages("pacman");library(pacman)}else{library(pacman)}
pacman::p_load(stringr, tidyverse, terra, vroom, httr, jsonlite)



lowest_gadm <- function(iso = 'KEN', out = NULL){
  
  suppressMessages(library(raster))
  suppressMessages(library(geodata))
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

get_iso_shapefile <- function(iso, path){
  
  out_dir <-  paste0(path, "/", iso)
  out_file <- paste0(out_dir, "/", iso,".shp")
  if(!file.exists(out_file)){
    cat(iso, " country shapefile does not exists, downloading it... \n")
    dir.create(path = out_dir,  recursive = TRUE)
    shp <- lowest_gadm(iso = iso, out = out_file)
    #adm <- grep(pattern = '^NAME_', x = names(shp), value = T)
    #shp@data$key <- tolower(do.call(paste, c(shp@data[,adm], sep="-")))
    #shp <- as(shp, 'SpatVector')
  } else {
    shp <- raster::shapefile(x = out_file)
    #adm <- grep(pattern = '^NAME_', x = names(shp), value = T)
    #shp@data$key <- tolower(do.call(paste, c(shp@data[,adm], sep="-")))
    #shp <- as(shp, 'SpatVector')
  }
  return(shp)
}



#' Function to extract information of GDAM country administrative levels based on coordinate
#' @param iso3 (character) Three letter country iso code
#' @param lat (numeric) decimal latitude
#' @param lng (numeric) decimal longitude
#' @param out_shp (character) path to shapefile folder
#' @return Data.frame with GDAM data extracted for coordinate
GDAM_extraction <- function(iso3 = NULL, 
                            lat = NULL, 
                            lng = NULL, 
                            shp_folder = NULL){
  
  stopifnot("shp_folder is null" = !is.null(shp_folder))
  stopifnot("iso3 is null" = !is.null(address_text))
  stopifnot("latitude is null" = !is.null(lat))
  stopifnot("longitude is null" = !is.null(lng))
  
  shp <- get_iso_shapefile(iso = iso3, path = shp_folder)
  
  cog_coord <- matrix(c(lng, lat), ncol = 2) %>% 
    terra::vect(., type = 'points', crs = terra::crs(shp))
  
  #plot(shp);points(cog_coord, col = "red")
  
  res <- terra::intersect(cog_coord, shp)
  res <- as.data.frame(res)
  
  if(nrow(res) == 0){
    res <- NA
  }
  
 return(res) 
}



GDAM_extraction(iso3 = "IND", lat = , lng = , shp_folder = "//alliancedfs.alliance.cgiar.org/cimmyt-aidi$/others")

###############################
### ISO conversion table #####
#############################

iso_table <- read.table("https://gist.githubusercontent.com/tadast/8827699/raw/f5cac3d42d16b78348610fc4ec301e9234f82821/countries_codes_and_coordinates.csv", header = T, sep = ",")
iso_table <- data.frame(apply(iso_table, 2, gsub, pattern = " ", replacement = "", simplify = T))


#################################################
#reverse geocoding (coordinates to address)#####
###############################################

gg_key <- "AIzaSyA4Zh8ejw3EsE0GEJmQwEJ6PTB7olsYl0k"


#' Function to call google maps geocoding api for foward and reverse geocoding
#' @param address_text (character) address or place name to be geocoded
#' @param lat (numeric) decimal latitude for reverse geocoding
#' @param lng (numeric) decimal longitude for reverse geocoding
#' @param iso2 (character) tow letter ISO 3166-1 country code
#' @param geocode_type one of forward for address geocoding or 'reverse' for reverse geocoding
#' @param language (character) language in which results must be returned
#' @return Data.frame with geocoded data returned by the google maps geocoding API
geocode <- function(address_text = NULL, 
                    lat = NULL, 
                    lng = NULL, 
                    iso2 = NULL, 
                    geocode_type = c('foward', "reverse"), 
                    language = "en"){
  
  stopifnot("geocode_type must be of length 1" = length(geocode_type) == 1)
  stopifnot("geocode_type must be one of foward or reverse" = geocode_type %in% c('foward', "reverse"))
  
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


iso3 <- "TZA"
address_text <- "EASTERN USAMBARA  - Bumbani - Kisiwani"
lat <- -5.3436
lng <- 38.8392
	

#foward geocoding
res_foward <- geocode(address_text = address_text,
                      lat = NULL,
                      lng = NULL,
                      iso2 =  iso_table$Alpha.2.code[iso_table$Alpha.3.code == iso3],
                      geocode_type = "foward",
                      language = "en")


#REVERSE geocoding
res_reverse <- geocode(address_text = NULL,
                       lat = lat,
                       lng = lng,
                       iso2 = NULL,
                       geocode_type = "reverse",
                       language = "en")


raw_db <- read.csv("C:/Users/acmendez/Downloads/genesys-accessions-BEL084.csv")

#columans importantes
# ORIGCTY COLLSITE DECLATITUDE DECLONGITUDE COORDUNCERT GEOREFMETH 

stopifnot("ACCENUM are not unique" = !any(duplicated(raw_db$ACCENUMB)))

head(raw_db)



all(is.na(raw_db$GEOREFMETH ))

