######################################
## Author: Andres Mendez
## date: Sep-2023

if(require(pacman)){install.packages("pacman");library(pacman)}else{library(pacman)}
pacman::p_load(stringr, stringi, tidyverse, terra, vroom, httr, jsonlite, stringr, cld3)

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
  stopifnot("error in terra::relate" = ncol(res)==2)
  df$shp_id <- res[,2]
  rm(res)
  names(shp) <- paste0("GADM_", names(shp))
  to_ret <- merge(df, shp, by.x = "shp_id",by.y = "GADM_UID", all.x = T, all.y = F)
  
  if(any(is.na(to_ret$shp_id))){
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
        vec <- vec[nchar(vec) != 0]
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
  
  GADM_df$GADM_quality_score_v1 <- apply(matchs, 1, function(vec){
    to_ret <- paste0(na.omit(vec), collapse =",")
    to_ret <- ifelse(nchar(to_ret) ==0, NA, to_ret)
  })
  
  GADM_df$GADM_max_admonlv <- rowSums(GADM_df[, grepl("GADM_NAME_[1-4]", names(GADM_df))] != "")
  
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
  
    
  
  GADM_df$GADM_quality_score_v2 <- GADM_df$GADM_admonlv_score/GADM_df$GADM_max_admonlv_score
  
 return(GADM_df)
  
}

######################################################
######### 1. USANDO GADM SHAPEFILE    ###############
####################################################

raw_df <- vroom::vroom("C:/Users/acmendez/Downloads/genesys-accessions-COL003.csv")
raw_df$id <- 1:nrow(raw_df)
output_df <- raw_df[!is.na(raw_df$DECLATITUDE) | !is.na(raw_df$DECLONGITUDE), ]
output_df <- output_df[!is.na(output_df$COLLSITE),]

shp_wrld <- terra::vect("C:/Users/acmendez/Downloads/gadm_410.gpkg")

output_df <- GADM_extraction(df = output_df,
                               shp_dir = NULL,#"//alliancedfs.alliance.cgiar.org/cimmyt-aidi$/others",
                               shp = shp_wrld)

output_df$std_collsite <- string_std(output_df$COLLSITE)

output_df <- GADM_quality_score(GADM_df = output_df)

################################################
### Propuesta 1 de categorias de calidad ######
##############################################

output_df$GADM_score_cats <- case_when(
  output_df$GADM_quality_score_v2 < 0.333 ~ "Low",
  output_df$GADM_quality_score_v2 >= 0.333 & output_df$GADM_quality_score_v2 < 0.6 ~ "Moderate",
  #output_df$GADM_quality_score_v2 > 0.5 & output_df$GADM_quality_score_v2 <= 0.8 ~ "Moderate-High",
  output_df$GADM_quality_score_v2 >= 0.6 ~ "High" 
)

#vroom::vroom_write(output_df, "C:/Users/acmendez/Downloads/gensys_CIAT.csv")
#output_df <- vroom::vroom("C:/Users/acmendez/Downloads/gensys_CIAT.csv")

output_df[output_df$GADM_quality_score_v2 > 0.8, grepl("std_|quality|ORIGCTY|cats", names(output_df)) ] %>% View
  dplyr::group_by(ORIGCTY, GDAM_score_cats) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  View

length(unique(output_df$ORIGCTY))

#########################################
#### Propuestas de gráficos ############
#######################################
#graficos pastel para GDAM_score_cats
p1 <- output_df %>% 
  group_by(GADM_score_cats) %>% 
  dplyr::tally() %>%
  drop_na() %>% 
  dplyr::rename(total= n, lab = GADM_score_cats ) %>% 
  dplyr::mutate(lab = factor(lab, levels = c("High", "Moderate-High", "Moderate", "Low")),
                Freq = total/sum(total)*100,
                ypos = cumsum(Freq)- 0.5*(Freq) ,
                txt  = paste0(round(Freq, 0), "%")) %>% 
  ggplot(aes(x = "", y = Freq, fill = lab, label = txt))+
  geom_bar(position = "stack", stat = "identity", color = "black")+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")+
  theme_void()+
  labs(fill = "Quality")+
  scale_fill_brewer(palette="Set1")

x11();p1


# paises mas importantes 
output_df %>%  
  dplyr::filter(ORIGCTY %in% c("PER", "COL", "BRA", "MEX", "VEN", "ECU")) %>%
  dplyr::mutate(GADM_score_cats = factor(GADM_score_cats, levels = c("High", "Moderate-High", "Moderate", "Low"))) %>% 
  dplyr::group_by(ORIGCTY, GADM_score_cats) %>% 
  dplyr::tally() %>% 
  tidyr::drop_na() %>% 
  mutate(freq = n / sum(n),
         freq = round(freq,4)*100) %>% 
  dplyr::ungroup() %>% 
  mutate(label_ypos= cumsum(freq/100) - 0.5*freq/100) %>% 
  ggplot(aes(x = ORIGCTY, y = freq, fill = GADM_score_cats))+
  geom_bar(stat = "identity")+
  labs(fill = "Quality")
  #geom_text(aes( label=freq), vjust=-1, color="white", size=3.5)
  

require(leaflet)
require(htmltools)
c_shp <- geodata::gadm("COL", level = 1, path = tempfile())
to_plot <- output_df %>% 
  #dplyr::filter(ORIGCTY == "COL") %>% 
  dplyr::distinct(DECLATITUDE, DECLONGITUDE, .keep_all = T) %>% 
  dplyr::mutate(col = case_when(
    GADM_score_cats == "High" ~ "#68da3e",
    GADM_score_cats == "Moderate" ~ "#ff9800",
    GADM_score_cats == "Low" ~ "#f50400"
  ),
  label = paste0("Quality: ", GADM_score_cats, 
                 "<br> admon level:", GADM_quality_score_v1,
                 "<br> Collsite: ", std_collsite,
                 "<br> NAME_1:", gsub("\\\\b", "",std_NAME_1),
                 "<br> NAME_2:", gsub("\\\\b", "",std_NAME_2),
                 "<br> NAME_3:", gsub("\\\\b", "",std_NAME_3),
                 "<br> NAME_4:", gsub("\\\\b", "",std_NAME_4),
                 "<br> coordinate:", DECLATITUDE, "; ", DECLONGITUDE),
  label = purrr::map(label, HTML))
  
leaflet() %>% 
  addTiles() %>% 
  # addPolygons(data = c_shp,
  #             weight = 1,
  #             fill = F) %>% 
  addCircleMarkers(
                   lng = to_plot$DECLONGITUDE,
                   lat = to_plot$DECLATITUDE,
                   radius = 4,
                   weight = 1,
                   fillColor = to_plot$col,
                   label = to_plot$label,
                   stroke = F,
                   fillOpacity = 1
                   )


###############################
### ISO conversion table #####
#############################

iso_table <- read.table("https://gist.githubusercontent.com/tadast/8827699/raw/f5cac3d42d16b78348610fc4ec301e9234f82821/countries_codes_and_coordinates.csv", header = T, sep = ",")
iso_table <- data.frame(apply(iso_table, 2, gsub, pattern = " ", replacement = "", simplify = T))

#columans importantes
# ORIGCTY COLLSITE DECLATITUDE DECLONGITUDE COORDUNCERT GEOREFMETH 
iso3 <- "COL"
address_text <- "Chau Thanh"
lat <- 10.0833
lng <- 106.0666

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
                       language = "en",
                       gg_key = gg_key)

#####################
# variables names coding
# GADM_ GADM world shapefile information
# std_ standardized text



