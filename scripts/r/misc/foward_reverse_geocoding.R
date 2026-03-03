######################################
## Author: Andres Mendez
## date: Sep-2023

if(require(pacman)){install.packages("pacman");library(pacman)}else{library(pacman)}
pacman::p_load(stringi, tidyverse, terra, vroom, httr, jsonlite, stringr)

##########################
### FUNCTIONS ###########
########################

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
  json_fl <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
  if(json_fl$status == "OK"){
    raw_info <- json_fl$results
    
  }else if(json_fl$status == "OVER_QUERY_LIMIT"){
    raw_info <- "OVER_QUERY_LIMIT"
  }else{
    warning(json_fl$status)
    raw_info <- NA
  }
  
  return(raw_info)
  
}


data_pth <- "C:/Users/acmendez/Downloads/genesys_downloaded_institutions_data (1).csv"



  data <- read.csv(data_pth)
  data <- data[complete.cases(data$DECLATITUDE, data$DECLONGITUDE),]
  data$id_latlng <- paste0(as.character(data$DECLATITUDE), ":",as.character(data$DECLONGITUDE))
  data <- data[!duplicated(data$id_latlng), ]
  
  #nrow(data) = 89.658 (5.00 USD per 1000 accession)  ==> total cost of 450 USD
  
  
  data <- data[, c("id_latlng", "DECLATITUDE", "DECLONGITUDE")]
  
  geocode_info <- list()
  
  
  for(i in 1:length(data$id_latlng)){
    id = data$id_latlng[i]
    cat("Processing id: ", id, " position: ", i, "/", length(data$id_latlng),  "\n")
    
    to_geocode <- data[data$id_latlng == id, ]
    while(TRUE){
      
      if(nrow(to_geocode) == 1){
        #API call
        res_1 <- geocode(
          lat = to_geocode$DECLATITUDE,
          lng =  to_geocode$DECLONGITUDE ,
          geocode_type = "reverse",
          gg_key = gg_key 
        )
        
        
      }else{
        
        warnings("Duplicated id")
        to_geocode <- to_geocode[1, ]
        
        res_1 <- geocode(
          lat = to_geocode$DECLATITUDE,
          lng =  to_geocode$DECLONGITUDE ,
          geocode_type = "reverse",
          gg_key = gg_key 
        )
      }
      
      
      if(class(res_1) == "character"){
        cat("OVER_QUERY_LIMIT reached \n")
        delay <- 60+round(runif(1,10,30),0)
        geocode_info[[i]] <- data.frame(message = "OVER_QUERY_LIMIT", id = id)
        Sys.sleep(delay)
        
      }else if(class(res_1) == "logical"){
        cat("Can't retrieve data from coordinate \n")
        geocode_info[[i]] <- data.frame(message = "NA", id = id)
        break
      }else{
        geocode_info[[i]] <- data.frame(res_1, id = id)
        break
      }
    }
    
    delay <- runif(1,0,1)
    if(i %% 1000 == 0){
      saveRDS(geocode_info, "/home/ruser/geocoded_data/geocode_info.rds")
      
    }
   
    Sys.sleep(delay)
     
  }#end for
  
 

##########################################
####3 USANDO REVERSE GEOCODING  #########
########################################


      38.8392
res_1 <- geocode(
  lat = -5.34360,
  lng =  38.8392 ,
  geocode_type = "reverse",
  gg_key = gg_key 
)

res_1 %>% View


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

output_df$GADM_score_cats <- dplyr::case_when(
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

#colores rojo el low
p1 <- output_df %>% 
  group_by(GADM_score_cats) %>% 
  dplyr::tally() %>%
  drop_na() %>% 
  dplyr::rename(total= n, lab = GADM_score_cats ) %>% 
  dplyr::mutate(lab = factor(lab, levels = c("High", "Moderate", "Low")),
                Freq = total/sum(total)*100,
                ypos = cumsum(Freq)- 0.5*(Freq) ,
                txt  = paste0(round(Freq, 0), "%")) %>% 
  ggplot(aes(x = "", y = Freq, fill = lab, label = txt))+
  geom_bar(position = "stack", stat = "identity", color = "black")+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")+
  theme_void()+
  labs(fill = "Quality")+
  scale_fill_manual(values = c("Low" = "#ff0000", "Moderate" = "#f6a700", "High" = "#009929"))

x11();p1

ggsave(plot = p1, 
       filename = "C:/Users/acmendez/Downloads/pieplot1.png", 
       dpi = 300, 
       width = 6, 
       height = 6, 
       units = "in")

# paises mas importantes 
p2 <- output_df %>%  
  dplyr::filter(ORIGCTY %in% c("PER", "COL", "BRA", "MEX", "VEN", "ECU")) %>%
  dplyr::mutate(GADM_score_cats = factor(GADM_score_cats, levels = c("High", "Moderate", "Low"))) %>% 
  dplyr::group_by(ORIGCTY, GADM_score_cats) %>% 
  dplyr::tally() %>% 
  tidyr::drop_na() %>% 
  mutate(freq = n / sum(n),
         freq = round(freq,4)*100) %>% 
  dplyr::ungroup() %>% 
  mutate(label_ypos= cumsum(freq/100) - 0.5*freq/100) %>% 
  ggplot(aes(x = ORIGCTY, y = freq, fill = GADM_score_cats))+
  geom_bar(stat = "identity")+
  labs(fill = "Quality")+
  scale_fill_manual(values = c("Low" = "#ff0000", "Moderate" = "#f6a700", "High" = "#009929"))+
  geom_text(aes(label = freq), size = 3, hjust = 0.5, vjust = 3, position = "stack")+
  theme_bw()

ggsave(plot = p2, 
       filename = "C:/Users/acmendez/Downloads/pieplot2.png", 
       dpi = 300, 
       width = 6, 
       height = 6, 
       units = "in")

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


#single_w <- stringr::str_replace_all(GADM_df[, paste0("std_NAME_", lvl)], "\\\\b", "")

aa <- Map(function(x,y){
  
  to_compare <- unlist(stringr::str_split(y, "\\|"))
  x_split   <- unlist(str_split(x, " "))
  st_dist <- as.vector(sapply(x_split, stringdist::stringdist, b  = to_compare, method = "lv") )
  
  
  if( all(is.na(st_dist)) ){
    to_ret <- NA
  }else if(all(st_dist > 1)){
    to_ret <- FALSE
  }else{
    to_ret <- TRUE
  }
  
  return(to_ret)
  
}, GADM_df$std_collsite[sq], single_w[sq]) %>% unlist
