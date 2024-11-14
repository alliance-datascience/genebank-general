########################################################
# script to download country GADM shapefiles and upload to AWS s3 bucket 
# aunthor: andres camilo mendez
# 2023

require(pacman)
pacman::p_load(Rarr,paws,paws.storage,terra, geodata, rvest)

s3_clt <- paws.storage::s3(
  config = list(
    credentials = list(
      creds = list(access_key_id     = '',
                   secret_access_key = '')
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
  
  } else {
    shp <- terra::vect(x = out_file)

  }
  
  lapply(list.files(out_dir, full.names = T), function(i){
    
    fl_name <- basename(i)
    cat("     >> putting", fl_name, " on bukcet \n" )
    s3_clt$put_object(Bucket = "genebanks",
                      StorageClass = "STANDARD",
                      Key = paste0("zone=landing/source=gadm/subject=shapefiles/institute=all/country=",tolower(iso),"/", fl_name), 
                      Body = i)
    
  })
  
  return(shp)
}

shp_folder = "//alliancedfs.alliance.cgiar.org/cimmyt-aidi$/others"
iso_table <- read.table("https://gist.githubusercontent.com/tadast/8827699/raw/f5cac3d42d16b78348610fc4ec301e9234f82821/countries_codes_and_coordinates.csv", header = T, sep = ",")
iso_table <- data.frame(apply(iso_table, 2, gsub, pattern = " ", replacement = "", simplify = T))


content_main <- rvest::read_html("https://gadm.org/download_country.html")
GADM_iso <- content_main %>%
  rvest::html_elements( xpath = '//*[@id="countrySelect"]/option') %>%
  rvest::html_attr("value") %>% 
  stringr::str_extract(., "^[A-Z]{3}") %>% 
  na.omit()

iso_codes <- unique(c(iso_table$Alpha.3.code, GADM_iso))

shps <- lapply(iso_codes, function(iso3){
  cat(">>> Procesing: ", iso3, "\n")
  tryCatch(expr = {
    shp <- get_iso_shapefile_aws(iso = iso3, path = shp_folder, s3_clt)
  },
  error = function(e){
    return(NA)
  })
  
})




root_dir <- "//alliancedfs.alliance.cgiar.org/cimmyt-aidi$/OSM_roads" 
urls_fl <- list.files("//alliancedfs.alliance.cgiar.org/cimmyt-aidi$/OSM_roads", pattern = ".csv$", full.names = T)

aa =lapply(urls_fl, read.csv, header = T) |> 
  bind_rows() |> 
  tibble::as_tibble() |> 
  dplyr::mutate(name = str_replace(urls, pattern = "-latest-free.shp.zip", replacement = ""),
                c_name = str_extract(name, "/[a-z-]+"),
                c_name = str_replace(c_name, "/", ""),
                subregion_name = ifelse(str_count(name, "/") == 1, name,str_replace(name, "/[a-z-]+", "")),
                folder_path = paste(root_dir, subregion_name ,sep = "/"),
                c_name = str_replace_all(c_name, "-", " "),
                c_name = ifelse(c_name == "us", "united states of america", c_name))|>
  tidyr::drop_na(c_name)


iso3_info = lapply(aa$c_name, function(cnt){
  df = geodata::country_codes(cnt) |> 
    dplyr::mutate(NAME_ISO = tolower(NAME_ISO))
  if(nrow(df) != 0){
    if(cnt == "kosovo"){
      to_ret = df
    }else{
      pos = which.max(stringdist::stringsim(cnt, df$NAME_ISO, method = "qgram"))
      to_ret = df[pos, ]
    }
    
  }else{
    to_ret = df
    to_ret[1, ] <- NA
  }
  
  return(to_ret)
}) |> 
  bind_rows()



final_df <- bind_cols(aa, iso3_info)

writexl::write_xlsx(final_df, "//alliancedfs.alliance.cgiar.org/cimmyt-aidi$/OSM_roads/country_roads_codes.xlsx")

View(final_df)

final_df2 <- readxl::read_excel( "//alliancedfs.alliance.cgiar.org/cimmyt-aidi$/OSM_roads/country_roads_codes.xlsx")


fls <- lapply(final_df2$folder_path, list.files)

all(sapply(fls, length) == 5)


all_drs <- list.dirs(root_dir, recursive = T, full.names =T)

fls <- lapply(all_drs, list.files, pattern = "gis_osm_roads_free")
pos <- which(sapply(fls, length) == 0)

all_drs[pos]

