pacman::p_load(tidyverse, terra)


root <- "C:/Users/acmendez/Downloads/wc2.1_2.5m_wind"

msk <- terra::rast("D:/gap_analysis_shinyapps/www/masks/mask_world.tif")


av_fls <- list.files(root, full.names = T, pattern = ".tif$")

stk <- terra::rast(av_fls)
stk <- terra::crop(stk, ext(msk))
stk <- terra::mask(stk, msk)
stk <- terra::mean(stk)
stk <- terra::resample(stk, msk)

names(stk)

msk == stk

writeRaster(stk, "C:/Users/acmendez/Downloads/new_rasters/wind_speed.tif" )


#### envirem

#root <- "C:/Users/acmendez/Downloads/global_current_2.5arcmin_geotiff"
root <- 'D:/OneDrive - CGIAR/Bolder Africa - Gap analysis/data/runs/input_data/generic_rasters/climate/wc2.1_2.5m'
msk <- terra::rast("D:/gap_analysis_shinyapps/www/masks/mask_world.tif")


av_fls <- list.files(root, full.names = T, pattern = ".tif$")

lapply(av_fls, function(fl){
  cat("processing: ", fl, "\n")
  out_name = stringr::str_replace(string = basename(fl),
                                  pattern = "current_2-5arcmin_|wc2.1_2.5m_", 
                                  replacement = "")
  stk <- terra::rast(fl)
  stk <- terra::project(stk, "epsg:4326")
  stk <- terra::crop(stk, ext(msk))
  stk <- terra::mask(stk, msk)
  stk <- terra::resample(stk, msk)
  
  writeRaster(stk, file.path("C:/Users/acmendez/Downloads/new_rasters", out_name) )
  
  
  
})


msk == stk

writeRaster(stk, "C:/Users/acmendez/Downloads/new_rasters/wind_speed.tif" )


aa = geodata::gadm("COL", level  = 2, path = tempfile())
x11();plot(aa)


pacman::p_load(tidyverse, readxl)

root <- "D:/OneDrive - CGIAR/Bolder Africa - Gap analysis/data/genebanks_data"


############### GHANA
gnbk <- "ghana"
fl <- file.path(root, gnbk, "ghana_genbank_cleaned.xlsx")

out <- file.path(root, gnbk, "ghana_genbank_GQS.csv")

db_raw <- readxl::read_excel(fl)


 db_raw %>% 
  dplyr::select(crop_name = `Name of taxon`,
                INSTCODE = `Holding institute (1)`,
                ACCENUMB = `Accession number`,
                ORIGCTY  = `Country of origin`,
                DECLATITUDE = Lat_clean,
                DECLONGITUDE = Lon_clean,
                coord_comment) %>% 
  dplyr::mutate(ORIGCTY = case_when(
    ORIGCTY == "Ghana" ~ "GHA",
    ORIGCTY == "Tengeru, Tanzania" ~ "TZA",
    ORIGCTY == "GHA" ~ "GHA",
    .default = NA
  )) %>% 
  dplyr::filter(!is.na(DECLATITUDE)  ) %>%
  dplyr::filter(!is.na(DECLONGITUDE)) %>% 
  dplyr::mutate(COLLSITE = NA,
                ELEVATION = NA) %>% 
  dplyr::select(crop_name,
                ORIGCTY,
                ELEVATION,
                INSTCODE,
                DECLATITUDE,
                DECLONGITUDE,
                ACCENUMB,
                COLLSITE,
                coord_comment) %>% 
  dplyr::mutate(crop_name = tolower(crop_name),
                crop_name = stringr::str_replace_all(crop_name, " ", "_")) %>% 
  write.csv(., out, row.names = F) 


############ TANZANIA

gnbk <- "tanzania"
fl <- file.path(root, gnbk, "tanzania_genbank_cleaned.xlsx")

out <- file.path(root, gnbk, "tanzania_genbank_GQS.csv")

db_raw <- readxl::read_excel(fl)

names(db_raw)

db_raw %>% 
  dplyr::mutate(COLLSITE = paste(ifelse(is.na(DISTRICT), "", DISTRICT ),	ifelse(is.na(TOWN), "", TOWN),	ifelse(is.na(VILLAGE), "", VILLAGE), sep = ","),
                INSTCODE = "TZA_genebank",
                ORIGCTY  = "TZA", # preguntar donde esta el origcty
                crop_name = paste(GENUS, SPECNAME)
                ) %>% 
  dplyr::select(crop_name,
                INSTCODE ,
                ACCENUMB = ACCNUM,
                COLLSITE,
                ORIGCTY,  
                DECLATITUDE = Lat_clean,
                DECLONGITUDE = Lon_clean,
                ELEVATION = ALTITUDE,
                coord_comment) %>% 
  dplyr::filter(!is.na(DECLATITUDE)  ) %>%
  dplyr::filter(!is.na(DECLONGITUDE)) %>%
  dplyr::select(crop_name,
                ORIGCTY,
                ELEVATION,
                INSTCODE,
                DECLATITUDE,
                DECLONGITUDE,
                ACCENUMB,
                COLLSITE,
                coord_comment) %>% 
  dplyr::mutate(crop_name = tolower(crop_name),
                crop_name = stringr::str_replace_all(crop_name, " ", "_")) %>% 
  write.csv(., out, row.names = F) 


####### UGANDA

gnbk <- "uganda"
fl <- file.path(root, gnbk, "uganda_genbank_cleaned.xlsx")

out <- file.path(root, gnbk, "uganda_genbank_GQS.csv")

db_raw <- readxl::read_excel(fl)

names(db_raw)

db_raw %>% 
  dplyr::mutate(COLLSITE = NA,
                INSTCODE = "UGA_genebank",
                ORIGCTY  =  ifelse(Country == "Uganda", "UGA", NA),
                ELEVATION = NA
  ) %>% 
  dplyr::select(crop_name = `Crop Name`,
                INSTCODE ,
                ACCENUMB = `Accession ID`,
                COLLSITE,
                ORIGCTY,  
                DECLATITUDE = Lat_DD,
                DECLONGITUDE = Lon_DD,
                ELEVATION,
                coord_comment) %>% 
  dplyr::filter(!is.na(DECLATITUDE)  ) %>%
  dplyr::filter(!is.na(DECLONGITUDE)) %>%
  dplyr::select(crop_name,
                ORIGCTY,
                ELEVATION,
                INSTCODE,
                DECLATITUDE,
                DECLONGITUDE,
                ACCENUMB,
                COLLSITE,
                coord_comment) %>% 
  dplyr::mutate(crop_name = tolower(crop_name),
                crop_name = stringr::str_replace_all(crop_name, " ", "_")) %>% 
  write.csv(., out, row.names = F) 


###### quality score - use script within docker folder



by_crop <- "D:/OneDrive - CGIAR/Bolder Africa - Gap analysis/data/runs/input_data/by_crop"

root <- "D:/Docker_tests/quality_score_docker/input_data"


av_fls = list.files(by_crop, pattern = "merged.csv", full.names = T, recursive = T)
av_fls = av_fls[!grepl("_old", av_fls)]

for(fl in av_fls){
  cat("Processing: ", fl, "\n")
  
  COMPLETE_data <- read.csv(fl)
  names(COMPLETE_data)[grepl("crop_name", names(COMPLETE_data))] <- "CROPNAME"
  
  final_df <- checking_process_v2(root = root,
                                  COMPLETE_data = COMPLETE_data)
  
  
  final_df %>% 
    dplyr::select(CROPNAME,
                  DECLATITUDE, 
                  DECLONGITUDE,
                  status,
                  source_db,
                  database_id,
                  SCORE,
                  ROUTE,
                  LI,
                  quality_score,
                  issue_txt_desc) %>% 
    write.csv(., gsub("merged.csv", "merged_GQS.csv", fl), row.names = F)
  
  
  
  cats_exclu <- c("Georeferenced to a centroid", 
                  "Zero coordinate", "Coordinate in Sea or Coast line",
                  "Mismatch ORIGCTY and GADM COUNTRY")
  
  final_df %>% 
    dplyr::filter(!grepl(paste0(cats_exclu, collapse = "|"), final_df$issue_txt_desc)) %>% 
    dplyr::select(CROPNAME,
                  DECLATITUDE, 
                  DECLONGITUDE,
                  status,
                  source_db,
                  database_id) %>% 
    write.csv(., gsub("merged.csv", "merged_GQS_cleaned.csv", fl), row.names = F)
  
  
}


####################33
####################3


jute_gene <- read.csv("D:/OneDrive - CGIAR/Bolder Africa - Gap analysis/data/runs/input_data/by_crop/jutemallow/GENESYS/jutemallow_genesys_clean.csv")
jute_clean <- read.csv("D:/OneDrive - CGIAR/Bolder Africa - Gap analysis/data/runs/input_data/by_crop/jutemallow/jutemallow_merged_GQS_cleaned.csv")


id_to_remove <- jute_gene %>% 
  filter(INSTCODE == "BEN097") %>% 
  dplyr::pull(database_id)
  

jute_clean %>% 
  filter(!database_id %in% id_to_remove) %>% 
  write.csv("D:/OneDrive - CGIAR/Bolder Africa - Gap analysis/data/runs/input_data/by_crop/jutemallow/jutemallow_merged_GQS_cleaned.csv", row.names = F)



#####3 unirt country shapefiles en uno solo


shp_root <- "//catalogue/cicap/1.Data/others/country_shps"

shp_fls <- list.files(shp_root, recursive = T, pattern= ".shp$", full.names = T)

shp_cty <- lapply(shp_fls, st_read)

merged <- dplyr::bind_rows(shp_cty)

wrld_shp <- terra::vect("~/GADM4.1_world.gpkg")

format(object.size(wrld_shp), units = "Mb")


coord <- terra::vect(data.frame(lon = -76.35197923862435, lat =  3.5067876057218466), geom = c("lon", "lat"))

system.time({
  extr <- terra::extract( wrld_shp, coord)
})

system.time({
  intr <- terra::intersect(coord, wrld_shp)
})


gbif_df <- read.delim("D:/OneDrive - CGIAR/Documents/BOLDER_WP1_GBIF_all_crops.csv", sep = ",", header = T, na.strings = "NA")
gbif_df %>% 
  filter(!is.na(decimalLatitude)) %>% 
  dplyr::distinct(decimalLatitude, decimalLongitude, .keep_all = T) %>% 
  nrow



  gbif_df$decimalLatitude








source("D:/gap_analysis_shinyapp/www/scripts/04_others/reclass_by_median.R")

  
  
  q = 0.5
  cost <- terra::rast("~/results_gap_analysis_2025/brassica_juncea4/pak_no_chn_ind_irn/results/brassica_juncea/gap_scores/cost_dist_score.tif") %>% 
    reclass_by_median(., q = q)
  dela <- terra::rast("~/results_gap_analysis_2025/brassica_juncea4/pak_no_chn_ind_irn/results/brassica_juncea/gap_scores/network_score.tif")%>% 
    reclass_by_median(., q = q)
  envi <- terra::rast("~/results_gap_analysis_2025/brassica_juncea4/pak_no_chn_ind_irn/results/brassica_juncea/gap_scores/environmental_score.tif")%>% 
    reclass_by_median(., q = q)
  
  final_gap_map <- sum(cost, dela, envi, na.rm = T)
  final_gap_map[final_gap_map == 0] <- NA
  terra::plet(final_gap_map)


