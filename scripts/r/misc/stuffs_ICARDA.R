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


s3_clt$download_file(
  Bucket = "genebanks",
  Key = "zone=landing/source=gadm/subject=shapefiles/institute=all/country=afg/AFG.shp",
  Filename = "./country_shps/afg.shp"
)

s3_clt$list_objects(
  Bucket  = "genebanks" ,
  Marker  = "zone=landing/source=gadm/subject=shapefiles/institute=all/country=afg/AFG.shp"
)




elev <- terra::rast("C:/Users/acmendez/Downloads/elevation_30s.tif")
col_shp <- terra::vect("C:/Users/acmendez/Downloads/Countries/Colombia/Departments/Departments_Colombia.shp")

elev_cp <- terra::crop(elev, ext(col_shp), mask = T)
elev_cp <- terra::mask(elev_cp, col_shp)


col_slope <- terra::terrain(elev_cp,  v="slope", neighbors=8)

terra::writeRaster(col_slope, "C:/Users/acmendez/Downloads/slope_col.tif")




av_dirs <- list.dirs("G:/Other computers/My Laptop (1)/Google Drive/Documentos varios/fotos celular", recursive = F)

dest_dir <- "E:/Google drive"

aa <- lapply(av_dirs, function(pth){
  
  cat("copying file from: ", pth, "\n")
  dir_nm <- basename(pth)
  
  out_dir <- paste(dest_dir, dir_nm, sep = .Platform$file.sep)
  if(!dir.exists(out_dir)){dir.create(out_dir)}
  
  av_files <- list.files(pth, full.names = T)
  
  empty = sapply(av_files, file.copy, to = out_dir)
  
  return("ok") 
})





av_files <- list.files("G:/Other computers/My Laptop (1)/Google Drive/R_SCRIPTS/imgs/__MACOSX/dataset/imgs", 
                       full.names = T,
                       pattern = ".jpg")

av_files <- paste0("G:/Other computers/My Laptop (1)/Google Drive/R_SCRIPTS/imgs/__MACOSX/dataset/imgs/._train_", 0:40479, ".jpg")
sapply(av_files, file.remove)

isos[!isos %in% bb]


aa  = geodata::gadm("ukr", level = 5, path = tempdir())
a




pacman::p_load(CoordinateCleaner, terra, tidyverse, stringr)


exmpl <- data.frame(species = sample(letters, size = 250, replace = TRUE),
                    decimalLongitude = runif(250, min = 42, max = 51),
                    decimalLatitude = runif(250, min = -26, max = -11))

test <- clean_coordinates(x = exmpl)
#> Testing coordinate validity
#> Flagged 0 records.
#> Testing equal lat/lon
#> Flagged 0 records.
#> Flagged 0 of 250 records, EQ = 0.

  #run more tests
  test <- CoordinateCleaner::clean_coordinates(x = exmpl, 
                            tests = c("capitals", 
                                      "centroids","equal", 
                                      "gbif", "institutions", 
                                      "outliers", "seas", 
                                      "zeros"),
                            )



bf_lnd <- terra::unwrap(CoordinateCleaner::buffland)
plot(bf_lnd)
summary(test)






inp <- db %>% 
  dplyr::filter(GADM_GID_0 == "COL" & CROPNAME  %in%  c("forages", "Forages", "forage")) 


lg = CoordinateCleaner::cc_cen(inp, 
                               lon = "DECLONGITUDE", 
                               lat = "DECLATITUDE", 
                               species = "INSTCODE",
                               buffer = 5000,
                               test = "provinces",
                               verify = F,
                               value = "flagged")

table(lg)

inp %>% 
  filter(!lg)


inp$test = if_else(lg, "blue", "red")

pnts <- terra::vect(inp, c("DECLONGITUDE", "DECLATITUDE"))


leaflet(pnts) %>% 
  addTiles() %>% 
  addCircleMarkers(
    col = ~test,
    radius =  3
  )

inp %>% 
  filter(!lg) %>% 
  View




h_inst <- read.delim("//alliancedfs.alliance.cgiar.org/gap_analysis_landraces/runs/input_data/institution_names/H_institutions.txt", sep = ",", encoding = "Latin-1")
g_inst <- read.delim("//alliancedfs.alliance.cgiar.org/gap_analysis_landraces/runs/input_data/institution_names/G_institutions.txt", sep = ",", encoding = "Latin-1")


h_inst_clean <- h_inst %>% 
  dplyr::select(FULL_NAME = organization,  INSTCODE = code, LONGITUDE = location.lon, LATITUDE = location.lat ) %>%
  dplyr::mutate(LONGITUDE = stringr::str_trim(LONGITUDE) %>%   as.numeric(LONGITUDE),
                LATITUDE  = stringr::str_trim(LATITUDE) %>% as.numeric(LATITUDE) ) %>% 
  tidyr::drop_na()

g_inst_clean <- g_isnt %>% 
  dplyr::select(FULL_NAME, INSTCODE, LONGITUDE, LATITUDE) %>% 
  dplyr::mutate(LONGITUDE = if_else(LONGITUDE == "null", NA, LONGITUDE),
                LATITUDE  = if_else(LATITUDE  == "null", NA, LATITUDE))






ciudad_cent  <- read.csv("C:/Users/acmendez/Downloads/world_cities_centroid.csv")
country_cent <- read.csv("C:/Users/acmendez/Downloads/world_countries_centroids.csv") 
depart_cent  <- read.csv("C:/Users/acmendez/Downloads/world_departments_centroids.csv")


df1 <- ciudad_cent %>% 
  dplyr::select(NAME = Urban.Agglomeration, LATITUDE = Latitude, LONGITUDE = Longitude) %>% 
  dplyr::mutate(TYPE = "city") 

df2 <- country_cent %>% 
  dplyr::select(NAME = COUNTRY, LATITUDE = latitude, LONGITUDE = longitude) %>% 
  dplyr::mutate(TYPE = "country")

df3 <- depart_cent %>% 
  dplyr::select(NAME = name, LATITUDE  = centroid.lat, LONGITUDE = centroid.lon) %>% 
  dplyr::mutate(TYPE = "department")

df4 <- CoordinateCleaner::institutions %>% 
  dplyr::select(NAME = name, LATITUDE  = decimalLatitude, LONGITUDE = decimalLongitude) %>% 
  dplyr::mutate(TYPE = "institution")

bind_rows(df1, df2, df3, df4) %>% 
  write.csv(., "C:/Users/acmendez/Downloads/centroid_checks_df.csv", row.names =F)




### obtener conteo de accesiones por coordenada

db <- vroom::vroom("C:/Users/acmendez/Downloads/genesys_quality_score_BORDERS (1).csv")
##do te same with coords that do not match CTY

db %>% 
  filter(!is.na(DECLATITUDE) & !is.na(DECLONGITUDE)) %>% 
  dplyr::mutate(id_latlng = paste0(DECLATITUDE, ";", DECLONGITUDE)) %>% 
  dplyr::group_by(INSTCODE, id_latlng, check_centroid_cat) %>% 
  tally() %>% 
  #arrange(desc(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(INSTCODE, check_centroid_cat) %>% 
  dplyr::reframe(min = min(n),max = max(n), greather_25 = sum(n >= 25 )) %>% 
  arrange(desc(max))  %>% 
  writexl::write_xlsx(.,"C:/Users/acmendez/downloads/acc_per_coord_countsV2.xlsx" )
  
db %>% 
  filter(!is.na(DECLATITUDE) & !is.na(DECLONGITUDE)) %>% 
  dplyr::mutate(id_latlng = paste0(DECLATITUDE, ";", DECLONGITUDE)) %>%
  filter( INSTCODE == "LBN002", id_latlng == "29;120") %>% 
  View()

db %>% 
  filter(!is.na(DECLATITUDE) & !is.na(DECLONGITUDE)) %>% 
  dplyr::mutate(id_latlng = paste0(DECLATITUDE, ";", DECLONGITUDE)) %>%
  filter( id_latlng == "29;120") %>% 
  dplyr::mutate(taxon = paste(GENUS, SPECIES, SUBTAXA, sep = "-")) %>% 
  group_by(INSTCODE, taxon) %>% 
  tally() %>% 
  View()



db %>% 
  filter(!is.na(DECLATITUDE) & !is.na(DECLONGITUDE)) %>% 
  dplyr::mutate(id_latlng = paste0(DECLATITUDE, ";", DECLONGITUDE)) %>% 
  dplyr::group_by(INSTCODE, id_latlng, check_bordering_cnt) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(INSTCODE, check_bordering_cnt) %>% 
  dplyr::reframe(count = length(), min = min(n),max = max(n), greather_25 = sum(n >= 25 )) %>% 
  arrange(desc(max)) %>% 
  writexl::write_xlsx(.,"C:/Users/acmendez/downloads/acc_per_coord_countsV3.xlsx" )


db %>% 
  filter(!is.na(DECLATITUDE) & !is.na(DECLONGITUDE)) %>% 
  dplyr::mutate(id_latlng = paste0(DECLATITUDE, ";", DECLONGITUDE)) %>% 
  dplyr::group_by(INSTCODE, check_wrong_ORIGCTY, check_bordering_cnt) %>% 
  tally() %>% 
  dplyr::ungroup() %>% 
  drop_na %>% 
  dplyr::mutate(tot = sum(n), .by = INSTCODE) %>% 
  writexl::write_xlsx(.,"C:/Users/acmendez/downloads/cnt_wrong_CTY.xlsx" )


db %>% 
  filter(!is.na(DECLATITUDE) & !is.na(DECLONGITUDE)) %>% 
  dplyr::mutate(id_latlng = paste0(DECLATITUDE, ";", DECLONGITUDE)) %>% 
  dplyr::group_by(INSTCODE, check_centroid_cat) %>% 
  tally() %>% 
  dplyr::ungroup() %>% 
  drop_na %>% 
  dplyr::mutate(tot = sum(n), .by = INSTCODE) %>% 
  writexl::write_xlsx(.,"C:/Users/acmendez/downloads/centroid_counts.xlsx" )


db %>% 
  filter(!is.na(DECLATITUDE) & !is.na(DECLONGITUDE)) %>% 
  dplyr::mutate(id_latlng = paste0(DECLATITUDE, ";", DECLONGITUDE)) %>% 
  dplyr::group_by(INSTCODE, check_wrong_ORIGCTY, check_bordering_cnt) %>% 
  tally() %>% 
  dplyr::ungroup() %>% 
  drop_na %>% 
  dplyr::mutate(tot = sum(n), .by = INSTCODE) %>% 
  writexl::write_xlsx(.,"C:/Users/acmendez/downloads/centroid_counts.xlsx" )



db %>% 
  filter(!is.na(DECLATITUDE) & !is.na(DECLONGITUDE)) %>% 
  filter(INSTCODE == "BEL084", check_bordering_cnt) %>% View

x11()
db %>%
  filter(!is.na(DECLATITUDE) & !is.na(DECLONGITUDE)) %>%
  dplyr::select(INSTCODE, ELEVATION, check_elev_suggested, check_elev_diff) %>%
  tidyr::drop_na() %>% 
  ggplot()+
  geom_histogram(aes( y = check_elev_diff))+
  ylab("Count")+
  xlab("Elevation diff")+
  theme_bw()


db %>%
  filter(!is.na(DECLATITUDE) & !is.na(DECLONGITUDE)) %>%
  dplyr::select(INSTCODE, ELEVATION, check_elev_suggested, check_elev_diff) %>%
  tidyr::drop_na() %>% 
  ggplot()+
  geom_boxplot(aes( y = check_elev_diff))+
  xlab("Count")+
  ylab("Elevation diff")+
  theme_bw()



########
pacman::p_load(ggmosaic)

get_branchs <- function(arbol, db){
  
  tmp_df <- db %>% 
    dplyr::mutate(check_decimals_cat = ifelse(check_decimals_lon < 2 | check_decimals_lat < 2, TRUE, FALSE),
                  check_sea_coast = ifelse(is.na(GADM_GID_0) & check_location_available, 1, 0),
                  check_ORIGCTY = case_when(
                    !is.na(ORIGCTY)  ~ 1,
                    #is.na(ORIGCTY) ~ NA,
                    .default = 0
                  ),
                  check_location_available = as.numeric(check_location_available),
                  check_zero_coords = ifelse(check_zero_coords, 1, 0  ),
                  check_centroid_cat = ifelse(check_centroid_cat == "Ok", 0, 1),
                  check_decimals_cat = ifelse(check_decimals_cat, 1, 0),
                  check_wrong_ORIGCTY = ifelse(check_wrong_ORIGCTY, 0, 1),
                  check_bordering_cnt = ifelse(check_bordering_cnt, 1, 0),
                  check_elev_cat = ifelse(check_elev_cat | is.na(check_elev_cat) , 1, 0),
                  admon_lvl_1_matched = case_when(
                    !is.na(admon_lvl_1_matched ) & check_location_available ~ 1,
                    is.na(admon_lvl_1_matched ) & check_location_available ~ 0,
                    .default = NA
                  ),
                  admon_lvl_2_matched = case_when(
                    !is.na(admon_lvl_2_matched ) & check_location_available ~ 1,
                    is.na(admon_lvl_2_matched ) & check_location_available ~ 0,
                    .default = NA
                  ),
                  check_collsite = ifelse(check_collsite, 1, 0)
                  
    ) %>% 
    dplyr::select(
      check_ORIGCTY,
      check_location_available,
      check_zero_coords,
      check_sea_coast,
      check_centroid_cat,
      check_decimals_cat,
      check_wrong_ORIGCTY,
      check_bordering_cnt,
      check_elev_cat,
      check_collsite,
      admon_lvl_1_matched,
      admon_lvl_2_matched  
    ) 
  
  stopifnot("missmatch in names" = all(names(tmp_df) %in% names(arbol)))
  tmp_df$SCORE <- NA
  tmp_df$LI <- NA
  tmp_df$ROUTE <- NA
  
  drop_vars <- c( "SCORE","LI"  )
  pos_drop <- grep(paste0(drop_vars, collapse = "|"), names(arbol))
  
  all_combs <- apply(arbol[, -pos_drop], 1, function(rw){
    paste0(rw[!is.na(rw)], collapse = "")})
  
  stopifnot("combs are not unique" = all(table(all_combs) == 1))
  
  df_keys <- data.frame(arbol[, drop_vars], all_combs)
  
  
  for(i in 1:nrow(arbol)){
    cat("Processing: ", i, "\n")
    vars_names <- names(arbol)[!is.na(arbol[i, ] )]
    vars_names <- vars_names[-grep(paste0(drop_vars, collapse = "|"), vars_names)]
    
    tmp_df$combi = apply(tmp_df[, vars_names],1, paste0, collapse = "")
    
    tmp_df$SCORE[ tmp_df$combi == df_keys$all_combs[i] & is.na(tmp_df$SCORE) ] <- df_keys$SCORE[i]
    tmp_df$LI[ tmp_df$combi == df_keys$all_combs[i] & is.na(tmp_df$LI)] <- df_keys$LI[i]
    tmp_df$ROUTE[ tmp_df$combi == df_keys$all_combs[i] & is.na(tmp_df$ROUTE) ] <- paste0("R", i)
    
    
  }
  
  
  tmp_df$combi <- NULL
  
  return(tmp_df[, c(drop_vars, "ROUTE")])
  
}

db <- vroom::vroom("C:/Users/acmendez/Downloads/genesys_quality_score_BORDERS (1).csv")

arbol <- readxl::read_excel("D:/OneDrive - CGIAR/Genebanks/data/arbol_decision_v1.xlsx", sheet = "Sheet3")

arbol <- arbol %>% dplyr::select(-cat_score, -ROUTE)

decisions <- get_branchs(arbol = arbol, db = db)

final_df <- cbind(db, decisions)

to_plot <- final_df %>% 
  dplyr::mutate(quality_score = case_when(
    SCORE <= 4 ~ "Low",
    SCORE > 4 & SCORE <= 8 ~ "Moderate",
    SCORE > 8 ~ "High",
    .default = NA
    ),
    quality_score = factor(quality_score, levels = c("Low", "Moderate", "High")),
    prescription = case_when(
      LI == 1 ~ "Hard",
      LI == 2 ~ "Moderate",
      LI == 3 ~ "Easy"
    ),
    prescription = factor(prescription, levels = c("Easy", "Moderate", "Hard")))# %>% 
 # dplyr::select(INSTCODE, quality_score, prescription) 
#tidyr::pivot_longer(., cols  = -INSTCODE, names_to = "var",values_to = "value")
writexl::write_xlsx(to_plot, "C:/Users/acmendez/Downloads/genesys_quality_score_v3.0.xlsx")

to_plot2 <- to_plot %>%
  ungroup() %>% 
  group_by(quality_score, prescription) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

g <- ggplot(to_plot2)+
  ggmosaic::geom_mosaic(aes( x = product(quality_score), fill = prescription))+
  theme_mosaic() +
  xlab("Quality")+
  ylab("")+
  labs(fill = "Level of improvement")+
  geom_text(stat = 'mosaic', aes(x = product(quality_score), label = paste0(round(count/sum(count)*100, 1), '%')),
            position = position_stack(vjust = 0.5))
  scale_fill_manual(values = c("#009929",  "#FEE090", "#bd0003"))

ggsave(g, filename = "C:/Users/acmendez/Downloads/mosaic_v1.png", width = 6, height = 6, units = "in", dpi = 300)


g2 <- ggplot(to_plot)+
  ggmosaic::geom_mosaic(aes( x = product(quality_score), fill = prescription))+
  theme_mosaic() +
  xlab("Quality")+
  ylab("")+
  facet_wrap( ~INSTCODE, ncol = 3, scales = "free")+
  labs(fill = "Level of improvement")+
  scale_fill_manual(values = c("#009929",  "#FEE090", "#bd0003"))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))

ggsave(g2, filename = "C:/Users/acmendez/Downloads/mosaic_v2.png", width = 8, height = 8, units = "in", dpi = 300)




to_plot %>% 
  dplyr::group_by(quality_score, prescription) %>% 
  dplyr::tally() %>% 
  dplyr::mutate(prop = paste0(round(n/sum(n), 3)*100, "%")) %>% 
  write_xlsx(., "C:/Users/acmendez/Downloads/tabla_score_V3.xlsx")


to_plot %>% 
  filter(INSTCODE == "MEX002") %>% 
  View




to_plot %>% 
  filter(INSTCODE %in% c("NGA039", "BEL084", "COL003", "LBN002")) %>% 
  select(INSTCODE, ACCENUMB, ORIGCTY, 
         GADM_GID_0,COLLSITE, DECLATITUDE, DECLONGITUDE, SCORE, ROUTE,
         starts_with("check_centroid"),
         check_wrong_ORIGCTY,
         check_bordering_cnt,
         quality_score, prescription)  %>%
  filter(check_wrong_ORIGCTY & !check_bordering_cnt) %>% 
  View()
  

#############################################
#### accesiones duplicadas #################
###########################################



raw_db <- vroom::vroom("C:/Users/acmendez/Downloads/genesys_quality_score_Vfinal.csv")


raw_db$id_latlng <- paste0(raw_db$DECLATITUDE, ";", raw_db$DECLONGITUDE)

raw_db %>% 
  filter(!is.na(DECLATITUDE) & !is.na(DECLONGITUDE)) %>% 
  dplyr::group_by(INSTCODE, CROPNAME, id_latlng, check_centroid_cat) %>%
  dplyr::add_count(id_latlng, name = "check_numb_acc_per_coord") %>% View

  tally() %>% 
  arrange(desc(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(INSTCODE, check_centroid_cat) %>% 
  dplyr::reframe(min = min(n),max = max(n), greather_25 = sum(n >= 25 )) %>% 
  arrange(desc(max))  %>% 
  writexl::write_xlsx(.,"C:/Users/acmendez/downloads/acc_per_coord_countsV2.xlsx" )

db %>% 
  filter(!is.na(DECLATITUDE) & !is.na(DECLONGITUDE)) %>% 
  dplyr::mutate(id_latlng = paste0(DECLATITUDE, ";", DECLONGITUDE)) %>%
  filter( INSTCODE == "LBN002", id_latlng == "29;120") %>% 
  View()

db %>% 
  filter(!is.na(DECLATITUDE) & !is.na(DECLONGITUDE)) %>% 
  dplyr::mutate(id_latlng = paste0(DECLATITUDE, ";", DECLONGITUDE)) %>%
  filter( id_latlng == "29;120") %>% 
  dplyr::mutate(taxon = paste(GENUS, SPECIES, SUBTAXA, sep = "-")) %>% 
  group_by(INSTCODE, taxon) %>% 
  tally() %>% 
  View()



