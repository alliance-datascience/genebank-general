require(pacman)
pacman::p_load(readxl, writexl, tidyverse, vroom)


 
db <- vroom::vroom("C:/Users/acmendez/Downloads/genesys_quality_score_08-2024.csv")

tree <-read.csv("D:/OneDrive - CGIAR/Genebanks/data/decision_tree_v1.csv") 

labs_dec <- c("Missing ORIGCTY",  "Missing Coordinates", "Zero coordinate", "Coordinate in Sea or Coast line",
"Georeferenced to a centroid", "Belong to a set of accessions (more than 25) with the same coordinate",
"Less than two Decimal places in coordinate", "Mismatch ORIGCTY and GADM COUNTRY",
"Coordinate near to country border", "Difference in ELEVATION and STRM (elevation) greather than 150 mts or missing", 
"Missing COLLSITE", 
"Missing Country admon level 1 in COLLSITE",
"Missing Country admon level 2 in COLLSITE")

for(i in 1:(ncol(tree)-3) ){
  
  nms <- labs_dec[i]
  tree[, i] <- ifelse(is.na(tree[, i]), NA, ifelse(tree[, i] == 0, nms, "-"))

}

tree$issue_txt_desc <- apply(tree[, 1:(ncol(tree)-3)], 1, function(rw){
  
  paste0(rw[!is.na(rw) & rw != "-"], collapse = "; ")
  
})


 db <- db %>%
   dplyr::left_join(., tree[, c("ROUTE", "issue_txt_desc")]) %>% 
   dplyr::filter(INSTCODE %in% c("COL003", "LBN002")) |>
   #dplyr::filter(!is.na(DECLONGITUDE) | !is.na(DECLATITUDE)) %>% 
   dplyr::mutate(
   quality_score = ifelse(SCORE == 9, "Moderate", quality_score),
   check_market_acc = ifelse(check_market_acc, "Market", ifelse(is.na(check_market_acc), NA, "Non market")),
   GADM_GID_0       = ifelse(is.na(GADM_GID_0) & !is.na(ORIGCTY), ORIGCTY, GADM_GID_0))%>%
   dplyr::select(id,INSTCODE,	ACCENUMB,	CROPNAME,	COLLSITE,	DECLATITUDE,	DECLONGITUDE, ELEVATION, 
                 check_collsite,	check_location_available,	ORIGCTY,	GADM_GID_0, check_bordering_cnt,	
                 check_elev_suggested, check_slope_suggested,	check_market_acc,	SCORE,	LI,	ROUTE,	
                 quality_score, issue_txt_desc)
#  %>%  
#    #arrow::write_parquet(., "./coord_quality_score/www/genesys_coord_check_to_app_new.parquet")
#    write_csv(., "./coord_quality_score/www/genesys_coord_check_to_app_new.csv")
#  
 conn <- dbConnect(drv = RSQLite::SQLite(), "./coord_quality_score/www/genesys_coord_check_to_app_new.sqlite")

 dbWriteTable(
   conn = conn,
   name = "quality_score_sql",
   value = db,
   overwrite = TRUE
 )
 
 dbDisconnect(conn)
 

#  db <- read.csv("./coord_quality_score/www/genesys_coord_check_to_app_new.csv", header = T)
#  format(object.size(db), units = "Mb")


 db_conn <- dbConnect(drv = RSQLite::SQLite(), "./coord_quality_score/www/genesys_coord_check_to_app_new.sqlite")
 
db <- dbGetQuery(
  conn = db_conn,
  "SELECT * FROM quality_score_sql"
)



 