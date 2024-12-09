require(pacman)

pacman::p_load(paws, paws.storage, terra, geodata)

pacman::p_load(data.table, maptools, sf, here, geodata, dplyr, stringr, stringi, sp,
               terra, vroom, httr, jsonlite, stringr, cld3, geosphere, readxl, paws , terra, geodata,
               stringr)



s3_clt <- paws.storage::s3(
  config = list(
    credentials = list(
      creds = list(access_key_id     = 'sample',
                   secret_access_key = 'sample')
    ),
    region = 'us-east-2'
  )
)


#country names

nms_fls <- s3_clt$list_objects(
  Bucket  = "genebanks",
  Prefix  = "zone=landing/source=gadm/subject=shapefiles/institute=all/"
)

nms <- sapply(nms_fls$Contents, function(lst)lst$Key)
nms <- stringr::str_extract(nms, "country=[a-z]{3}")
nms <- stringr::str_replace(nms, "country=", "")
nms <- unique(nms)
nms <- na.omit(nms)

#Listar objetos dentro del bucker en la ruta especificada

for(c_nm in nms){
  
  cat("> Downloading data for: ", c_nm, "\n")
  
  shps_fls <- s3_clt$list_objects(
    Bucket  = "genebanks",
    Prefix  = paste0("zone=landing/source=gadm/subject=shapefiles/institute=all/country=", c_nm)
  )
  
  av_fls <- sapply(shps_fls$Contents, function(lst)lst$Key)
  
  # Descargar los archivos a la sesion de R
  lapply(av_fls, function(fl){
    
    b_nm_ext <- basename(fl)
    b_nm <- gsub(".[a-zA-Z]{3}$", "", b_nm_ext)
    out_dir <- paste0("./country_shps/", b_nm)
    out_file <- paste0(out_dir, "/", b_nm_ext)
    if(!dir.exists(out_dir)){dir.create(out_dir, recursive = T)}
    
    s3_clt$download_file(
      Bucket = "genebanks",
      Key = fl,
      Filename = paste0("./country_shps/", b_nm, "/",  b_nm_ext)
    )
    
  })
  
}



