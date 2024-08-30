
#' Function to copy country shapefiles files from S3 bucket to EC2 R-studio session
#' @param s3_clt (object):S3 client from paws.strograge call
#' @param out_dir_name (character): out directory name
#' @return NULL
copy_s3_country_shp <- function( s3_clt, out_dir_name){
  
  nms_fls <- s3_clt$list_objects_v2(
    Bucket  = "genebanks",
    Prefix  = "zone=landing/source=gadm/subject=shapefiles/institute=all/"
  )
  
  nms <- sapply(nms_fls$Contents, function(lst)lst$Key)
  
  if(length(nms_fls$NextContinuationToken) > 0){
    nms_fls_add <- s3_clt$list_objects_v2(
      Bucket  = "genebanks",
      Prefix  = "zone=landing/source=gadm/subject=shapefiles/institute=all/",
      ContinuationToken = nms_fls$NextContinuationToken
    )
    nms_add <- sapply(nms_fls_add$Contents, function(lst)lst$Key)
    nms_add <- nms_add[!grepl("country=world", nms_add)]
    nms <- c(nms, nms_add)
  }
  
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
      
      out_dir <- here::here( out_dir_name , b_nm)
      out_file <- here::here(out_dir, b_nm_ext )
      
      if(!dir.exists(out_dir)){dir.create(out_dir, recursive = T)}
      
      if(!file.exists(out_file)){
      s3_clt$download_file(
        Bucket = "genebanks",
        Key = fl,
        Filename = out_file
      )
      }else{
        cat(">>> file aready exists. \n")
      }
      
    })
    
  }
  
  return(NULL)
  
}




#' Function to copy world Roads shapefiles files from S3 bucket to EC2 R-studio session
#' @param s3_clt (object):S3 client from paws.strograge call
#' @param out_dir_name (character): out directory name
#' @return NULL
copy_s3_roads_shp <- function( s3_clt, out_dir_name){
  
  
  out_dir <- here::here( out_dir_name)
  if(!dir.exists(out_dir)){dir.create(out_dir, recursive = T)}
  
  nms_fls <- s3_clt$list_objects(
    Bucket  = "genebanks",
    Prefix  = "zone=landing/source=OSM_roads/"
  )
  
  nms <- sapply(nms_fls$Contents, function(lst)lst$Key)
  nms <- nms[grepl(".gpkg$", nms)]
  #nms <- stringr::str_extract(nms, "country=[a-zA-Z]{3}")
  #nms <- stringr::str_replace(nms, "country=", "")
  #nms <- unique(nms)
  #nms <- na.omit(nms)
  
  
  for(c_nm in nms){
    
    cat("> Downloading data for: ", c_nm, "\n")
    
 
      b_nm_ext <- basename(c_nm)
      #b_nm <- gsub(".gpkg$", "", b_nm_ext)
      
      out_file <- here::here(out_dir, b_nm_ext)
      if(!file.exists(out_file)){
        
        s3_clt$download_file(
          Bucket = "genebanks",
          Key = c_nm,
          Filename = out_file
        )
        
      }else{
        cat(">>> file aready exists. \n")
      }
      
      
    
    
  }
  
  return(NULL)
}



#' Function to copy slope and elevation tiff files from S3 bucket to EC2 R-studio session
#' @param s3_clt (object):S3 client from paws.strograge call
#' @param out_dir_name (character): out directory name
#' @return NULL
copy_s3_misc_files <- function( s3_clt, out_dir_name){
  
  
  out_dir <- here::here( out_dir_name)
  
  nms_fls <- s3_clt$list_objects(
    Bucket  = "genebanks",
    Prefix  = "zone=landing/source=misc/"
  )
  
  nms <- sapply(nms_fls$Contents, function(lst)lst$Key)
  nms <- nms[grepl(".tif$", nms)]
  stopifnot("Found more than two files." = length(nms) == 2)
  
  
  if(!dir.exists(out_dir)){dir.create(out_dir, recursive = T)}
  
 for(c_nm in nms){
   
   cat("> Downloading data for: ", c_nm, "\n")
   b_nm_ext <- basename(c_nm)

   out_file <- here::here(out_dir, b_nm_ext)
   
   if(!file.exists(out_file)){
     s3_clt$download_file(
       Bucket = "genebanks",
       Key = c_nm,
       Filename = out_file
     )
   }else{
     cat(">>> File already exists. \n")
   }
   
   
   
 }
   
  
  return(NULL)
  
}



#' Function to copy roads_dist_db from S3 bucket to EC2 R-studio session
#' @param s3_clt (object):S3 client from paws.strograge call
#' @param out_dir_name (character): out directory name
#' @return NULL
copy_s3_roads_dist_db_files <- function( s3_clt, out_dir_name){
  
  
  out_dir <- here::here( out_dir_name)
  
  nms_fls <- s3_clt$list_objects(
    Bucket  = "genebanks",
    Prefix  = "zone=landing/source=roads_dist_db/"
  )
  
  nms <- sapply(nms_fls$Contents, function(lst)lst$Key)
  nms <- nms[grepl(".parquet$", nms)]
  
  if(!dir.exists(out_dir)){dir.create(out_dir, recursive = T)}
  
  for(c_nm in nms){
    
    cat("> Downloading data for: ", c_nm, "\n")
    b_nm_ext <- basename(c_nm)
    
    out_file <- here::here(out_dir, b_nm_ext)
    
    if(!file.exists(out_file)){
      s3_clt$download_file(
        Bucket = "genebanks",
        Key = c_nm,
        Filename = out_file
      )
    }else{
      cat(">>> File already exists. \n")
    }
    
    
    
  }
  
  
  return(NULL)
  
}



#' Function to copy centroids_check_db.csv from S3 bucket to EC2 R-studio session
#' @param s3_clt (object):S3 client from paws.strograge call
#' @param out_dir_name (character): out directory name
#' @return NULL
copy_s3_centorids_db_files <- function( s3_clt, out_dir_name){
  
  
  out_dir <- here::here( out_dir_name)
  
  nms_fls <- s3_clt$list_objects(
    Bucket  = "genebanks",
    Prefix  = "zone=landing/source=centroids/"
  )
  
  nms <- sapply(nms_fls$Contents, function(lst)lst$Key)
  nms <- nms[grepl(".csv$", nms)]
  
  if(!dir.exists(out_dir)){dir.create(out_dir, recursive = T)}
  
  for(c_nm in nms){
    
    cat("> Downloading data for: ", c_nm, "\n")
    b_nm_ext <- basename(c_nm)
    
    out_file <- here::here(out_dir, b_nm_ext)
    
    if(!file.exists(out_file)){
      s3_clt$download_file(
        Bucket = "genebanks",
        Key = c_nm,
        Filename = out_file
      )
    }else{
      cat(">>> File already exists. \n")
    }
    
    
    
  }
  
  
  return(NULL)
  
}

#' Function to copy bordering_countries.csv from S3 bucket to EC2 R-studio session
#' @param s3_clt (object):S3 client from paws.strograge call
#' @param out_dir_name (character): out directory name
#' @return NULL
copy_s3_bordering_countries_db_files <- function( s3_clt, out_dir_name){
  
  
  out_dir <- here::here( out_dir_name)
  
  nms_fls <- s3_clt$list_objects(
    Bucket  = "genebanks",
    Prefix  = "zone=landing/source=bordering_countries/"
  )
  
  nms <- sapply(nms_fls$Contents, function(lst)lst$Key)
  nms <- nms[grepl(".csv$", nms)]
  
  if(!dir.exists(out_dir)){dir.create(out_dir, recursive = T)}
  
  for(c_nm in nms){
    
    cat("> Downloading data for: ", c_nm, "\n")
    b_nm_ext <- basename(c_nm)
    
    out_file <- here::here(out_dir, b_nm_ext)
    
    if(!file.exists(out_file)){
      s3_clt$download_file(
        Bucket = "genebanks",
        Key = c_nm,
        Filename = out_file
      )
    }else{
      cat(">>> File already exists. \n")
    }
    
    
    
  }
  
  
  return(NULL)
  
}


#' Function to copy decision_tree.csv from S3 bucket to EC2 R-studio session
#' @param s3_clt (object):S3 client from paws.strograge call
#' @param out_dir_name (character): out directory name
#' @return NULL
copy_s3_decision_tree_file <- function( s3_clt, out_dir_name){
  
  out_dir <- here::here( out_dir_name)
  
  nms_fls <- s3_clt$list_objects(
    Bucket  = "genebanks",
    Prefix  = "zone=landing/source=decision_tree/"
  )
  
  nms <- sapply(nms_fls$Contents, function(lst)lst$Key)
  nms <- nms[grepl(".csv$", nms)]
  
  if(!dir.exists(out_dir)){dir.create(out_dir, recursive = T)}
  
  for(c_nm in nms){
    
    cat("> Downloading data for: ", c_nm, "\n")
    b_nm_ext <- basename(c_nm)
    
    out_file <- here::here(out_dir, b_nm_ext)
    
    if(!file.exists(out_file)){
      s3_clt$download_file(
        Bucket = "genebanks",
        Key = c_nm,
        Filename = out_file
      )
    }else{
      cat(">>> File already exists. \n")
    }
    
    
    
  }
  
  
  return(NULL)
  
}


#' Function to copy genesys downloaded data from S3 bucket to EC2 R-studio session
#' @param s3_clt (object):S3 client from paws.strograge call
#' @param out_dir_name (character): out directory name
#' @return NULL
copy_s3_genesys_data <- function( s3_clt, out_dir_name){
  
  out_dir <- here::here( out_dir_name)
  
  nms_fls <- s3_clt$list_objects(
    Bucket  = "genebanks",
    Prefix  = "zone=raw/source=genesys/subject=passport_data_07-2024_filtered/institute=all/"
  )
  
  nms <- sapply(nms_fls$Contents, function(lst)lst$Key)
  nms <- nms[grepl(".csv$", nms)]
  
  if(!dir.exists(out_dir)){dir.create(out_dir, recursive = T)}
  
  for(c_nm in nms){
    
    cat("> Downloading data for: ", c_nm, "\n")
    b_nm_ext <- basename(c_nm)
    
    out_file <- here::here(out_dir, b_nm_ext)
    
    if(!file.exists(out_file)){
      s3_clt$download_file(
        Bucket = "genebanks",
        Key = c_nm,
        Filename = out_file
      )
    }else{
      cat(">>> File already exists. \n")
    }
    
    
    
  }
  
  
  return(NULL)
  
}




