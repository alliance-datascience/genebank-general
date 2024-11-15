suppressMessages(if(!require(pacman)){install.packages("pacman");library(pacman)}else{library(pacman)})
pacman::p_load(data.table, sf, here, geodata, dplyr, stringr, stringi,writexl, arrow, readr,
               terra, vroom, httr, jsonlite, stringr, cld3, geosphere, readxl, paws, paws.storage)

#' Function to copy country shapefiles files from S3 bucket to EC2 R-studio session
#' @param s3_clt (object):S3 client from paws.strograge call
#' @param out_dir_name (character): out directory name
#' @return NULL
#' @example copy_s3_country_shp(s3_clt,  file.path(root,"country_shps"))
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
      
      out_dir <- file.path( out_dir_name , b_nm)
      out_file <- file.path(out_dir, b_nm_ext )
      
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



#' Function to copy slope and elevation tiff files from S3 bucket to EC2 R-studio session
#' @param s3_clt (object):S3 client from paws.strograge call
#' @param out_dir_name (character): out directory name
#' @return NULL
#' @example copy_s3_misc_files(s3_clt,  file.path(root,"misc"))
copy_s3_misc_files <- function( s3_clt, out_dir_name){
  
  
  out_dir <- file.path( out_dir_name)
  
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
    
    out_file <- file.path(out_dir, b_nm_ext)
    
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
#' @example copy_s3_centorids_db_files(s3_clt,  file.path(root,"centroids_data"))
copy_s3_centorids_db_files <- function( s3_clt, out_dir_name){
  
  
  out_dir <- file.path( out_dir_name)
  
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
    
    out_file <- file.path(out_dir, b_nm_ext)
    
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
#' @example copy_s3_decision_tree_file(s3_clt,  file.path(root,"decision_tree"))
copy_s3_decision_tree_file <- function( s3_clt, out_dir_name){
  
  out_dir <- file.path( out_dir_name)
  
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
    
    out_file <- file.path(out_dir, b_nm_ext)
    
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
#' @example copy_s3_genesys_data(s3_clt,  file.path(root,"quality_score_data"))
copy_s3_genesys_data <- function( s3_clt, out_dir_name){
  
  out_dir <- file.path( out_dir_name)
  
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
    
    out_file <-file.path(out_dir, b_nm_ext)
    
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
#' @example copy_s3_bordering_countries_db_files(s3_clt,  file.path(root,"country_borders"))
copy_s3_bordering_countries_db_files <- function( s3_clt, out_dir_name){
  
  
  out_dir <- file.path( out_dir_name)
  
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
    
    out_file <- file.path(out_dir, b_nm_ext)
    
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


#' Function to create necessary folders and execute copy_s3 functions
#' @param s3_clt (object):S3 client from paws.strograge call
#' @param root (object): fullpath to root folder
#' @return NULL
#' @example get_input_files(s3_clt, root = file.path('path to dir'))
get_input_files <- function(s3_clt,  root){
  
  dir_names <- c("country_shps", "misc", "centroids_data", "decision_tree", "quality_score_data", "country_borders", "score_results")
  
  dir_path <- lapply(dir_names, function(pt){
    fl <- file.path(root, pt)
    dir.create(fl, recursive = T)
    return(fl)
  
    })
  
  copy_s3_country_shp(s3_clt,  file.path(root,"country_shps"))
  copy_s3_misc_files(s3_clt,  file.path(root,"misc"))
  copy_s3_centorids_db_files(s3_clt,  file.path(root,"centroids_data"))
  copy_s3_decision_tree_file(s3_clt,  file.path(root,"decision_tree"))
  #copy_s3_genesys_data(s3_clt,  file.path(root,"quality_score_data"))
  copy_s3_bordering_countries_db_files(s3_clt,  file.path(root,"country_borders"))
  
  try({
    
    download.file("https://github.com/alliance-datascience/genebank-general/raw/refs/heads/dev/scripts/r/misc/GCA_data_dictionary.xlsx", 
                  destfile = file.path(root, "quality_score_data", "GCA_data_dictionary.xlsx"))
    
  })
  
  
  return(NULL)
  
  
}






