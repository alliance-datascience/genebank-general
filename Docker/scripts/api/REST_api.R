require(pacman)
pacman::p_load(tidyverse, plumber)

#' @apiTitle QualityScore API
#' @apiDescription API for calculating QualityScore

source(file.path(Sys.getenv("SCRIPT_DIR"), "02_genesys_quality_score_v2.R"))
#source("D:/Docker_tests/quality_score_docker/scripts/quality_score/02_genesys_quality_score_v2.R")

#' Returns json with quality score calculated for single accession
#* @param ACCENUMB:str Accession number
#* @param COLLSITE:str Collecting site description
#* @param ORIGCTY:str  Country of origin
#* @param DECLATITUDE:number Decimal degree latitude
#* @param DECLONGITUDE:number Decimal degree longitude
#* @param ELEVATION:number elevation (Meters above the sea level)
#* @param INSTCODE:str Institution code
#* @param CROPNAME:str Crop Name
#* @get /single_accession
#* @tag Single-accession
#* @serializer json
function(ACCENUMB, COLLSITE = NA, ORIGCTY = NA, DECLATITUDE = NA, DECLONGITUDE = NA, ELEVATION = NA, INSTCODE, CROPNAME) {
  
  
dta <- data.frame(ACCENUMB     = ACCENUMB, 
                  COLLSITE     = COLLSITE,
                  ORIGCTY      = COLLSITE, 
                  DECLATITUDE  = as.numeric(DECLATITUDE), 
                  DECLONGITUDE = as.numeric(DECLONGITUDE), 
                  ELEVATION    = as.numeric(ELEVATION),
                  INSTCODE     = INSTCODE, 
                  CROPNAME     = CROPNAME)

print("estoy entrando al calculo")
#pth <- Sys.getenv("DB_PATH")
df_final <- checking_process_v2(root = Sys.getenv("WORK_DIR"), 
                    COMPLETE_data = dta)

df_final <- dplyr::select(df_final, 
                          ACCENUMB, 
                          COLLSITE, 
                          ORIGCTY, 
                          DECLATITUDE, 
                          DECLONGITUDE, 
                          ELEVATION, 
                          INSTCODE, 
                          CROPNAME,
                          SCORE,
                          `LEVEL_OF_IMP` = LI,
                          ROUTE,
                          QUALITY_SCORE = quality_score,
                          ISSUE = issue_txt_desc)



return(df_final)

}

parser_csv2 <- function(...) {
  parser_read_file(function(tmpfile) {
    utils::read.csv(tmpfile, header = T, sep = ",")
  })
}

register_parser("csv2", parser_csv2, fixed = "text/csv")

#' Returns json with quality score calculated for passport data files (only input csv allowed)
#* @param dataset:file Comma separated value passport data file
#* @parser multi
#* @parser csv2
#* @post /upload
#* @response 200 Quality score data.frame
#* @serializer csv list(type="text/plain; charset=UTF-8")
#* @tag Delimited-text-file
function(dataset) {
  
  # Load the file based on its type
  if (is.raw(dataset)) {
    stop("Unsupported file type, please use a RDS, CSV, XLS or XLSX file.")
  }else if("list" %in% class(dataset)){
    stopifnot("Multiple datasets found." = length(dataset) == 1)
    dataset <- dplyr::bind_rows(dataset)
  }
  
  df_final <- checking_process_v2(root =  Sys.getenv("WORK_DIR"), 
                                  COMPLETE_data = dataset)
  df_final <- dplyr::select(df_final, 
                            ACCENUMB, 
                            COLLSITE, 
                            ORIGCTY, 
                            DECLATITUDE, 
                            DECLONGITUDE, 
                            ELEVATION, 
                            INSTCODE, 
                            CROPNAME,
                            SCORE,
                            `LEVEL_OF_IMP` = LI,
                            ROUTE,
                            QUALITY_SCORE = quality_score,
                            ISSUE = issue_txt_desc)
  
  
  
  return(df_final)
}

