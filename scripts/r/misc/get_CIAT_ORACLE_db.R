require(pacman)
pacman::p_load(DBI, RJDBC, rJava, tidyverse)


get_db <- function(
    classPath = NULL,
    dsn_url      = NULL,
    dsn_uid      = NULL ,
    dsn_pwd      = NULL,  
    consulta     = NULL){
  
  db <-  tryCatch({
    
    jdbcDriver <- RJDBC::JDBC("oracle.jdbc.OracleDriver", 
                              classPath = classPath)
    
    jdbcConnection <- DBI::dbConnect(jdbcDriver,
                                     dsn_url,
                                     dsn_uid,
                                     dsn_pwd )
    
    db <- DBI::dbGetQuery(jdbcConnection,
                          consulta) 
    
  },
  error=function(cond) {
    print("Unable to connect to Database.")
    return(NA)
  })
  
  
  return(db)
  
}


db <- get_db(classPath    = "D:/OneDrive - CGIAR/Documents/ojdbc11.jar",
             dsn_url      = "jdbc:oracle:thin:@kappa.ciat.cgiarad.org:1521/ciat",
             dsn_uid      = "URGANALISTA" ,
             dsn_pwd      = "All$URG#24*",  
             consulta     = "SELECT * FROM BPRE_PASAPORTES")

db %>% 
  write_csv(., "C:/Users/acmendez/Downloads/ORALCE_CIAT_beans.csv")







