# Get Genesys occurrence data for distribution analysis 
# updated by Maria Victoria Diaz and K. de Sousa 

#..........................................
#..........................................

# Packages ####
library(genesysr)
library(data.table)
library(readxl)
library(here)

set_here()

# Read institution codes

inst <- read.csv(here("docs","institutions_cgiar.csv"), header = T, sep = ",")



# Login the Genesys API


setup_production()
user_login()


# Define the function to extract passport data from the institute using the API

extract_inst <- function(inst){
  
  cat(inst, "\n")
  
  
  passport <- download_mcpd(inst)
  
  data <- read_excel(passport, sheet = 1)
  
  
  if(nrow(data)>0){
    
    write.csv(data, here("data", paste0(gsub(".xlsx", ".csv",passport))), row.names = F)
    
  }
  
  file.remove(passport)
  
  return(data)
  
}






extract_all <- function(inst){
  
  
  # Applying the function extract_inst to every institute in the list 
  
  datas <- lapply(inst$INSTCODE, extract_inst)
  
  datas <- do.call(rbind, datas)
  
  
  # Saving raw data 
  
  write.csv(datas, here('data', 'raw','genesys_downloaded_institutions_data.csv'), row.names = F)
  
}




extract_all(inst)
