#'@author Andres Camilo Mendez
#' Scrape worldwide roads shapefile from Openstreet map satellite data
#' 2024
#' 
require(pacman)
pacman::p_load(rvest, tidyverse)


#' Function to scrape links from OPS roads shapefile webpage
#' NIC and ECU are not available for downloading
#' @param out_dir (character): full directory path to download files
#' @param main_page_url (character): url of webpage
#' @return country road shapefile
scrape_OPS <- function(out_dir, main_page_url){
  
  content_main <- rvest::read_html(main_page_url)
  
  continents <- content_main %>%
    rvest::html_elements("table") %>% 
    rvest::html_elements( xpath = '//table[@id="subregions"]//a[contains(@href, ".html")]') %>% 
    rvest::html_attr("href")
  
  continents <- continents[!grepl("antarctica", continents)]
  
  
  for(cnt in continents){
    cat("> Scrapping links for: ",cnt, "\n")
    continents_url <- paste(main_page_url, cnt, sep = "/")
    continents_url <- continents_url[!grepl("antarctica", continents_url)]
    
    all_countries_nms <- rvest::read_html(continents_url) %>% 
      rvest::html_elements(xpath = '//*[not(div[@id="details"])]//table[@id="subregions"]//a[contains(@href, ".html")]') %>% 
      rvest::html_attr("href") %>% 
      basename(.) %>% 
      stringr::str_replace(string = ., pattern = ".html", replacement = "")
    
    country_urls <- rvest::read_html(continents_url) %>% 
      rvest::html_elements(xpath = '//*[not(div[@id="details"])]//table[@id="subregions"]//a[contains(@href, ".shp")]')%>% 
      rvest::html_attr("href") 
    
    country_shp_av <- str_replace(basename(country_urls), "-latest-free.shp.zip", "")
    
    not_av_cnt <- all_countries_nms[!all_countries_nms %in% country_shp_av]
    if(cnt == "europe.html"){
      not_av_cnt <- not_av_cnt[!grepl("russia", not_av_cnt)]
    }
    
    if(length(not_av_cnt) != 0){
      
      url_not_av <- paste(main_page_url, str_replace(cnt, ".html", ""),  paste0(not_av_cnt, ".html"), sep ="/")
      url_not_av[grepl("russia", url_not_av)] <- gsub("asia/", "", url_not_av[grepl("russia", url_not_av)])
      
      for(url in url_not_av){
        country_urls_plus <- rvest::read_html(url) %>% 
          rvest::html_elements(xpath = '//*[not(div[@id="details"])]//table[@id="subregions"]//a[contains(@href, ".shp")]') %>%  
          rvest::html_attr("href") 
        
        subregion_shp_av <- str_replace(basename(country_urls_plus), "-latest-free.shp.zip", "")
        
        all_subregions_nms <-  rvest::read_html(url) %>% 
          rvest::html_elements(xpath = '//*[not(div[@id="details"])]//table[@id="subregions"]//a[contains(@href, ".html")]') %>% 
          rvest::html_attr("href") %>% 
          basename(.) %>% 
          stringr::str_replace(string = ., pattern = ".html", replacement = "")
        
        not_av_subregion <- all_subregions_nms[!all_subregions_nms %in% subregion_shp_av]
        
        subregion_new_url <- c()
        if(length(not_av_subregion) != 0){
          
          for(sr_url in not_av_subregion){
            
            url_not_av_sr <- paste(str_replace(url, ".html", ""),  paste0(sr_url, ".html"), sep ="/")

            subregion_urls_plus <- rvest::read_html(url_not_av_sr) %>% 
              rvest::html_elements(xpath = '//*[not(div[@id="details"])]//table[@id="subregions"]//a[contains(@href, ".shp")]') %>%  
              rvest::html_attr("href") 
            
            subregion_urls_plus <- subregion_urls_plus[grepl("/", subregion_urls_plus)]
            
            if(length(subregion_urls_plus) !=0){
              subregion_urls_plus <-  paste(gsub(".html", "", cnt), gsub(".html", "", basename(url)) ,subregion_urls_plus , sep = "/")
              subregion_new_url <- c(subregion_new_url, subregion_urls_plus)
            }
              
          }
        }
        
        
        
        new_urls <- paste(gsub(".html", "", cnt), country_urls_plus[grepl("/", country_urls_plus)], sep = "/")
        new_urls[grepl("russia", new_urls)] <- gsub("asia/", "", new_urls[grepl("russia", new_urls)])
        
        country_urls <- c(country_urls, new_urls, subregion_new_url)
      }
      
    }
    
    write.csv(data.frame("urls" = country_urls), paste(out_dir, paste0(gsub(".html", "", cnt),"_OSM_urls.csv"), sep = "/"), row.names = F)
    
    for(url in country_urls){
      cat(">>>> Downloading data for: ", url, "\n")
      country_name <- gsub("-latest-free.shp.zip", "" , basename(url))  
      url_download <- paste(main_page_url, url, sep="/")
      
      out_cuntry_dir <- paste(out_dir,gsub(".html", "", cnt) ,country_name, sep = "/")
      out_zip_filename <- paste0(out_cuntry_dir, "/", country_name, ".zip")
      
      if(!dir.exists(out_cuntry_dir)){dir.create(out_cuntry_dir, recursive = T)}
      
      if(length(list.files(out_cuntry_dir)) == 0){
        
        download.file(url_download, out_zip_filename, method = "curl")
        out_filename <- paste0(out_cuntry_dir, "/", country_name, ".zip")
        fl_nm <- utils::unzip(out_zip_filename,list = TRUE )$Name
        fl_nm <- fl_nm[grepl("roads_free", fl_nm)]
        
        utils::unzip(out_zip_filename, 
                     exdir = out_cuntry_dir,
                     files = fl_nm,
                     overwrite = T)
        
        file.remove(out_zip_filename)
        
        Sys.sleep(sample(3:10, 1))
      }else{
        cat(">>>> file already downloaded, skiping it... \n")
      }
      
      
      
    }
    
    
    
    
  }
  
  
}#end function



scrape_OPS(out_dir       = "//alliancedfs.alliance.cgiar.org/cimmyt-aidi$/OSM_roads",
           main_page_url = "https://download.geofabrik.de")








gmaps <- readRDS("D:/OneDrive - CGIAR/Genebanks/data/geocode_info.rds")

df_ginfo <- lapply(gmaps, function(lst){
  
  # data.frame(formatted_addres = lst$formatted_address,
  #            admon_level = sapply(lst$types, paste0, collapse = "-"),
  #            id = lst$id)
  
  if(is.null(lst$message)){
    to_ret <- dplyr::bind_rows(lst$address_components) %>% 
      dplyr::mutate(types = purrr::map(types, paste0, collapse = "-"),
                    types = unlist(types)) %>% 
      dplyr::distinct() %>%
      dplyr::filter(!types %in% c("plus_code", "postal_code", "route")) %>% 
      dplyr::mutate(order = stringr::str_extract(types, "administrative_area_level_[0-9]{1}"),
                    order = ifelse(is.na(order), 999, order),
                    order = stringr::str_extract(order, "[0-9]{1}"),
                    id  = unique(lst$id)) %>% 
      dplyr::arrange(order) %>% 
      dplyr::select(long_name, short_name, admon_level = types, id )
  }else{
    to_ret <- data.frame(long_name = NA, short_name = NA, admon_level = NA, id = unique(lst$id))
  }
  
  return(to_ret)
})


do.call(rbind, df_ginfo)











