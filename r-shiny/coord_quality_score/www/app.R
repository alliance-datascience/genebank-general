suppressMessages(if(!require(pacman)){install.packages("pacman");library(pacman)}else{library(pacman)})
suppressMessages(if(!require(shinyauthr)){install.packages("shinyauthr");library(shinyauthr)}else{library(shinyauthr)})
suppressMessages(if(!require(tidyverse)){install.packages("tidyverse");library(tidyverse)}else{library(tidyverse)})
suppressMessages(if(!require(shinydashboard)){install.packages("shinydashboard");library(shinydashboard)}else{library(shinydashboard)})
suppressMessages(if(!require(shinydashboardPlus)){install.packages("shinydashboardPlus");library(shinydashboardPlus)}else{library(shinydashboardPlus)})
suppressMessages(if(!require(shinyWidgets)){install.packages("shinyWidgets");library(shinyWidgets)}else{library(shinyWidgets)})
suppressMessages(if(!require(shiny)){install.packages("shiny");library(shiny)}else{library(shiny)})
suppressMessages(if(!require(shinyjs)){install.packages("shinyjs");library(shinyjs)}else{library(shinyjs)})
suppressMessages(if(!require(plotly)){install.packages("plotly");library(plotly)}else{library(plotly)})
suppressMessages(if(!require(gtools)){install.packages("gtools");library(gtools)}else{library(gtools)})
suppressMessages(if(!require(glue)){install.packages("glue");library(glue)}else{library(glue)})
suppressMessages(if(!require(htmltools)){install.packages("htmltools");library(htmltools)}else{library(htmltools)})
suppressMessages(if(!require(lubridate)){install.packages("lubridate");library(lubridate)}else{library(lubridate)})
suppressMessages(if(!require(ggpubr)){install.packages("ggpubr");library(ggpubr)}else{library(ggpubr)})
suppressMessages(if(!require(zoo)){install.packages("zoo");library(zoo)}else{library(zoo)})
suppressMessages(if(!require(httr)){install.packages("httr");library(httr)}else{library(httr)})
suppressMessages(if(!require(jsonlite)){install.packages("jsonlite");library(jsonlite)}else{library(stringr)})
suppressMessages(if(!require(stringr)){install.packages("stringr");library(stringr)}else{library(pacman)})
suppressMessages(if(!require(shinythemes)){install.packages("shinythemes");library(shinythemes)}else{library(shinythemes)})
suppressMessages(if(!require(readxl)){install.packages("readxl");library(readxl)}else{library(readxl)})
suppressMessages(if(!require(leaflet)){install.packages("leaflet");library(leaflet)}else{library(leaflet)})
suppressMessages(if(!require(sf)){install.packages("sf");library(sf)}else{library(sf)})
suppressMessages(if(!require(bslib)){install.packages("bslib");library(bslib)}else{library(bslib)})
suppressMessages(if(!require(thematic)){install.packages("thematic");library(thematic)}else{library(thematic)})
suppressMessages(if(!require(stringi)){install.packages("stringi");library(stringi)}else{library(stringi)})


getColor <- function(df) {
  sapply(df$final_score_cats, function(cats) {
    if(cats == "High") {
      "green"
    } else if(cats == "Moderate") {
      "orange"
    } else {
      "red"
    } })
}

check_opt <- function(var){
  if(!is.null(var)){
    if(length(var) == 1){
      if(var == "Todos" | var == "Todas"){var <- NULL}
    }
  }
  return(var)
}

do_filter <- function(db, 
                      instcode = NULL,
                      cropname = NULL,
                      origcty  = NULL
                      
){
  
  
  # if(cliente == "Todos" | cliente == "Todas"){cliente <- NULL}
  # if(sucursal == "Todos"| sucursal == "Todas"){sucursal <- NULL}
  # if(plataforma == "Todos" | plataforma == "Todas"){plataforma <- NULL}
  # if(solicitud == "Todos" | solicitud == "Todas"){solicitud <- NULL}
  # 
  var_list <-  list("INSTCODE" = instcode,
                    "CROPNAME" = cropname,
                    "ORIGCTY"  = origcty)
  
  var_list <- lapply(var_list, check_opt)
  
  are_null <- sapply(var_list, is.null)
  
  if(!all(are_null)){
    av_names <- names(var_list[!are_null])
    
    to_eval <- lapply(av_names, function(nm){
      
      if(nm == "INSTCODE"){
        frml <- paste0("dplyr::filter(", nm, " %in% ", "var_list[['", nm, "']]",")")
      }else{
        frml <- paste0("dplyr::filter(", nm, " == '", var_list[[nm]],"')")
      }
      
      return(frml)
      
    }) %>% unlist %>% 
      paste0( ., collapse = " %>% ") %>% 
      paste0("db %>% ", .)
    
    filtered_db <- eval(parse(text = to_eval))
  }else{
    filtered_db <- db
  }
  
  return(filtered_db)
}




db <- readr::read_csv("./www/genesys_coord_check_to_app.csv") 


# db %>% 
#   dplyr::mutate(marker_col = case_when(
#     final_score_cats == "Low" ~ "#BD0000",
#     final_score_cats == "Moderate" ~ "#E8C900",
#     .default = "#0B960B"
#   ))%>% 
#     write_csv(., "C:/Users/acmendez/Downloads/genesys_coord_check_to_app.csv")

# db %>%
#  dplyr::mutate(popup_text  = paste0(
#    "Institution code: ", INSTCODE, "<br/>",
#    "Crop Name: ", CROPNAME, "<br/>",
#    "Origin Country: ", ORIGCTY, "<br/>",
#    "Accession Number: ", ACCENUMB, "<br/>", 
#    "Status: ",final_score_cats, "<br/>",
#    issue_txt_desc)) %>%
#  write_csv(., "C:/Users/acmendez/Downloads/genesys_coord_check_to_app.csv")


btn_style <- HTML('.btn-light {
    --bs-btn-color: #000;
    --bs-btn-bg: #f8f8f8;
    --bs-btn-border-color: #f8f8f8;
    --bs-btn-hover-color: #000;
    --bs-btn-hover-bg: #d3d3d3;
    border: 1px solid #000000;
    --bs-btn-hover-border-color: #c6c6c6;
    --bs-btn-focus-shadow-rgb: 211,211,211;
    --bs-btn-active-color: #000;
    --bs-btn-active-bg: #c6c6c6;
    --bs-btn-active-border-color: #bababa;
    --bs-btn-active-shadow: inset 0 3px 5px rgba(0,0,0,0.125);
    --bs-btn-disabled-color: #000;
    --bs-btn-disabled-bg: #f8f8f8;
    --bs-btn-disabled-border-color: #f8f8f8;
}


.radiobtn {font-size:12px;height:35px;}
')


ui <- page_navbar(
  theme = bs_theme(preset = "shiny"),
  title = "Viewer",
  collapsible = TRUE,
  inverse = TRUE,
  id = "navbar",
  fillable = "Dashboard",
  nav_panel("Dashboard",
            layout_sidebar(
              border = T,
              fillable = F,
              fill = F,
              sidebar = bslib::sidebar(
                id = "side1",
                width = 300,
                tags$head(tags$style(btn_style)),
                title = img(src="https://alliancebioversityciat.org/sites/default/files/styles/1920_scale/public/images/Alliance%20Logo%20Refresh-color.jpg?itok=aWZGtycl"),
                tags$br(),
                uiOutput("filtros1")
              ),
              tags$h3(tags$b("COORDINATES QUALITY STATUS"), style = "text-align: center;"),
              layout_columns(col_widths  = c(7, 5)
                             ,class = "mt-3"
                             ,row_heights = c(2, 2)
                             ,card(
                               full_screen = T
                               ,card_header(
                                 "Interactive Map"
                               )
                               ,leafletOutput("map1")
                               #reactableOutput("rct1", width = "90%") 
                               
                             )
                             ,card(
                               full_screen = T
                               ,card_header(
                                 "Overall Distribution"
                               )
                               ,card_body( plotlyOutput("gr1", height = "100%"))
                             )
              )
              , layout_columns(col_widths  = c(4, 4, 4)
                               ,class = "mt-3"
                               ,fill = T
                               ,row_heights = c(1, 2)
                               ,card(
                                 full_screen = T
                                 ,card_header(
                                   "Top 5 Low Quality"
                                 )
                                 ,card_body(plotlyOutput("gr2")) 
                               )
                               ,card(
                                 full_screen = T
                                 ,card_header(
                                   "Top 5 Moderate Quality"
                                 )
                                 ,card_body(plotlyOutput("gr3")) 
                               )
                               ,card(
                                 full_screen = T
                                 ,card_header(
                                   "Top 5 High Quality"
                                 )
                                 ,card_body(plotlyOutput("gr4")) 
                               )
                               
              )
              
            )
  )
  
  
)




server <- function(input, output, session){
  
  
  rv_vals        <- reactiveValues()
  rv_vals$db_raw <- db
  ###################
  ### FILTROS UI ###
  #################
  
  output$filtros1 <- renderUI({
    
    inst_code <- unique(rv_vals$db_raw$INSTCODE)
    cropname <- c("Todas",unique(rv_vals$db_raw$CROPNAME))
    origcty  <- c("Todas", unique(rv_vals$db_raw$ORIGCTY ))
    
    
    tagList(
      
      pickerInput(
        inputId = "filt1",
        label = "Institution Code:", 
        choices = inst_code,
        multiple = TRUE,
        selected = inst_code,
        options = pickerOptions(container = "body", 
                                actionsBox = TRUE,
                                liveSearch = TRUE),
        width = "100%"
      ),
      
      tags$br(),
      
      pickerInput(
        inputId  = "filt2",
        label    = "Crop Name:", 
        choices  =  cropname,
        selected = "Todas",
        options  = pickerOptions(container = "body", 
                                 liveSearch = TRUE),
        width    = "100%"
      ),
      
      tags$br(),
      
      pickerInput(
        inputId  = "filt3",
        label    = "Origin Country:", 
        choices  = origcty,
        selected = "Todas",
        options  = pickerOptions(container = "body", 
                                 liveSearch = TRUE),
        width    = "100%"
      )
      
    )
    
  })
  
  ################
  ## observe ####
  ###############
  
  observeEvent(input$filt1, {
    
    
    rv_vals$db_clean <- do_filter(db       = rv_vals$db_raw, 
                                  instcode = input$filt1,
                                  cropname = NULL,
                                  origcty  = NULL )
    
    rv_vals$cropname    <- c("Todas", unique(rv_vals$db_clean$CROPNAME))
    rv_vals$origcty     <- c("Todas", unique(rv_vals$db_clean$ ORIGCTY))
    
    updatePickerInput(inputId  = "filt2",
                      session  = session,
                      choices  = rv_vals$cropname,
                      selected = "Todas")
    
    updatePickerInput(inputId  = "filt3",
                      session  = session,
                      choices  = rv_vals$origcty,
                      selected = "Todas")
    
    
  })
  
  observeEvent(input$filt2, {
    
    rv_vals$db_clean <- do_filter(db       = rv_vals$db_raw, 
                                  instcode = input$filt1,
                                  cropname = input$filt2,
                                  origcty  = NULL )
    
    
    rv_vals$origcty     <- c("Todas", unique(rv_vals$db_clean$ ORIGCTY))
    
    
    updatePickerInput(inputId  = "filt3",
                      session  = session,
                      choices  = rv_vals$origcty,
                      selected = "Todas")
    
    
    
  }, ignoreInit  = F)
  
  observeEvent(input$filt3, {
    
    
    rv_vals$db_clean <- do_filter(db       = rv_vals$db_raw, 
                                  instcode = input$filt1,
                                  cropname = input$filt2,
                                  origcty  = input$filt3 )
    
  })
  
  
  
  #################
  ### Graficos ###
  ###############
  
  output$map1 <- renderLeaflet({
    
    req(rv_vals$db_raw)
    
    markers <- rv_vals$db_raw %>%
      dplyr::filter(DECLATITUDE > -70 | is.na(DECLATITUDE) ) %>% 
      dplyr::filter(check_location_available) %>% 
      dplyr::distinct(DECLATITUDE, DECLONGITUDE, .keep_all = T)
    
    #mytext <-  lapply(markers$popup_text, htmltools::HTML)
    
    
    icons <- awesomeIcons(
      icon = 'ios-information',
      iconColor = 'black',
      library = 'ion',
      markerColor = as.character(getColor(markers))
    )
    
    leaflet(markers) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lat = 0, lng = 0, zoom = 1) %>% 
      addAwesomeMarkers(
        lng = ~markers$DECLONGITUDE,
        lat = ~markers$DECLATITUDE,
        #radius = 5,
        #color = ~markers$marker_col,
        #fillOpacity = 0.8,
        #stroke = T,
        icon = ~icons,
        popup = ~markers$popup_text,
        clusterOptions = markerClusterOptions() 
      )
    
    
    
  })
  
  observeEvent(rv_vals$db_clean, {
    
    
    markers <- rv_vals$db_clean %>%
      dplyr::filter(DECLATITUDE > -70 | is.na(DECLATITUDE) ) %>%
      dplyr::filter(check_location_available) %>% 
      dplyr::distinct(DECLATITUDE, DECLONGITUDE, .keep_all = T)
    
    #mytext <-  lapply(markers$popup_text, htmltools::HTML)
    
    
    icons <- awesomeIcons(
      icon = 'ios-information',
      iconColor = 'black',
      library = 'ion',
      markerColor = as.character(getColor(markers))
    )
    
    
    if(nrow(markers) > 5000){
      
      leafletProxy("map1", data = markers) %>% 
        clearMarkers() %>%
        clearMarkerClusters() %>% 
        addAwesomeMarkers(
          lng = ~markers$DECLONGITUDE,
          lat = ~markers$DECLATITUDE,
          #radius = 5,
          #color = ~markers$marker_col,
          #fillOpacity = 0.8,
          #stroke = T,
          icon = ~icons,
          popup = ~markers$popup_text,
          clusterOptions = markerClusterOptions() 
        )
    }else{
      
      leafletProxy("map1", data = markers) %>% 
        clearMarkers() %>%
        clearMarkerClusters() %>% 
        addAwesomeMarkers(
          lng = ~markers$DECLONGITUDE,
          lat = ~markers$DECLATITUDE,
          #radius = 5,
          #color = ~markers$marker_col,
          #fillOpacity = 0.8,
          #stroke = T,
          icon = ~icons,
          popup = ~markers$popup_text 
        )
    }
    
    
    
    top_5 <- rv_vals$db_clean %>%
      dplyr::select(INSTCODE, final_score_cats, issue_txt_desc) %>%
      as_tibble() %>%
      dplyr::mutate(issues = stringr::str_replace(issue_txt_desc, "Issues found: ", "") %>%
                      stringr::str_replace_all(., ", or both,", "") %>%
                      stringr::str_split(., ",")) 
    
    top_5 <-  lapply(c("Low", "Moderate", "High"), function(vr){
      to_ret <- top_5 %>%
        dplyr::filter(final_score_cats == vr) %>%
        dplyr::pull(issues) %>%
        unlist %>%
        table(.) %>%
        prop.table() %>%
        sort(., decreasing = T)
      
      round(to_ret[1:6]*100, 1)
    })
    names(top_5) <- c("Low", "Moderate", "High")
    
    rv_vals$top_5_lst <- top_5
    
    
    
  })
  
  
  
  output$gr1 <- renderPlotly({
    
    req(rv_vals$db_clean)
    
    to_plot <-  rv_vals$db_clean %>%
      dplyr::mutate(final_score_cats       = factor(final_score_cats, levels = c("High", "Moderate","Low") )) %>% 
      dplyr::group_by(final_score_cats ) %>%
      dplyr::tally() %>%
      dplyr::mutate(text = paste0("Count: ", n)) %>% 
      dplyr::mutate(n = round(n/sum(n)*100, 1))
    
    cols <- c("#0B960B", "#FFC300", "#DE1700")
    
    
    plot_ly(data = to_plot,
            labels =  ~final_score_cats,
            values = ~n,
            text = ~text,
            sort = FALSE,
            marker = list(colors = cols),
            textposition = 'outside'
    ) %>% 
      add_pie(hole = 0.4) %>% 
      layout(title = "",
             showlegend = T,
             
             xaxis = list(showgrid = F, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             uniformtext=list(minsize=8, mode='hide')
      ) %>% 
      style(hoverinfo = 'none')
    
    
  })
  
  output$gr2 <- renderPlotly({
    
    req(rv_vals$top_5_lst)
    
    to_plot<-  data.frame("text" = names(rv_vals$top_5_lst[[1]]), "n" = as.numeric(rv_vals$top_5_lst[[1]]))
    
    to_plot$text <- factor(to_plot$text, levels = (to_plot$text))
    
    plot_ly(to_plot,
            x = ~n,
            y = ~text,
            type = 'funnel',
            texttemplate  = "%{x}%",
            orientation = 'h',
            marker = list(color = ~"#DE1700")) %>%
      layout(bargap = 0.1, barmode = 'overlay',
             xaxis = list(tickmode = 'array', tickvals = c(-1000, -500, 0, 500, 1000),
                          ticktext = c('1000', '500', '0', '500', '1000')),
             yaxis = list(title = ""))
    
    
    
    
  })
  
  
  output$gr3  <- renderPlotly({
    req(rv_vals$top_5_lst)
    
    to_plot<-  data.frame("text" = names(rv_vals$top_5_lst[[2]]), "n" = as.numeric(rv_vals$top_5_lst[[2]]))
    
    to_plot$text <- factor(to_plot$text, levels = (to_plot$text))
    
    plot_ly(to_plot,
            x = ~n,
            y = ~text,
            type = 'funnel',
            texttemplate  = "%{x}%",
            orientation = 'h',
            marker = list(color = ~"#FFC300")) %>%
      layout(bargap = 0.1, barmode = 'overlay',
             xaxis = list(tickmode = 'array', tickvals = c(-1000, -500, 0, 500, 1000),
                          ticktext = c('1000', '500', '0', '500', '1000')),
             yaxis = list(title = ""))
    
    
    
  })
  
  output$gr4  <- renderPlotly({
    req(rv_vals$top_5_lst)
    
    to_plot<-  data.frame("text" = names(rv_vals$top_5_lst[[3]]), "n" = as.numeric(rv_vals$top_5_lst[[3]]))
    
    to_plot$text <- factor(to_plot$text, levels = (to_plot$text))
    
    plot_ly(to_plot,
            x = ~n,
            y = ~text,
            type = 'funnel',
            texttemplate  = "%{x}%",
            orientation = 'h',
            marker = list(color = ~"#0B960B")) %>%
      layout(bargap = 0.1, barmode = 'overlay',
             xaxis = list(tickmode = 'array', tickvals = c(-1000, -500, 0, 500, 1000),
                          ticktext = c('1000', '500', '0', '500', '1000')),
             yaxis = list(title = ""))
    
    
    
  })
  
  
  
}#END SERVER









