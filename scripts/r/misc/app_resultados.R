library(shinyauthr)
library(tidyverse)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shiny)
library(shinyjs)
library(odbc)
library(classInt)
library(plotly)
library(gtools)
library(glue)
library(htmltools)
library(DescTools)
library(shinythemes)
library(highcharter)
library(data.table)
library(DT)
library(lubridate)
library(ggpubr)
library(openxlsx)
library(zoo)
library(RColorBrewer)
library(scales)
library(V8)
library(httr)
library(jsonlite)
library(magrittr)
library(base64enc)
library(RMySQL)
library(reactable)
library(DBI)
library(stringr)
library(shinythemes)
library(readxl)
library(leaflet)
library(sf)
library(bslib)
library(thematic)
library(googleVis)
library(stringi)


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




db <- readr::read_csv("C:/Users/acmendez/Downloads/genesys_coord_check_to_app.csv") 


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
  
  output$rct1 <- renderReactable({

    req(rv_vals$db_clean)
    reacTh <- reactableTheme(
      # text_color <- "hsl(0, 0%, 95%)",
      # text_color_light <- "hsl(0, 0%, 70%)",
      # text_color_lighter <- "hsl(0, 0%, 55%)",
      # color = "hsl(233, 9%, 87%)",
      # backgroundColor = "hsl(233, 9%, 19%)",
      # borderColor = "hsl(233, 9%, 22%)",
      # stripedColor = "hsl(233, 12%, 22%)",
      highlightColor = "hsl(0, 0%, 77%)",
      inputStyle = list(backgroundColor = "hsl(213, 4%, 63%)"),
      filterInputStyle =  list(backgroundColor = "white", color = "hsl(213, 4%, 2%)", height = "20px"),
      # selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      # pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      # pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
    )
    #
    tbl <- rv_vals$db_clean  %>%
      dplyr::filter(ESTADO_PEDIDO == "EN PROCESO") %>%
      dplyr::group_by(CLIENTE, ESTADO_PEDIDO) %>%
      dplyr::count() %>%
      dplyr::arrange(desc(n)) %>%
      dplyr::rename(`Recuento de NUMERO PEDIDO` = n)

    if(nrow(tbl) == 0){
      tbl <- data.frame(CLIENTE  = 0,ESTADO_PEDIDO = 0, NUMERO_PEDIDOS = 0 )
    }

    ttl <- sum(tbl$`Recuento de NUMERO PEDIDO`)

    reactable(tbl,
              sortable  = T,
              filterable = F,
              theme = reacTh,
              columns = list(
                CLIENTE = colDef(
                  minWidth = 200,
                  footer = "Total",
                  footerStyle = list(fontWeight = "bold")
                ),
                ESTADO_PEDIDO = colDef(
                  minWidth = 100,
                ),
                `Recuento de NUMERO PEDIDO` = colDef(
                  minWidth = 100,
                  footer = ttl,
                  footerStyle = list(fontWeight = "bold")
                )
              ),
              language = reactableLang(
                filterPlaceholder = "Filtrar",
                noData = "No encontrado",
                pageInfo = "",#"{rowStart}\u2013{rowEnd} de {rows} Registros",
                pagePrevious = "\u276e",
                pageNext = "\u276f",
              ),
              defaultPageSize  = 5,
              paginationType = "simple",
              pagination = F,
              height = 250,
              highlight = TRUE,
              striped = TRUE,
              bordered = TRUE
    )#end reactable

  })

  output$rct2 <- renderReactable({

    req(rv_vals$db_clean)
    reacTh <- reactableTheme(
      # text_color <- "hsl(0, 0%, 95%)",
      # text_color_light <- "hsl(0, 0%, 70%)",
      # text_color_lighter <- "hsl(0, 0%, 55%)",
      # color = "hsl(233, 9%, 87%)",
      # backgroundColor = "hsl(233, 9%, 19%)",
      # borderColor = "hsl(233, 9%, 22%)",
      # stripedColor = "hsl(233, 12%, 22%)",
      highlightColor = "hsl(0, 0%, 77%)",
      inputStyle = list(backgroundColor = "hsl(213, 4%, 63%)"),
      filterInputStyle =  list(backgroundColor = "white", color = "hsl(213, 4%, 2%)", height = "15px"),
      # selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      # pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      # pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
    )

    tbl <- rv_vals$db_clean %>%
      dplyr::filter(ESTADO_PEDIDO == "EN PROCESO") %>%
      dplyr::group_by(NUMERO_PEDIDO, ESTADO_PEDIDO) %>%
      dplyr::reframe( `Recuento de NUMERO PEDIDO` = n(),
                      `Suma de CANTIDAD DE LINEAS` = sum(CANTIDAD_LINEAS_PEDIDO))


    if(nrow(tbl) == 0){
      tbl <- data.frame(NUMERO_PEDIDO  = "", ESTADO_PEDIDO = "", `Recuento de NUMERO PEDIDO` = 0,
                        `Suma de CANTIDAD DE LINEAS` = 0)
    }

    ttl1 <- sum(tbl$`Recuento de NUMERO PEDIDO`)
    ttl2 <- sum(tbl$`Suma de CANTIDAD DE LINEAS`)

    reactable(tbl,
              sortable  = T,
              filterable = F,
              theme = reacTh,
              columns = list(
                NUMERO_PEDIDO = colDef(
                  filterable = TRUE,
                  minWidth = 100,
                  footer = "Total",
                  footerStyle = list(fontWeight = "bold")
                ),
                ESTADO_PEDIDO = colDef(
                  minWidth = 80,
                ),
                `Recuento de NUMERO PEDIDO` = colDef(
                  minWidth = 100,
                  footer = ttl1,
                  footerStyle = list(fontWeight = "bold")
                ),
                `Suma de CANTIDAD DE LINEAS` = colDef(
                  minWidth = 100,
                  footer = ttl2,
                  footerStyle = list(fontWeight = "bold")
                )
              ),
              language = reactableLang(
                filterPlaceholder = "Filtrar",
                noData = "No encontrado",
                pageInfo = "",#"{rowStart}\u2013{rowEnd} de {rows} Registros",
                pagePrevious = "\u276e",
                pageNext = "\u276f",
              ),
              defaultPageSize  = 5,
              paginationType = "simple",
              pagination = F,
              height = 300,
              highlight = TRUE,
              striped = TRUE,
              bordered = TRUE
    )#end reactable

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
      add_pie(hole = 0.8) %>% 
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


app <- shiny::shinyApp(ui, server)
runApp(app)









