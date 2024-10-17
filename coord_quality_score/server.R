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
suppressMessages(if(!require(reactable)){install.packages("reactable");library(reactable)}else{library(reactable)})
suppressMessages(if(!require(DBI)){install.packages("DBI");library(DBI)}else{library(DBI)})
suppressMessages(if(!require(RSQLite)){install.packages("RSQLite");library(RSQLite)}else{library(RSQLite)})
suppressMessages(if(!require(shiny.i18n)){install.packages("shiny.i18n");library(shiny.i18n)}else{library(shiny.i18n)})


translator <- Translator$new(translation_csvs_path = "./www/")


getColor <- function(df) {
  sapply(df$quality_score, function(cats) {
    if(cats == "Low") {
      "red"
    } else if(cats == "Moderate") {
      "orange"
    } else {
      "green"
    } })
  
  
}

check_opt <- function(var){
  if(!is.null(var)){
    if(length(var) == 1){
      if(var == "Todos" | var == "Todas" | var == "All"){var <- NULL}
    }
  }
  return(var)
}

do_filter <- function(db_conn, 
                      instcode = NULL,
                      cropname = NULL,
                      origcty  = NULL,
                      markets   = NULL
){
  
  var_list <-  list("INSTCODE" = instcode,
                    "CROPNAME" = cropname,
                    "ORIGCTY"  = origcty,
                    "check_market_acc" = markets)
  
  var_list <- lapply(var_list, check_opt)
  
  are_null <- sapply(var_list, is.null)
  
  if(!all(are_null)){
    av_names <- names(var_list[!are_null])
    
    to_eval <- lapply(av_names, function(nm){
      
      qry <- paste0(nm, " = '",  var_list[[nm]],"'")
      #frml <- paste0("dplyr::filter(", nm, " == '", var_list[[nm]],"')")
      
      
      return(qry)
      
    }) %>% unlist %>% 
      paste0( ., collapse = " AND ") %>% 
      paste0(.,";")
      #%>% paste0("db %>% ", .)
    full_query = paste0("SELECT * FROM quality_score_sql WHERE ", to_eval)
    #filtered_db <- eval(parse(text = to_eval))
    print("cargue la BD")
    filtered_db <- DBI::dbGetQuery(
      conn = db_conn,
      statement = full_query
    )
  }else{
    stop("cargue toda la BD")
    filtered_db <- DBI::bGetQuery(
      conn = db_conn,
      statement =  "SELECT * FROM quality_score_sql"
    )
  }
  
  return(filtered_db)
}


filter_issue <- function(txt_vec, target){
  #txt_vec <- c("Missing COLLSITE", 'Georeferenced to a centroid')
  txt_vec <- unlist(txt_vec)
  txt_vec <- sapply(txt_vec, gsub, pattern = "\\(", replacement = "\\\\(")
  txt_vec <- sapply(txt_vec, gsub, pattern = "\\)", replacement = "\\\\)")
  # target <- c("Missing Coordinates; Missing COLLSITE; Georeferenced to a centroid", 
  #             "Missing COLLSITE; Georeferenced to a centroid",
  #             "Georeferenced to a centroid; Missing Country admon level 1 in COLLSITE")

  to_ret  = sapply(str_match_all(target, paste0(txt_vec, collapse = "|")), length) == length(txt_vec)
  
  return(to_ret)
  
}


db_conn <- DBI::dbConnect(drv = RSQLite::SQLite(), "./www/genesys_coord_check_to_app_new.sqlite")#read.csv("./www/genesys_coord_check_to_app_new.csv", header = T)
inst_av <- DBI::dbGetQuery(
  conn = db_conn,
  statement = "SELECT DISTINCT INSTCODE FROM quality_score_sql;"
) %>% unlist(., recursive = T, use.names = F)

shiny::onStop(function() {
  dbDisconnect(db_conn)
})

server <- function(input, output, session){
  
  ####################
  ### LANGUAGE ######
  ##################
  
  
  trs <- reactive({
    selected <- input$lan
    
    if (length(selected) > 0 && selected %in% translator$get_languages()) {
      translator$set_translation_language(selected)
    }
    
    translator
  })
  
  observeEvent(input$lan, {
    # This print is just for demonstration
    print(paste("Language change!", input$lan))
    # Here is where we update language in session
    shiny.i18n::update_lang(input$lan)
  })
  
  rv_vals        <- reactiveValues()
  #rv_vals$db_raw <- db
  ###################
  ### FILTROS UI ###
  #################
  
  output$filtros1 <- renderUI({
    
   
    inst_code <- inst_av
    cropname  <- c("All")
    origcty   <- c("All")
    markets   <- c("All","Non market", "Market")
    
    tagList(
      
      pickerInput(
        inputId = "filt1",
        label = "Institution Code:", 
        choices = inst_code,
        multiple = F,
        selected = "COL003",
        options = pickerOptions(container = "body", 
                                liveSearch = TRUE),
        width = "100%"
      ),
      
      tags$br(),
      
      pickerInput(
        inputId  = "filt2",
        label    = "Crop Name:", 
        choices  =  cropname,
        selected = "All",
        options  = pickerOptions(container = "body", 
                                 liveSearch = TRUE),
        width    = "100%"
      ),
      
      tags$br(),
      
      pickerInput(
        inputId  = "filt3",
        label    = "Origin Country:", 
        choices  = origcty,
        selected = "All",
        options  = pickerOptions(container = "body", 
                                 liveSearch = TRUE),
        width    = "100%"
      ),
      
      tags$br(),
      
      pickerInput(
        inputId  = "filt4",
        label    = "Markets:", 
        choices  = markets,
        selected = "All",
        width    = "100%"
      )
      
    )
    
  })
  
  ################
  ## observe ####
  ###############
  
  observeEvent(input$filt1, {

    rv_vals$db_clean <- do_filter(db_conn  = db_conn, 
                                  instcode = input$filt1,
                                  cropname = NULL,
                                  origcty  = NULL,
                                  markets  = NULL)
    
    rv_vals$cropname    <- c("All", unique(rv_vals$db_clean$CROPNAME))
    rv_vals$origcty     <- c("All", unique(rv_vals$db_clean$ORIGCTY))
    rv_vals$markets     <- c("All", unique(rv_vals$db_clean$check_market_acc))
    
    updatePickerInput(inputId  = "filt2",
                      session  = session,
                      choices  = rv_vals$cropname,
                      selected = "All")
    
    updatePickerInput(inputId  = "filt3",
                      session  = session,
                      choices  = rv_vals$origcty,
                      selected = "All")
    
    updatePickerInput(inputId  = "filt4",
                      session  = session,
                      choices  = rv_vals$market,
                      selected = "All")
  
    updatePickerInput(inputId  = "filt_issue",
                      session  = session,
                      selected = character(0))
    
  })
  
  observeEvent(input$filt2, {

    if(input$filt2 != "All"){
      
    rv_vals$db_clean <- do_filter(db_conn  = db_conn, 
                                  instcode = input$filt1,
                                  cropname = input$filt2,
                                  origcty  = NULL,
                                  markets  = NULL )
    
    }
    
    rv_vals$origcty     <- c("All", unique(rv_vals$db_clean$ ORIGCTY))
    rv_vals$markets     <- c("All", na.omit(unique(rv_vals$db_clean$check_market_acc)))
    
    
    updatePickerInput(inputId  = "filt3",
                      session  = session,
                      choices  = rv_vals$origcty,
                      selected = "All")
    
    updatePickerInput(inputId  = "filt4",
                      session  = session,
                      choices  = rv_vals$market,
                      selected = "All")
    
    updatePickerInput(inputId  = "filt_issue",
                      session  = session,
                      selected = character(0))
    
  }, ignoreInit  = T)
  
  observeEvent(input$filt3, {

    if(input$filt3 != "All"){
    rv_vals$db_clean <- do_filter(db_conn  = db_conn, 
                                  instcode = input$filt1,
                                  cropname = input$filt2,
                                  origcty  = input$filt3,
                                  markets  = NULL)
    }
    rv_vals$markets     <- c("All", na.omit(unique(rv_vals$db_clean$check_market_acc)))
    
    
    updatePickerInput(inputId  = "filt4",
                      session  = session,
                      choices  = rv_vals$market,
                      selected = "All")
    
    updatePickerInput(inputId  = "filt_issue",
                      session  = session,
                      selected = character(0))
    
    
  }, ignoreInit  = T)
  
  observeEvent(input$filt4, {

    if(input$filt4 != 'All'){
    rv_vals$db_clean <- do_filter(db_conn  = db_conn, 
                                  instcode = input$filt1,
                                  cropname = input$filt2,
                                  origcty  = input$filt3,
                                  markets  = input$filt4)
    }
    
    updatePickerInput(inputId  = "filt_issue",
                      session  = session,
                      selected = character(0))
  
    
  }, ignoreInit = T)
  
  
# ValueBoxs ---------------------------------------------------------------

  output$vbox1 <- renderUI({
    req(rv_vals$db_clean)
    nrow(rv_vals$db_clean) %>% 
      format(., nsmall=1, big.mark=",")
  })
  
  output$vbox2 <- renderUI({
    req(rv_vals$db_clean)
    
    rv_vals$db_clean %>% 
      dplyr::filter(check_location_available == 0) %>% 
      nrow(.) %>% 
      format(., nsmall=1, big.mark=",")
    
  })
  
  output$vbox3 <- renderUI({
    req(rv_vals$db_clean)
    
    denom <- nrow(rv_vals$db_clean)
    num <- rv_vals$db_clean %>% 
      dplyr::filter(check_location_available == 0) %>% 
      nrow(.)
    
    round((num/denom)*100, 1)
  })  
  
  #################
  ### Graficos ###
  ###############
  
  output$map1 <- renderLeaflet({
    
    req(rv_vals$db_clean)
    
    markers <- rv_vals$db_clean %>%
      dplyr::filter(DECLATITUDE > -70 | is.na(DECLATITUDE) ) %>% 
      dplyr::filter(check_location_available == 1) %>% 
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
        #icon = ~icons,
        popup = ~markers$popup_text,
        clusterOptions = markerClusterOptions(
          iconCreateFunction  = JS("
      function(cluster) {
        var childCount = cluster.getChildCount();

		var c = ' marker-cluster-';
		if (childCount < 10) {
			c += 'small';
		} else if (childCount < 100) {
			c += 'medium';
		} else {
			c += 'large';
		}

		return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster marker-cluster-acm', iconSize: new L.Point(40, 40) });
      }
    ")
          ) 
      )
    
    
    
  })
  
  output$map2 <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lat = 0, lng = 0, zoom = 1) 
    
  })
  
  observeEvent(rv_vals$db_clean, {
    
    rv_vals$db_clean <- rv_vals$db_clean %>%
      dplyr::mutate(
        marker_col = case_when(
        quality_score == "High" ~ "#0B960B",
        quality_score == "Moderate" ~ "#E8C900",
        .default = "#BD0000"
      ),
      mosaic_color  = case_when(
        LI == "Hard" ~ "#bb004b",
        LI == "Moderate" ~ "#eab301",
        LI == "Easy" ~ "#7ed600",
        .default = "#a7a7a7"
      ),
      popup_text  = paste0(
        "<b>Institution code: </b>", INSTCODE, "<br/>",
        "<b>Crop Name: </b>", CROPNAME, "<br/>",
        "<b>Origin Country: </b>", ORIGCTY, "<br/>",
        "<b>Accession Number: </b>", ACCENUMB, "<br/>",
        "<b>Score Number:</b>", SCORE, "<br/>",
        "<b>Status: </b>", paste0(quality_score, " quality"), "<br/>",
        "<b>Level of improvement: </b>", paste0(ifelse(LI == "Completed", "No action required", LI), " to curate"), "<br/>",
        paste0("<b>Issues found: </b>", issue_txt_desc) ))
    
    rv_vals$tbl <- rv_vals$db_clean %>% 
      dplyr::mutate(quality_score = paste0("<b style = 'color:", marker_col, "'>", quality_score, "</b>"),
                    LI = paste0("<b style = 'color:", mosaic_color, "'>", LI, "</b>"),
                    check_slope_suggested = round(check_slope_suggested, 0)) %>% 
      dplyr::select(ACCENUMB, 
                    COLLSITE, 
                    CROPNAME, 
                    DECLONGITUDE, 
                    DECLATITUDE,
                    ORIGCTY,
                    GADM_ISO3 = GADM_GID_0,
                    ELEVATION,
                    STRM_ELEV = check_elev_suggested,
                    SLOPE_DEGREE = check_slope_suggested,
                    QUALITY_SCORE = quality_score,
                    LEVEL_OF_IMPROVEMENT = LI,
                    issue_txt_desc)

    markers <- rv_vals$db_clean %>%
      dplyr::filter(DECLATITUDE > -70 | is.na(DECLATITUDE) ) %>%
      dplyr::filter(check_location_available == 1) %>% 
      dplyr::distinct(DECLATITUDE, DECLONGITUDE, .keep_all = T)
    #mytext <-  lapply(markers$popup_text, htmltools::HTML)
    
  
    
    icons <- awesomeIcons(
      icon = 'ios-information',
      iconColor = 'black',
      library = 'ion',
      markerColor = as.character(getColor(markers))
    )
    
    
    if(nrow(markers) > 1500){
      
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
          clusterOptions = markerClusterOptions(
            iconCreateFunction = JS("
      function(cluster) {
        var childCount = cluster.getChildCount();

		var c = ' marker-cluster-';
		if (childCount < 10) {
			c += 'small';
		} else if (childCount < 100) {
			c += 'medium';
		} else {
			c += 'large';
		}

		return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster marker-cluster-acm', iconSize: new L.Point(40, 40) });
      }
    ")
          ) 
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
    
     
    
  }, priority = 100)
  
  observeEvent(input$filt_issue, {
    
    req(rv_vals$db_clean)
    
    print(input$filt_issue)
    
    rv_vals$tbl <- rv_vals$db_clean %>% 
      dplyr::mutate(quality_score = paste0("<b style = 'color:", marker_col, "'>", quality_score, "</b>"),
                    LI = paste0("<b style = 'color:", mosaic_color, "'>", LI, "</b>"),
                    check_slope_suggested = round(check_slope_suggested, 0)) %>% 
      dplyr::select(ACCENUMB, 
                    COLLSITE, 
                    CROPNAME, 
                    DECLONGITUDE, 
                    DECLATITUDE,
                    ORIGCTY,
                    GADM_ISO3 = GADM_GID_0,
                    ELEVATION,
                    STRM_ELEV = check_elev_suggested,
                    SLOPE_DEGREE = check_slope_suggested,
                    QUALITY_SCORE = quality_score,
                    LEVEL_OF_IMPROVEMENT = LI,
                    issue_txt_desc)
    
    if(!is.null(input$filt_issue)){
      rv_vals$tbl <- rv_vals$tbl %>% 
        dplyr::filter(filter_issue(input$filt_issue, issue_txt_desc))
    
    }
    
    
    
    
  }, ignoreNULL = F)
  
  
  output$gr1 <- renderPlotly({
    
    req(rv_vals$db_clean)
    
    to_plot <-  rv_vals$db_clean %>%
      dplyr::mutate(quality_score       = factor(quality_score, levels = c("High", "Moderate","Low") )) %>% 
      dplyr::group_by(quality_score ) %>%
      dplyr::tally() %>%
      dplyr::mutate(text = paste0("Count: ", n)) %>% 
      dplyr::mutate(n = round(n/sum(n)*100, 1))
    
    cols <- dplyr::case_when(
      to_plot$quality_score == "High" ~ "#0B960B",
      to_plot$quality_score ==  "Moderate"  ~ "#FFC300",
      .default = "#DE1700")
    
    plot_ly(data = to_plot,
            labels =  ~quality_score,
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
    req(rv_vals$db_clean)
    
    df = rv_vals$db_clean %>%
      dplyr::mutate(quality_score = factor(quality_score , levels = c("High", "Moderate", "Low")),
                    LI = ifelse(LI == "Completed", "Not necessary", LI))  %>% 
      dplyr::group_by(quality_score, LI) %>% 
      count(.drop = F) %>%
      dplyr::ungroup()
    
    if(length(unique(df$quality_score)) != 3 | length(unique(df$LI)) != 4){
      
      cat_ql <- c("High", "Moderate", "Low")
      cat_LI <- c("Not necessary", "Easy", "Moderate", "Hard")
      
      
      cat_ql <- cat_ql[! cat_ql %in% unique(df$quality_score)]
      cat_LI <- cat_LI[! cat_LI %in% unique(df$LI)]
      
      to_add = expand.grid(cat_ql, cat_LI)
      names(to_add) = c("quality_score", "LI")
      to_add$n = 0
      
      df <- df %>% 
        bind_rows(to_add)
    }
    
    df <- df %>% 
      tidyr::pivot_wider(., names_from = quality_score, values_from = n) %>% 
      dplyr::mutate(across(everything(.), function(i){ifelse(is.na(i), 0, i)}))  %>% 
      dplyr::mutate(High_perc = round(High/sum(High)*100, 1),
                    Moderate_perc = round(Moderate/sum(Moderate)*100, 1),
                    Low_perc  = round(Low/sum(Low)*100, 1),
                    LI = factor(LI, levels = c("Hard", "Moderate", "Easy", "Not necessary"))) %>% 
      dplyr::mutate(across(where(is.numeric), function(i){ifelse(is.nan(i), 0, i)})) %>% 
      dplyr::arrange(LI) %>% 
      tibble::column_to_rownames(var = "LI")
    
    
    
    
    widths <- colSums(df[, !grepl("_perc" , names(df))])
    marker_colors <- c('Hard' = "#ff0000", 'Moderate' = "#eab301", 'Easy' = "#7ed600",
                       "Not necessary" = "#a7a7a7")
    
    
    fig1 <- plot_ly()
    
    for (idx in rownames(df)) {
      dff <- df[idx, ]
      fig1 <- fig1 %>%
        add_trace(
          x = cumsum(widths) - widths,
          y = as.numeric(dff[grepl("_perc" , names(df))]),
          width = widths,
          marker = list(color = marker_colors[idx]),
          text = sprintf('%.1f%%', as.numeric(dff[grepl("_perc" , names(df))])),
          name = idx,
          type = 'bar',
          offset = 0
        )
    }
    
    
     fig1 %>%
      layout(
        barmode = 'stack',
        xaxis = list(
          tickvals = cumsum(widths) - widths/2,
          ticktext = names(df[, !grepl("_perc" , names(df))]),
          title = "Quality"
        ),
        yaxis = list(range = c(0, 100),
                     title = "Percentage (%)"),
        legend = list(title = list(text = "<b> Level of improvement </b>"))
      )
    
  })
  

  
  output$rt1 <- reactable::renderReactable({
    
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
      filterInputStyle =  list(backgroundColor = "hsl(213, 4%, 84%)", color = "hsl(213, 4%, 2%)"),
      selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      rowSelectedStyle = list(backgroundColor = "#ffff77", boxShadow = "inset 2px 0 0 0 #ffa62d")
      # pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      # pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")  #b5b5b5
    )
    
     
    reactable(rv_vals$tbl %>% 
                dplyr::select(-issue_txt_desc,
                              -ELEVATION,
                              -STRM_ELEV,
                              -SLOPE_DEGREE), 
              paginationType = "simple",
              selection = "single",
              pagination = T,
              onClick = "select",
              wrap = F,
              #defaultSorted = list(id = "desc"), 
              theme = reacTh,
              columns = list(
                ACCENUMB = colDef(
                  resizable = F,
                  filterable = TRUE
                ),
                DECLATITUDE = colDef(
                  na = "-"
                ),
                DECLONGITUDE = colDef(
                  na = "-"
                ),
                COLLSITE = colDef(
                  resizable = TRUE,
                  filterable = F
                ),
                GADM_ISO3 = colDef(
                  resizable = T,
                  filterable = F
                ),
                QUALITY_SCORE = colDef(
                  sortable = TRUE,
                  resizable = T,
                  filterable = TRUE,
                  html = T
                ),
                LEVEL_OF_IMPROVEMENT = colDef( 
                  resizable = T,
                  filterable = TRUE,
                  sortable = TRUE,
                  html = T
                )
              ),
              language = reactableLang(
                filterPlaceholder = "Filter",
                noData = "Not found",
                pageInfo = "{rowStart}\u2013{rowEnd} of {rows} accessions",
                pagePrevious = "\u276e",
                pageNext = "\u276f",
              ),
              defaultPageSize = 50,
              highlight = TRUE,
              striped = TRUE,
              bordered = TRUE
    )#end reactable
    
    
    
  })
  
  observeEvent(reactable::getReactableState("rt1"), {
    

    if(!is.null(reactable::getReactableState("rt1")$selected)){
      
      rw = reactable::getReactableState("rt1")$selected
      
      vec = rv_vals$tbl[rw, ]
      
      if(!is.na(vec$DECLONGITUDE) & !is.na(vec$DECLATITUDE)){
        
        
        markers <- rv_vals$db_clean %>%
          dplyr::filter(ACCENUMB == vec$ACCENUMB ) 
      
        icons <- awesomeIcons(
          icon = 'ios-information',
          iconColor = 'black',
          library = 'ion',
          markerColor = as.character(getColor(markers))
        )
        
        
        leafletProxy("map2", data = markers) %>% 
          clearMarkers() %>%
          clearMarkerClusters() %>% 
          setView(vec$DECLONGITUDE, vec$DECLATITUDE, zoom= 11) %>% 
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
      }
      
      output$add_info1 <- renderPrint({
        
        to_print <- paste0("<b>ACCENUMB: </b>", vec$ACCENUMB, "<br>",
                           '<b>ELEVATION: </b>', vec$ELEVATION, " Mts above the sea level<br>",
                           "<b>STRM ELEVATION: </b>", vec$STRM_ELEV, " Mts above the sea level<br>",
                           "<b>TERRAIN SLOPE(degree): </b>", vec$SLOPE_DEGREE, "º<br>",
                           "<b>ACCESSION ISSUE: </b>", gsub(";", " - ", vec$issue_txt_desc))
        HTML(to_print)
        
        
        
      })
      
    }
    
    
    
    
  })
 
  output$down_btn <- downloadHandler(
    filename = function(){
      paste0('quality_score_data_', Sys.Date(), ".csv")
    },
    content = function(con){
      write.csv(rv_vals$tbl %>% 
                  dplyr::mutate(QUALITY_SCORE = gsub("<.*?>", "", QUALITY_SCORE),
                                LEVEL_OF_IMPROVEMENT = gsub("<.*?>", "", LEVEL_OF_IMPROVEMENT)), con)
    })
  
  
}#END SERVER

