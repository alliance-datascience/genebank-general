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
suppressMessages(if(!require(shiny.i18n)){install.packages("shiny.i18n");library(shiny.i18n)}else{library(shiny.i18n)})
suppressMessages(if(!require(bsplus)){install.packages("bsplus");library(bsplus)}else{library(bsplus)})


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

.marker-cluster-acm div {
    background-color: rgba(209, 209, 209, 0.8);
}

.marker-cluster-acm {
	background-color: rgba(183, 183, 183, 0.6);
	}

.radiobtn {font-size:12px;height:35px;}

.sidebar-fixed {
position: fixed;
width: 400px !important;
overflow: auto;
}


')

#shinyOptions(bslib = TRUE)
#bs_theme_fonts("Fira Code")

source("www/helpers.R")

trs <- Translator$new(translation_csvs_path = "./www/")
trs$set_translation_language("en") # here you select the default translation to display



ui <- page_navbar(
  theme = bs_theme(preset = "shiny", font_scale = 1),
  title = "Viewer",
  collapsible = TRUE,
  inverse = TRUE,
  id = "navbar",
  fillable = "Dashboard",
  nav_panel(shiny.i18n::usei18n(trs),
            title = trs$t("Dashboard"),
            layout_sidebar(
              border = T,
              fillable = F,
              fill = F,
              sidebar = bslib::sidebar(
                id = "side1",
                width = 400,
                class = "sidebar-fixed",
                tags$head(tags$style(btn_style)),
                title = img(src="https://www.fao.org/images/cgrfalibraries/cgrfa-images/cgiar-initiative---genebanks-02.jpg?sfvrsn=1b1842cc_0"),
                tags$br(),
                uiOutput("filtros1")
              ),
              tags$h3(tags$b(trs$t("COORDINATES QUALITY STATUS")), style = "text-align: center;"),
              tags$h5(trs$t("Last update"), "23-09-2024", style = "text-align: center;")
              ,layout_columns(
                col_widths  = c(4, 4,4)
                ,class = "mt-3"
                ,row_heights = c(1, 1, 1)
                ,value_box(
                  trs$t("Total Accessions"),
                  uiOutput("vbox1", container = h2),
                  showcase = bsicons::bs_icon("geo")
                )
                ,value_box(
                  trs$t("Missing coordinates"),
                  uiOutput("vbox2", container = h2),
                  showcase = bsicons::bs_icon("x-octagon")
                )
                ,value_box(
                  trs$t("Missing coordinates percentage"),
                  uiOutput("vbox3", container = h2),
                  showcase = bsicons::bs_icon("percent")
                )
                
              )
              ,layout_columns(col_widths  = c(7, 5)
                             ,class = "mt-3"
                             ,row_heights = c(2, 2)
                             ,card(
                               full_screen = T
                               ,card_header(
                                 trs$t("Interactive Map")
                               )
                               ,leafletOutput("map1")
                               #reactableOutput("rct1", width = "90%") 
                               
                             )
                             ,card(
                               full_screen = T
                               ,height = "800px"
                               ,card_header(
                                 trs$t("Quality score overall distribution")
                               )
                               ,card_body( plotlyOutput("gr1", height = "100%"),
                                           plotlyOutput("gr2", height = "100%"))
                             )
              )
              , layout_columns(col_widths  = c(8, 4)
                               ,class = "mt-3"
                               ,fill = T
                               #,row_heights = c(1, 2)
                               ,card(
                                 full_screen = T
                                 ,card_header(
                                   trs$t("Table")
                                 )
                                 ,card_body(
                                   shinyWidgets::pickerInput(
                                     inputId = "filt_issue",
                                     label = trs$t("Filter by issue"), 
                                     choices = c("Missing ORIGCTY",  
                                                 "Missing Coordinates", 
                                                 "Zero coordinate", 
                                                 "Coordinate in Sea or Coast line",
                                                 "Georeferenced to a centroid", 
                                                 "Belong to a set of accessions (more than 25) with the same coordinate",
                                                 "Less than two Decimal places in coordinate", 
                                                 "Mismatch ORIGCTY and GADM COUNTRY",
                                                 "Coordinate near to country border", 
                                                 "Difference in ELEVATION and STRM (elevation) greather than 150 mts or missing", 
                                                 "Missing COLLSITE", 
                                                 "Missing Country admon level 1 in COLLSITE",
                                                 "Missing Country admon level 2 in COLLSITE"),
                                     multiple = TRUE,
                                     options = pickerOptions(container = "body", 
                                                             actionsBox = TRUE),
                                     width = "100%"
                                   )
                                   ,reactableOutput("rt1", height  = "800px"),
                                   tags$hr(),
                                   tags$div(
                                     tags$div(
                                       downloadButton(
                                         label = trs$t("Download"),
                                         outputId = "down_btn",
                                         class = "btn btn-outline-secondary btn-lg") 
                                     ,style = "margin:  0; position: absolute; top: 50%; left: 50%; -ms-transform: translate(-50%, -50%); transform: translate(-50%, -50%);")
                                     ,style = "height:100px; position: relative") %>%  
                                     bs_embed_tooltip( title = "Download filtered table", placement = "right")
                                 ) 
                               )
                               ,card(
                                 full_screen = T
                                 ,card_header(
                                   trs$t("Additional Info")
                                 )
                                 ,card_body(tags$h3(trs$t("Accession status")),
                                            htmlOutput("add_info1"),
                                            tags$hr(),
                                            tags$h3(trs$t("Accession location")),
                                            leafletOutput("map2")
                                            ) 
                               )
                               
              )
              
            )
  ),
  nav_spacer(),
  nav_menu(title = "",
           align = "right",
           icon = tags$i(class="fa-solid fa-language"),
           nav_item( 
             tags$div(style = "width: '20px'; background-color:'red';"),
             radioGroupButtons(
               inputId = "lan",
               label = "",
               status = "btn btn-outline-secondary btn-lg",
               choices = c("English" = "en", "Español" = "es"),
               direction = "vertical",
               justified = T,
               individual = F,
               checkIcon = list(
                 yes = icon("ok", 
                            lib = "glyphicon"))
               
             )
             ))
  
  
)








