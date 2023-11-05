
#Utils
library(shiny)
library(dplyr)
library(forcats)
library(tidyr)
library(ggplot2)
library(stringr)
library(shinyscreenshot)
library(glue)
library(leaflet)
library(sf)
library(shinythemes)
library(shinycssloaders)
library(htmltools)
library(DT)
library(shinydashboard)
library(shinyWidgets)
library(leafsync)
library(highcharter)
#remotes::install_github("Ebukin/devPTIpack")
library(devPTIpack)
library(maptools)
#remotes::install_github("rdpeng/gpclib")
library(gpclib)
library(waiter)
library(leaflet.minicharts)

#suppressWarnings(gpclibPermit())

theme_set(theme_light())
################################################################################
#Read in Data
# Shape File (Both District + Upazila)

bgd_shp <- readRDS("data/bgd_shp.RDS")

#Data Set
data <- readRDS("data/data.RDS")

#PTI Geometries
pti_shps <-  readRDS("data/bgd_geometries.rds") 

#PTI Metadata
pti_mtdt <- readRDS("data/bgd_metadata_climate.RDS")

#Socio-Economic Indicators List
development_options <- data %>%
  filter(domain == "Development Outcomes") %>%
  distinct(indicator) %>%
  pull(indicator)

#Natural Hazard Option List
hazards_options <- data %>% 
  filter(domain == "Natural Hazards") %>%  
  distinct(indicator_1) %>% 
  pull(indicator_1)

#RWI options list
rwi_options <- data %>% 
  filter(domain == "Relative Wealth Index") %>% 
  distinct(indicator_1) %>% 
  pull(indicator_1)

#Domain 
domain_options <- data %>% 
  distinct(domain) %>% 
  pull(domain)

# Spatial Level
spatial_level <- unique(data$polygon)

#Listed Indicator Options
indicator_listed = (list(`River flooding` = list("Expected mortality from river floods (population count)", 
                                                 "Expected mortality from river floods (% of population)", 
                                                 "Expected damage on builtup from river floods (hectares)", 
                                                 "Expected damage on builtup from river floods (% of builtup)", 
                                                 "Expected damage on agricultural land from river floods (hectares)"), 
                                                # "Expected exposure of agricultural land to river floods (% of ADM agricultural land)"),
                         `Coastal flooding` = list("Expected mortality from coastal floods (population count)", 
                                                   "Expected mortality from coastal floods (% of population)",
                                                   "Expected annual impact on builtup area due to coastal floods (hectares)", 
                                                   "Expected annual impact on builtup area due to coastal floods (% of builtup)"),
                         `Landslides` = list("Population exposed to landslide hazard (population count)", 
                                             "Builtup exposed to landslide hazard (Builtup count)"),
                         `Drought` = list("Frequency of agricultural stress affecting at least 30% of arable land during Season 1 (percentage of historical period 1984-2022)",
                                          "Frequency of agricultural stress affecting at least 30% of arable land during Season 2 (percentage of historical period 1984-2022)" ),
                         `Heat stress` = list("Expected annual exposure of population to very strong heat stress hazard (population count)"),
                         `Air pollution`= list("Expected increse of mortality from air pollution (population count)"),
                         `Tropical Cyclone` = list(" Expected annual impact on builtup area due to tropical cyclone (hectares)",
                                                   "Expected annual impact on builtup area due to tropical cyclone (% of builtup)"),
                         `Demography` = list("Total number of population in the district (count)", 
                                             "Total number of population in the Upazila (count)"),
                         `Agriculture & Built-up Area` = list("Total builtup area in the district (hectares)",
                                                              "Total  agricultural land in the district (hectares)", 
                                                              "Total builtup area in the Upazila (hectares)",
                                                              "Total  agricultural land in the upazila (hectares)"),
                         `Relative Wealth Index` = list('Mean Relative Wealth Index')))


legend <- readRDS("data/legend.rds")
districts <- readRDS("data/district.rds")
data_pca <- readRDS("data/data_pca.rds")
#Feature choices
mychoices <- list(
  `Disaster Data` = c(names(data_pca[2:20])),
  `Survey Data` = c(names(data_pca[21:420])),
  `Relative Wealth Index` = c(names(data_pca[421]))
)

################################################################################

#User Interface
################################################################################
ui <- function(request){
  navbarPage("CLIMATE Dashboard",
             header=tags$style(HTML("
                                        .container-fluid{
                                          padding: 3px !important;
                                        }
                                        .navbar{
                                         margin-bottom: 0px !important;
                                         margin-left: 1px !important;
                                         margin-right: 1px !important;
                                        }")),  
             
             tags$head(tags$script(type="text/javascript", src = "wb_img.js")),
             
             tabPanel("INTERACTIVE MAPS",
                      tabsetPanel(id = "main_page",
                                  type = c("hidden"),
                                  tabPanel("main_page1", 
                                           id="main_page1", 
                                           # use_waiter(),
                                           waiter_show_on_load(html = spin_loaders(10)),
                                           tags$style(type = 'text/css', '#maps {height: calc(97vh - 100px) !important;}', style= 'padding:0px;'),
                                           leafletOutput("maps"),
                                           br(),
                                           tags$head(tags$style("#source_map{color:black; font-size:12px; font-style:italic; max-height: 110px; 'padding:0px;'; margin-top:-18px; background: #ffe6cc; }")),
                                           
                                           verbatimTextOutput("source_map"),
                                           
                                           absolutePanel(id = "controls", class = "panel panel-default", fixed= TRUE,
                                                         draggable = TRUE, bottom = "auto", right = "auto", left = 70, top = 80,
                                                         width = 280, height = "auto",
                                                         style = "background-color: white;
                                                   opacity: 0.85;
                                                   padding: 20px 20px 20px 20px;
                                                   margin: auto;
                                                   border-radius: 5pt;
                                                   box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                                                   padding-bottom: 2mm;
                                                   padding-top: 1mm;",
                                                         br(),
                                                         selectInput("domain_map",
                                                                     "Choose Domain",
                                                                     choices = domain_options,
                                                                     selected = domain_options[1],
                                                                     selectize = F),
                                                         
                                                         selectInput("indicator_map",
                                                                     "Choose Indicator",
                                                                     choices = indicator_listed,
                                                                     # hazards_options,
                                                                     selectize = F),  
                                                         conditionalPanel(
                                                           condition = "input.domain_map == 'Natural Hazards' ||
                                                                        input.domain_map == 'Relative Wealth Index'",
                                                           selectInput("polygon_map",
                                                                       "Choose Spatial Level",
                                                                       choices = spatial_level,
                                                                       selected = spatial_level[1],
                                                                       selectize = F)
                                                         ),
                                                         conditionalPanel(
                                                           condition = 
                                            "input.indicator_map !== 'Expected mortality from coastal floods (population count)'&&
                                             input.indicator_map !== 'Expected mortality from coastal floods (% of population)'&&
                                             input.indicator_map !== 'Expected annual impact on builtup area due to coastal floods (hectares)' &&
                                             input.indicator_map !== 'Expected annual impact on builtup area due to coastal floods (% of builtup)'&&
                                             input.indicator_map !== 'Expected annual exposure of population to very strong heat stress hazard (population count)'&&
                                             input.indicator_map !== 'Expected increse of mortality from air pollution (population count)'&&
                                             input.indicator_map !== 'Builtup exposed to landslide hazard (Builtup count)'&&
                                             input.indicator_map !== 'Population exposed to landslide hazard (population count)'&&
                                             input.indicator_map !== 'Expected annual impact on builtup area due to tropical cyclone (hectares)'&&
                                             input.indicator_map !== 'Expected annual impact on builtup area due to tropical cyclone (% of builtup)'&&
                                             input.indicator_map !== 'Expected mortality from river floods (% of population)'&&
                                             input.indicator_map !== 'Expected mortality from river floods (population count)'&&
                                             input.indicator_map !== 'Expected damage on agricultural land from river floods (hectares)'&&
                                             input.indicator_map !== 'Expected damage on builtup from river floods (hectares)'",      
                                                           
                                                           numericInput("bins",
                                                                        "Choose Number of Bins",
                                                                        value = 5,
                                                                        min=3,
                                                                        max = 10,
                                                                        step = 1)
                                                         ),
                                                         downloadButton("mapdata", "Data", class= "btn-sm"),
                                                         actionButton("screenshot", "Image",class="btn-sm", icon=icon("camera")),
                                                         actionButton("help_map", "Help", icon= icon('question-circle'), class ="btn-sm"),
                                                         br(),
                                                         br()
                                           )
                                  ))),
             tabPanel("PTI",
                      tabsetPanel(id = "pti_help",
                                  type = c("hidden"),
                                  # selected ="PTI1" ,
                                  
                                  tabPanel("PTI1", 
                                           id="PTI1", 
                                           
                                           devPTIpack::mod_ptipage_twocol_ui(
                                             id = "pti_mod", 
                                             map_height = "calc(90vh)", 
                                             side_width = "350px", 
                                             wt_style = "zoom:1;", 
                                             show_waiter = FALSE,
                                             wt_dwnld_options = c("data", "weights"),
                                             map_dwnld_options = c()),
                                           
                                           h6(actionLink("pti_link_1",
                                                         "What is the Project Targeting Index (PTI)?")),
                                           h6(actionLink("pti_link_2",
                                                         "How is the PTI constructed, and how should the score be interpreted?"))
                                           
                                  ))),
             tabPanel("PCA",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          style = "background-color: white;",
                          tags$strong(tags$em(tags$h6("Select the features to compute the Geographic Targeting Index based on Principal Component Analysis (PC1)"))),
                          shinyWidgets::pickerInput("features_selected",
                                                    "Select featues for PCA",
                                                    choices =  mychoices,
                                                    selected = mychoices[1],
                                                    options = list(
                                                      `actions-box` = TRUE),
                                                    multiple=TRUE),
                          
                          br(),
                          # br(),
                          # Variance graph of the PCs selected
                          
                          shiny::plotOutput("var_explained_pcs",
                                            height = "150px",
                                            width = '100%'),
                          br(),               
                          numericInput("bins_pca",
                                       "Choose Number of Bins",
                                       value = 5,
                                       min=3, 
                                       max = 10, 
                                       step = 1),
                        
                          br(),
                          shiny::fluidRow(shiny::downloadLink("pca_download",
                                                              "Download PCA (xlsx)",
                                                              icon= icon("download"),
                                                              class = "btn-sm")),
                          br(),
                          h6(actionLink("pca_link_1",
                                        "What are the PCA Scores?")),
                          h6(actionLink("pca_link_2",
                                        "How is the PCA constructed, and how should the score be interpreted?"))
                        ),
                        
                        
                        shiny::mainPanel(
                          width = 9,
                          tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                          tags$style(type = "text/css", "#main_map {height: calc(100vh - 80px) !important;}"),
                          
                          
                          
                          leaflet::leafletOutput("main_map",
                                                 height = '100vh',
                                                 width = "75.5vw"
                          ),
                          
                          tags$style(' #main_map {
                        position: relative;
                        margin-left: -26px;
                        padding: 0px;
                        }')
                          
                        )
                      )),
             tabPanel("COMPARISON MAPS",
                      
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          style = "background-color: white;
                               opacity: 0.85;  
                               padding: 20px 20px 20px 20px;
                               
                               border-radius: 5pt;
                               box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                               padding-bottom: 2mm;
                               padding-top: 1mm;",
                          
                          selectInput("domain_map1",
                                      "Choose Domain for MAP1",
                                      choices = domain_options,
                                      selected = domain_options[1],
                                      selectize = F),   
                          selectInput("indicator_map1",
                                      "Choose Indicator for MAP1",
                                      choices = hazards_options,
                                      selectize = F),
                          conditionalPanel(
                            condition = "input.domain_map1 == 'Natural Hazards' ||
                                         input.domain_map1 == 'Relative Wealth Index'",
                            selectInput("polygon_map1",
                                        "Choose Spatial Level for MAP1",
                                        choices = spatial_level,
                                        selected = spatial_level[1],
                                        selectize = F)
                          ),
                          
                          hr(),
                          selectInput("domain_map2",
                                      "Choose Domain for MAP2",
                                      choices = domain_options,
                                      selected = domain_options[1],
                                      selectize = F),  
                          
                          selectInput("indicator_map2",
                                      "Choose Indicator for MAP2",
                                      choices = hazards_options,
                                      selectize = F),
                          conditionalPanel(
                            condition = "input.domain_map2 == 'Natural Hazards' ||
                                         input.domain_map2 == 'Relative Wealth Index'",
                            selectInput("polygon_map2",
                                        "Choose Spatial Level for MAP2",
                                        choices = spatial_level,
                                        selected = spatial_level[1],
                                        selectize = F)
                          ),
                          
                          # actionButton("screenshot_comp", "Image",class="btn-sm", icon = icon("camera")),
                          actionButton("help_comp", "Help", icon= icon('question-circle'), class ="btn-sm"),
                        ),
                        
                        mainPanel(
                          width = 9,
                          fluidRow(
                            column(width = 6,
                                   offset = 0,
                                   style = 
                                     'padding-bottom:0px; 
                                       padding-left:0px; 
                                       padding-right:0px; 
                                       margin-left:-10px; 
                                       position: relative;',
                                   tags$style(type = 'text/css', '#double_map_1 {height: calc(85vh - 50px) !important;}'),
                                   leafletOutput("double_map_1", width = "100%", height = "400px"),
                                                              ),
                            column(width = 6,
                                   offset = 0,
                                   style = 
                                     'padding-bottom:0px; 
                                       padding-left:2px; 
                                       padding-right:2px; 
                                       margin-left:0px;
                                       margin-right:5px;
                                       position: relative;',
                                   tags$style(type = 'text/css', '#double_map_2 {height: calc(85vh - 50px) !important;}'),
                                   # tags$style(type="text/css", "#double_map_2.recalculating { opacity: 1.0; }"),
                                   leafletOutput("double_map_2", width = "100%", height = "400px")
                                  
                            )),
                       
                          fluidRow(
                            column(6,
                                   offset = 0,
                                   style =
                                     'padding-bottom:0px;
                                       padding-left:0px;
                                       padding-right:0px;
                                       margin-left:0px;
                                       margin-right:-10px;
                                       position: relative;',
                                   tags$head(tags$style(
                                     "#source_comp1{color:black; 
                                       margin-left:-10px;  
                                       font-size:12px; font-style:italic; max-height: 110px; background: #ffe6cc;  }")),
                                   tags$style(type = 'text/css', '#source_comp1 {height: calc(20vh - 20px) !important;}'),
                                   verbatimTextOutput("source_comp1")),
                            column(6,
                                   offset = 0,
                                   style = 
                                     'padding-bottom:0px;
                                       padding-left:2px;
                                       padding-right:2px;
                                       margin-left:0px;
                                       margin-right:5px;
                                       position: relative;',
                                   tags$head(tags$style(
                                     "#source_comp2{color:black; 
                                       font-size:12px; font-style:italic; max-height: 110px; background: #ffe6cc; }")),
                                   tags$style(type = 'text/css', '#source_comp2 {height: calc(20vh - 20px) !important;}'),   
                                   verbatimTextOutput("source_comp2")
                                   
                            ))
                        )
                      )
             ),
             
             tabPanel("GRAPHS",
                      
                      sidebarLayout(
                        sidebarPanel(width= 3,
                                     style = "background-color: white;
                               opacity: 0.85;  
                               padding: 20px 20px 20px 20px;
                               margin: auto;
                               border-radius: 5pt;
                               box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                               padding-bottom: 2mm;
                               padding-top: 1mm;",
                                     
                                     
                                     selectInput(
                                       "division_bar",
                                       "Choose Division",
                                       choices = unique(data$division)[-5],
                                       selectize = F
                                     ),
                                     
                                     selectInput(
                                       "domain_bar",
                                       "Choose Domain",
                                       choices = unique(data$domain)
                                     ),
                                     selectInput(
                                       "indicator_bar",
                                       "Choose Indicator",
                                       choices = unique(data$indicator_1),
                                       selectize = F
                                     ),
                                     
                                     conditionalPanel(
                                       condition = "input.domain_bar == 'Natural Hazards' ||
                                                    input.domain_bar == 'Relative Wealth Index'",  
                                       selectInput("polygon_bar",
                                                   "Choose Spatial Level",
                                                   choices = unique(data$polygon),
                                                   selected = "District",
                                                   selectize = F)
                                     ),
                                     
                                     
                                     actionButton("help_bar", "Help", icon= icon('question-circle'), class ="btn-sm"),
                                     
                        ),
                        mainPanel(
                          plotOutput(
                            'bar_chart',
                            width = '900px',
                            height = '900px'
                          ),
                          
                          
                          fluidRow(
                            column(width = 8,
                                   (uiOutput("labels_bar"))
                            )
                          )
                          
                        )
                        
                      )
                      
             ),
             tabPanel("TABLES",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          style = "background-color: white;
                               opacity: 0.85;  
                               padding: 20px 20px 20px 20px;
                               margin: auto;
                               border-radius: 5pt;
                               box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                               padding-bottom: 2mm;
                               padding-top: 1mm;",
                          
                          selectInput("table_division",
                                      "Choose Division",
                                      choices = unique(data$division),
                                      selectize = F),
                          
                          selectInput("table_domain",
                                      "Choose Domain",
                                      choices = unique(data$domain),
                                      selectize = F),
                          selectInput("table_indicator",
                                      "Choose Indicator",
                                      choices = unique(data$indicator_1),
                                      selectize = F),
                          conditionalPanel(
                            condition = "input.table_domain == 'Natural Hazards' ||
                                         input.table_domain == 'Relative Wealth Index'",
                            selectInput("table_polygon",
                                        "Choose Spatial Level",
                                        choices = unique(data$polygon),
                                        selectize = F)
                          ),
                          
                          
                          verbatimTextOutput("source_table"),
                          tags$head(tags$style("#source_table{color:black; font-size:12px; font-style:italic; 
               overflow-y:scroll; max-height: 120px; background: #ffe6cc;}")),
                          br(),
                          downloadButton("downloadtable",
                                         "Save",
                                         type= "default", 
                                         class="btn-sm"),
                          actionButton("help_tables", "Help", icon= icon('question-circle'), class ="btn-sm")
                          
                        ),
                        mainPanel(
                          width=9,
                          dataTableOutput("tables_main")
                        )
                      )
                      
             ),
             tabPanel("ABOUT",
                      tabsetPanel(
                        tabPanel("ABOUT",
                                 
                                 mainPanel(
                                   width = 12,
                                   style="border:1px solid DodgerBlue;",
                                   
                                   tags$p(tags$strong(h4("CLIMATE DASHBOARD"))),
                                   hr(),
                                   tags$p(tags$b("Bangladesh is currently among the countries most affected by extreme weather events globally."),
                                          "The long-term Climate Risk Index (CRI) ranks Bangladesh as the 7th most at-risk country over 
the 2000-2019 period, with 173 extreme events recorded (CRI 2021). Global climate projections 
of temperature and precipitation, compounded by the degradation of the environment, due to land use 
patterns and unplanned urban development, indicate that", tags$b("even under the most optimistic global climate 
scenario, Bangladesh will continue to disproportionally – and more frequently and 
intensely – suffer "), "from extreme weather events and long-term climatic changes."  ) , 
                                   
                                   tags$p(tags$b("However, Disaster and climate Risk are not uniform across space."), 
                                          "It is a function of the probability and intensity with 
which a hazard occurs, and the exposure of people and assets to this hazard, both of which differ strongly across
geographies. Moreover, socioeconomic conditions, driving vulnerability, are also highly heterogenous at small scales, 
making communities and households more, or less, vulnerable in the face of climate change. The analytics in this 
dashboard", tags$b("zoom in to the level of granular administrative units, where investment decisions are made and local policy 
should be targeted.")),
                                   
                                   tags$p(tags$b("This dashboard focuses on disaster risk from floods, heat stress, droughts, landslides, tropical cyclone,
       and air pollution."), "These pose a critical environmental concern for Bangladesh, and indeed 
       across large parts of South Asia. To assess and quantify the impact of these hazards, 
       whether extreme events or long-term climatic changes, 
       we look at", tags$b("three types of exposure: (1) population, 
       (2) built-up assets, and (3) agricultural land."), 
                                          "Where available, an impact function is added to the exposure variables, 
       to demonstrate the expected annual impact on population health in terms 
       of morbidity and mortality, the potential damage to built-up assets and to 
       agricultural land. Vulnerability is then captured by a series of", 
                                          tags$b("socioeconomic indicators and development outcomes, tailored to specific hazards."),
                                          tags$hr(),
                                          
                                          tags$p(tags$em(tags$b("For further information and questions or suggestions, please reach out to:"))),
                                          
                                          # tags$p(<a href="mailto:afinn1@worldbank.org">afinn1@worldbank.org</a>),
                                          tags$p("Pierre Chrzanowski, Disaster Risk Management Specialist, GFDRR, World Bank  -",  tags$b(tags$a(href="mailto:pchrzanowski@worldbank.org", "pchrzanowski@worldbank.org"))),
                                          tags$p("Stuart Alexander Fraser , Lead Disaster Risk Management Consultant, GFDRR, World Bank   -",  tags$b(tags$a(href="mailto:sfraser@worldbank.org", "sfraser@worldbank.org"))),
                                          tags$p("Lander Bosch, Regional Geographer/YP    -",  tags$b(tags$a(href="mailto:lbosch@worldbank.org", "lbosch@worldbank.org"))),
                                          tags$p("Mattia Amadio, Disaster Risk Management Consultant, GFDRR, World Bank -",  tags$b(tags$a(href="mailto:mamadio@worldbank.org", "mamadio@worldbank.org"))),
                                          tags$p("Nuala Margaret Cowan, Disaster Risk Management Consultant, GFDRR, World Bank -",  tags$b(tags$a(href="mailto:ncowan@worldbank.org", "ncowan@worldbank.org"))),
                                          tags$p("Andrea Garcia Tapia, Data Science & Community Manager, GFDRR, The World Bank -",  tags$b(tags$a(href="mailto:agarciatapia@worldbank.org", "agarciatapia@worldbank.org"))),
                                          tags$p("Sergio Olivieri, Senior Economist, Statistician, Poverty and Equity Global Practice, World Bank -",  tags$b(tags$a(href="mailto:solivieri@worldbank.org", "solivieri@worldbank.org"))),
                                          tags$p("Ayago Wambile, Senior Economist , Poverty and Equity Global Practice, World Bank -",  tags$b(tags$a(href="mailto:awambile@worldbank.org", "awambile@worldbank.org"))),
                                          tags$p("Bernard Haven, Senior Economist, MTI Global Practice, World Bank  -",  tags$b(tags$a(href="mailto:bhaven@worldbank.org", "bhaven@worldbank.org"))),
                                          tags$p("Md Zia Uddin Foisal, Disaster and Climate Risk Data Fellow, GFDRR, World Bank -",  tags$b(tags$a(href="mailto:mfoisal@worldbank.org", "mfoisal@worldbank.org"))),
                                          
                                          
                                   )
                                 )
                        ),
                        tabPanel("DOCUMENTATAION",
                                 
                                 mainPanel(
                                   
                                   br(),
                                   tags$hr(),
                                   h5("Get datasets used in the climate dashboard"),
                                   tags$br(),
                                   fluidRow(
                                     
                                     downloadButton("download_nat",
                                                    "Natural Hazards",
                                                    class = "btn-success"),
                                     
                                     downloadButton("download_dev",
                                                    "Development Outcomes",
                                                    class = "btn-success"),
                                     downloadButton("download_glossary",
                                                    "Data Glossary",
                                                    class = "btn-success")
                                   ),
                                   
                                   
                                   hr(),
                                   tags$hr(),
                                   h4(strong("Github Repository")),
                                   br(),
                                   tags$a(href= "https://github.com/GFDRR/CCDR-tools", "Repo Link", target="_blank"), br(),
                                   
                                   
                                 )),
                        tabPanel("FEEDBACK",
                                 mainPanel(
                                   br(),
                                   h4(strong("Please use the link below to register your valued feedback")),
                                   br(),
                                   tags$a(href="https://forms.office.com/Pages/DesignPageV2.aspx?subpage=design&token=00c72e08-32ea-4c7b-ac7b-30930d30b274&id=wP6iMWsmZ0y1bieW2PWcNpMxHt0laMlJu7t4ksK2dhVUOVJJUDI3SFRWOTFBRkVKOU5YRjhBV040Si4u",
                                          "Register Feedback",
                                          target="_blank")
                                 )
                        )
                      )
             )
  )
  
}              

################################################################################

#Server
################################################################################
################################################################################
server <- function(input, output, session) {
  ################################################################################
  #Main Landing page  
  
  # source(file.path("main_landing_page.R"), local = TRUE)  
  # w <- Waiter$new()
  
  # observeEvent(input$main_page, {
  #   
  waiter_show()
  Sys.sleep(3)
  waiter_hide()
  
  # })
  
  ################################################################################
  #Main Maps
  source(file.path("maps.R"), local = TRUE)
  ################################################################################
  
  ################################################################################
  #Comparison Maps
  # source(file.path("proxy.R"), local = TRUE)
  
  source(file.path("comparison_maps1.R"), local = TRUE)  
  
  source(file.path("comparison_maps2.R"), local = TRUE)  
  ################################################################################
  #Bar Charts
  source(file.path('bar_charts.R'), local = TRUE)  
  ################################################################################
  #Main Tables
  source(file.path("tables.R"), local = TRUE)  
  
  #PCA
  source(file.path("pca.R"), local = TRUE)         
  
  ################################################################################
  ################################################################################
  #PTI Help
  source(file.path("PTI_Help.R"), local= TRUE)
  #PTI Server Side
  mod_ptipage_newsrv(
    id = "pti_mod",
    inp_dta = reactive(pti_mtdt),
    shp_dta = reactive(pti_shps),
    show_waiter = FALSE
    
  )
  
  ################################################################################
  #Data Dwonload
  source(file.path("data_download.R"), local = TRUE)  
  ################################################################################
  
}
################################################################################

################################################################################
################################################################################

# Run the application 
shinyApp(ui = ui, server = server)
################################################################################