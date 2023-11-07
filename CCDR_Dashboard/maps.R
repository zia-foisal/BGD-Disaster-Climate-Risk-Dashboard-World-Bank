#Proxy Mpas

#Main Interactive Maps
##############################################.        
#### Modal  ----
###############################################.
observeEvent(input$help_map,
             {
               showModal(modalDialog(
                 title = "How to use these maps",
                 p("These maps give district level estimates of CCDR- Bangladesh indicators over the selected filters"),
                 p("All indicators are rounded to 2 decimal points"),
                 # p("All Natural Hazards Indicators are rounded to 3 decimal points"),
                 p("Expect the color mapping to reverse with the context of  the selected indicators - e.g. Poverty (High) = Red whereas; Access to improved toilet facilities (High) = Blue"),
                 size = "m",
                 easyClose = TRUE,
                 fade=FALSE,
                 footer = modalButton("Close (Esc)")))
               }
             )


###Code

#shape file used
bgd_shp1 <-  reactive({
  bgd_shp[[input$polygon_map]]
  })

#Development outcomes Data
map_data <- reactive({
  data %>% 
    filter(polygon  %in%  input$polygon_map,
           domain    %in% input$domain_map,
           indicator_1 %in%  input$indicator_map)
  })

#Updating Indicators based on Domain
observeEvent(input$domain_map,{
  if( input$domain_map == "Development Outcomes")
    {
    choices_dev =  data %>% 
      filter(domain == "Development Outcomes") %>%
      distinct(indicator_1) %>%
      pull(indicator_1)
    
    updateSelectInput(
      getDefaultReactiveDomain(),
      "indicator_map",
      choices = choices_dev
    )
    }
  else if
  (input$domain_map == "Relative Wealth Index")
    {
    choices_dev =  data %>% 
      filter(domain == "Relative Wealth Index") %>% 
      distinct(indicator_1) %>% 
      pull(indicator_1)
    
    updateSelectInput(
      getDefaultReactiveDomain(),
      "indicator_map",
      choices = choices_dev
      )
    }else {
      updateSelectInput(
        getDefaultReactiveDomain(),
        "indicator_map",
        choices = indicator_listed
        # hazards_options[-11]
      )
  }
})

observeEvent(input$domain_map,{   #Latest to stop upazila option to show up in districts when come back from development to hazards
  observeEvent(input$polygon_map,
               {
                 if(input$polygon_map == "District" & input$domain_map == "Natural Hazards"){
                   dis_haz_choices = (list(
                     `River flooding` = list("Expected annual impact on builtup area due to river floods (hectares)",
                                             "Expected annual impact on builtup area due to river floods (relative)",
                                             "Expected annual mortality due to river floods (population count)",
                                             "Expected annual mortality due to river floods (relative)",
                                             "Expected annual exposure of agricultural land to river floods (hectares)",
                                             "Expected annual exposure of agricultural land to river floods (relative)"),
                     `Coastal flooding` = list("Expected annual impact on builtup area due to coastal floods (hectares)",
                                               "Expected annual impact on builtup area due to coastal floods (relative)",
                                               "Expected annual mortality due to coastal floods (population count)",
                                               "Expected annual mortality due to coastal floods (relative)",
                                               "Expected annual exposure of agricultural land to coastal floods (hectares)",
                                               "Expected annual exposure of agricultural land to coastal floods (relative)"),
                     # `Landslides` = list("Population exposed to landslide hazard (population count)",
                     #                     "Builtup exposed to landslide hazard (Builtup count)"),
                     `Drought` = list("Frequency of agricultural land exposed to drought affecting at least 30% of arable land during period 1984-2022 (growing season 1)",
                                      "Frequency of agricultural land exposed to drought affecting at least 50% of arable land during period 1984-2022 (growing season 1)" ),
                     `Heat stress` = list("Expected annual exposure of population to extreme heat stress hazard (population count)",
                                          "Expected annual exposure of population to extreme heat stress hazard (relative)"),
                     #`Air pollution`= list("Expected increase of mortality from air pollution (population count)"),
                     `Tropical Cyclone` = list("Expected annual impact on builtup area due to tropical cyclone (hectares)",
                                               "Expected annual impact on builtup area due to tropical cyclone (relative)"),
                     `Demography` = list("Number of Population in the district ( in thousands)"),
                     `Agriculture & Built-up Area` = list("Total builtup Area in the district (hectares)",
                                                          "Total agricultural land in the district (hectares)",
                     `Relative Wealth Index` = list('Mean Relative Wealth Index',
                                                    'Majority Relative Wealth Index')))
                     )
                   updateSelectInput(
                     getDefaultReactiveDomain(),
                     "indicator_map",
                     choices = dis_haz_choices,
                     selected = dis_haz_choices$`Agriculture & Built-up Area`[[2]]
                   )
                   }
                 else if(input$polygon_map == "Upazila" & input$domain_map == "Natural Hazards"){
                   upa_haz_choices = (list(
                     `River flooding` = list("Expected annual impact on builtup area due to river floods (hectares)",
                                                                   "Expected annual impact on builtup area due to river floods (relative)",
                                                                   "Expected annual mortality due to river floods (population count)",
                                                                   "Expected annual mortality due to river floods (relative)",
                                                                   "Expected annual exposure of agricultural land to river floods (hectares)",
                                                                   "Expected annual exposure of agricultural land to river floods (relative)"),
                                           `Coastal flooding` = list("Expected annual impact on builtup area due to coastal floods (hectares)",
                                                                     "Expected annual impact on builtup area due to coastal floods (relative)",
                                                                     "Expected annual mortality due to coastal floods (population count)",
                                                                     "Expected annual mortality due to coastal floods (relative)",
                                                                     "Expected annual exposure of agricultural land to coastal floods (hectares)",
                                                                     "Expected annual exposure of agricultural land to coastal floods (relative)"),
                                           # `Landslides` = list("Population exposed to landslide hazard (population count)", 
                                           #                     "Builtup exposed to landslide hazard (Builtup count)"),
                                           `Drought` = list("Frequency of agricultural land exposed to drought affecting at least 30% of arable land during period 1984-2022 (growing season 1)",
                                                            "Frequency of agricultural land exposed to drought affecting at least 50% of arable land during period 1984-2022 (growing season 1)" ),
                                           `Heat stress` = list("Expected annual exposure of population to extreme heat stress hazard (population count)",
                                                                "Expected annual exposure of population to extreme heat stress hazard (relative)"),
                                           #`Air pollution`= list("Expected increse of mortality from air pollution (population count)"),
                                           `Tropical Cyclone` = list("Expected annual impact on builtup area due to tropical cyclone (hectares)",
                                                                     "Expected annual impact on builtup area due to tropical cyclone (relative)"),
                                           `Demography` = list("Number of Population in the upazila ( in thousands)"),
                                           `Agriculture & Built-up Area` = list("Total builtup Area in the upazila (hectares)",
                                                                                "Total agricultural land in the upazila (hectares)"),
                                           `Relative Wealth Index` = list('Mean Relative Wealth Index',
                                                                          'Majority Relative Wealth Index')))
    
      # hazards_options[-c(1,2,3, 11)]
    updateSelectInput(
      getDefaultReactiveDomain(),
      "indicator_map",    
      choices = upa_haz_choices)
  }
})
})

#Updating spatial level based on selected domain: so that when domain goes to development, 
# District is selected by default
observeEvent(input$domain_map,{
  if(input$domain_map == "Development Outcomes"){
    choices_pol1 = unique(data$polygon)[1]
    
    updateSelectizeInput(
      getDefaultReactiveDomain(),
      "polygon_map",
      choices = choices_pol1)
    
  }else{
    choices_pol2 = unique(data$polygon)
    
    updateSelectizeInput(
      getDefaultReactiveDomain(),
      "polygon_map",
      choices = choices_pol2)
  }
})

#Lealfet
output$maps <- renderLeaflet({
  # message("rendering local map")
  leaflet(options = leafletOptions(zoomSnap = 0.20, zoomDelta = 0.20)) %>%
    addProviderTiles(providers$Esri, group = "ESRI") %>%
    addProviderTiles(providers$CartoDB, group = "CARTO") %>%
    # addProviderTiles(providers$st , group = "Stadia") %>%
    setView(lng= 90.344352, lat = 23.6943117, zoom = 7)
})

#Labelling for  Map

labels_map <- reactive({
  if(input$domain_map == "Development Outcomes"){  
    paste0(glue::glue("<b>District</b>: { bgd_shp1()$district } </br>"), glue::glue("<b> { map_data()$indicator_1 }: </b>"), "\n", glue::glue("{ round(map_data()$value, 2)  }"), " ", glue::glue("{ map_data()$unit }"), sep = "") %>% 
      lapply(htmltools::HTML) 
  }else if(input$polygon_map == "Upazila" & input$domain_map == "Natural Hazards"){
    paste0(glue::glue("<b>Upazila</b>: { bgd_shp1()$upazila } </br>"), glue::glue("<b> { map_data()$indicator_1 }: </b>"), "\n", glue::glue("{ round(map_data()$value, 3) }"), " ", glue::glue("{ map_data()$unit }"),  sep = "") %>% 
      lapply(htmltools::HTML) 
  }else if(input$polygon_map == "District" & input$domain_map == "Natural Hazards"){
    paste0(glue::glue("<b>District</b>: { bgd_shp1()$district } </br>"), glue::glue("<b> { map_data()$indicator_1 }: </b>"), "\n", glue::glue("{ round(map_data()$value, 3) }"), " ", glue::glue("{ map_data()$unit }"), sep = "") %>% 
      lapply(htmltools::HTML) 
  }else if(input$polygon_map == "District" & input$domain_map == "Relative Wealth Index"){
    paste0(glue::glue("<b>District</b>: { bgd_shp1()$district } </br>"), glue::glue("<b> { map_data()$indicator_1 }: </b>"), "\n", glue::glue("{ round(map_data()$value, 2) }"), " ", glue::glue("{ map_data()$unit }"), sep = "") %>% 
      lapply(htmltools::HTML)
  }else if(input$polygon_map == "Upazila" & input$domain_map == "Relative Wealth Index"){
    paste0(glue::glue("<b>Upazila</b>: { bgd_shp1()$upazila } </br>"), glue::glue("<b> { map_data()$indicator_1 }: </b>"), "\n", glue::glue("{ round(map_data()$value, 2) }"), " ", glue::glue("{ map_data()$unit }"), sep = "") %>% 
      lapply(htmltools::HTML) 
  }
})

pal_new <- reactive({
    req(unique(map_data()$context) %in% c("negative", "positive"))
    if (unique(map_data()$context) == "negative"){
      rev(colorRampPalette(colors = c('#2c7bb6', '#abd9e9', '#ffffbf', '#fdae61', '#d7191c'), space = "Lab")(input$bins))
     } else {
      colorRampPalette(colors = c('#2c7bb6', '#abd9e9', '#ffffbf', '#fdae61', '#d7191c'), space = "Lab")(input$bins)
    }

  })
  
  
#breaks defined since no variation in coastal floods so using the previous coloring approach instead of quintiles
breaks <- reactive({
  req(unique(map_data()$context) %in% c("negative", "positive"))
  if(
    unique(map_data()$indicator_1) == "Expected annual mortality due to coastal floods (population count)"||
    unique(map_data()$indicator_1) == "Expected annual mortality due to coastal floods (relative)"
    # unique(map_data()$indicator_1) == "Expected increse of mortality from air pollution (population count)"
    ){
    seq(min(map_data()$value),
        max(map_data()$value),
        (max(map_data()$value)/3))
    }
  else if( unique(map_data()$indicator_1) == "Expected annual impact on builtup area due to coastal floods (hectares)" ||
           unique(map_data()$indicator_1) == "Expected annual impact on builtup area due to coastal floods (relative)"||
           unique(map_data()$indicator_1) == "Expected annual exposure of agricultural land to coastal floods (hectares)" ||
           unique(map_data()$indicator_1) == "Expected annual exposure of agricultural land to coastal floods (relative)"
           ){
    seq(min(map_data()$value),
        max(map_data()$value),
        (max(map_data()$value)/2))
    } else if
  (unique(map_data()$indicator_1) == "Expected annual impact on builtup area due to river floods (hectares)"||
   unique(map_data()$indicator_1) == "Expected annual impact on builtup area due to river floods (relative)"||
   unique(map_data()$indicator_1) == "Expected annual mortality due to river floods (population count)"||
   unique(map_data()$indicator_1) == "Expected annual mortality due to river floods (relative)"||
   unique(map_data()$indicator_1) == "Expected annual exposure of agricultural land to river floods (hectares)" ||
   unique(map_data()$indicator_1) == "Expected annual exposure of agricultural land to river floods (relative)"
   )
      {
    quantile(map_data()$value, seq(0, 1, 1 / (7)), na.rm = TRUE) %>%
      unique()
    
    }else if(unique(map_data()$indicator_1) == "Expected annual impact on builtup area due to tropical cyclone (hectares)"||
             unique(map_data()$indicator_1) == "Expected annual impact on builtup area due to tropical cyclone (relative)"||
             unique(map_data()$indicator_1) == "Expected annual exposure of population to extreme heat stress hazard (population count)"||
             unique(map_data()$indicator_1) == "Expected annual exposure of population to extreme heat stress hazard (relative)"||
             unique(map_data()$indicator_1) == "Frequency of agricultural land exposed to drought affecting at least 30% of arable land during period 1984-2022 (growing season 1)"||
             unique(map_data()$indicator_1) == "Frequency of agricultural land exposed to drought affecting at least 50% of arable land during period 1984-2022 (growing season 1)" 
             ){
      quantile(map_data()$value, seq(0, 1, 1 / (8)), na.rm = TRUE) %>%
        unique()
      } else{ quantile(map_data()$value, seq(0, 1, 1 / (input$bins)), na.rm = TRUE) %>%
          unique()
    }
  })

  pal <- reactive ({
    colorBin(palette = pal_new(),
             bins= breaks(),
             na.color = "grey",
             domain = NULL,
             # (map_data()[,"value"]),
             pretty = F,
             reverse=T
    )

  })
  
  pal_leg <- reactive ({
    colorBin(palette = pal_new(),
             bins= breaks(),
             na.color = "grey",
             domain = map_data()[,"value"],
             pretty = FALSE,
             reverse=T
             )
  })
  
  observe({
    req(input$main_page == "main_page1")
    leafletProxy("maps", data=bgd_shp1()) %>% 
      clearShapes() %>% 
      addPolygons(label= labels_map(),
                  labelOptions = labelOptions(
                    style = list("font-weight"= "normal",
                                 padding= "3px 8px",
                                 "color"= "black"), 
                    textsize= "12px",
                    direction = "auto",
                    opacity = 0.9),
                  fillColor =  ~pal()(map_data()$value),
                  fillOpacity = 1,
                  stroke = TRUE,
                  color= "white",
                  weight = 1,
                  opacity = 0.9,
                  fill = TRUE,
                  dashArray = c(5,5),
                  smoothFactor = .8,
                  highlightOptions = highlightOptions(weight= 5,
                                                      fillOpacity = 1,
                                                      opacity= 1,
                                                      bringToFront = TRUE),
                  group = "Polygons") %>%
      addLayersControl(baseGroups = c("CARTO", "ESRI", "Stadia"),
                       options = layersControlOptions(collapsed = TRUE)) %>% 
      addScaleBar("bottomleft") %>%
      addMeasure("bottomleft")
    
    
    leafletProxy("maps", data= map_data()) %>%
      clearControls() %>%
      addLegend("bottomright",
                pal= pal_leg(),
                values= map_data()$value,
                title = if(unique(map_data()$unit) != ""){
                  glue("Legend", " ", "{ unique(map_data()$unit)  }")
                  }else{
                    "Legend"
                },
                opacity= 1,
                labFormat = labelFormat(
                  between = "  :  ",
                  digits = 2,
                  transform = function(x)  {
                    x
                    # paste0(min(x),":", max(x))
                    }
                  )
                )
    }
    )


output$source_map <- renderText({
  paste(" Source: ", glue("{ unique(map_data()$source) }"), 
        "\n",
        "Definition: ", glue("{ unique(map_data()$definition) }"))
})

observeEvent(input$screenshot,{
  screenshot(filename = glue("{ input$screenshot }", " ", "screenshot"), id = "maps", scale = 0.90, timer = 1)
  
})


#Download data underlying the shown map
output$mapdata <- downloadHandler(
  filename = function(){
    paste(glue("{ input$indicator_map }"), "_", glue("{ input$domain_map }"), ".csv")
    },
  content = function(file){
    write.csv(map_data(), file) 
  }
)

