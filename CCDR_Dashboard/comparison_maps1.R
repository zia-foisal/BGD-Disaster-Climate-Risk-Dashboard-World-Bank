#Comaprison leaflet proxy#Comparison Maps
##############################################.        
#### Modal  ----
###############################################.
observeEvent(input$help_comp, {
  showModal(modalDialog(
    title = "How to use these maps",
    p("These maps offer comparison (spatial correlation) between selected CCDR-Bangladesh indicators"),
    # p("MAP1 refers to"),
    p("These maps give district and upazila level estimates of CCDR-Bangladesh indicators over the selected filters"),
    p("Upazila level maps are available only for Natural Hazards indicators"),
    p("All indicators are rounded to 2 decimal points"),
    # p("All Natural Hazards Indicators are rounded to 3 decimal points"),
    p("Expect the color mapping to reverse with the context of  the selected indicators - e.g. Poverty (High) = Red whereas; Access to improved toilet facilities (High) = Blue"),
    size = "m", easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
})


#code
#Map1

#shape file used
bgd_shp_comp <-  reactive({
  bgd_shp[[input$polygon_map1]]
})

#Development outcomes Data MAP1
map_data1 <- reactive({
  # req(input$domain_map1 == "Natural Hazards" || input$domain_map1 == "Development Outcomes")
  data %>% 
    filter(polygon   %in%  input$polygon_map1,
           domain    %in%  input$domain_map1,
           indicator_1 %in%  input$indicator_map1)
})

#Updating Indicators based on Domain
observeEvent(input$domain_map1,{
  if( input$domain_map1 == "Development Outcomes"){
    choices_dev_c1 =  data %>% 
      filter(domain == "Development Outcomes") %>% 
      distinct(indicator_1) %>% 
      pull(indicator_1)
    
    updateSelectInput(
      getDefaultReactiveDomain(),
      "indicator_map1",    
      choices = choices_dev_c1
    )
  }else if(input$domain_map1 == "Relative Wealth Index"){
    choices_dev_c1 =  data %>% 
      filter(domain == "Relative Wealth Index") %>% 
      distinct(indicator_1) %>% 
      pull(indicator_1)
    
    updateSelectInput(
      getDefaultReactiveDomain(),
      "indicator_map1",    
      choices = choices_dev_c1
    )
  }else {
    updateSelectInput(
      getDefaultReactiveDomain(),
      "indicator_map1",    
      choices = hazards_options[-11])
  }
})

observeEvent(input$domain_map1,{
observeEvent(input$polygon_map1,{
  if(input$polygon_map1 == "District" & input$domain_map1 == "Natural Hazards"){
    dis_haz_choices_c1 = (list(`River flooding` = list("Expected mortality from river floods (population count)", 
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
                               `Demography` = list("Total number of population in the district (count)"),
                               `Agriculture & Built-up Area` = list("Total builtup area in the district (hectares)",
                                                                    "Total  agricultural land in the district (hectares)"),
                               `Relative Wealth Index` = list('Mean Relative Wealth Index'))) 
      # hazards_options[-c(11,24,25,26)]
    updateSelectInput(
      getDefaultReactiveDomain(),
      "indicator_map1",    
      choices = dis_haz_choices_c1,
      selected =  dis_haz_choices_c1$`Agriculture & Built-up Area`[[2]]
    )
  }else if(input$polygon_map1 == "Upazila" & input$domain_map1 == "Natural Hazards"){
    upa_haz_choices_c2 = (list(`River flooding` = list("Expected mortality from river floods (population count)", 
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
                               `Demography` = list("Total number of population in the Upazila (count)"),
                               `Agriculture & Built-up Area` = list("Total builtup area in the Upazila (hectares)",
                                                                    "Total  agricultural land in the upazila (hectares)"),
                               `Relative Wealth Index` = list('Mean Relative Wealth Index'))) 
      # hazards_options[-c(1,2,3, 11)]
    updateSelectInput(
      getDefaultReactiveDomain(),
      "indicator_map1",    
      choices = upa_haz_choices_c2)
  }
})
})

#Updating spatial level based on selected domain: so that when domain goes to development, 
# District is selected by default
observeEvent(input$domain_map1,{
  if(input$domain_map1 == "Development Outcomes"){
    choices_pol_c1 = unique(data$polygon)[1]
    
    updateSelectizeInput(
      getDefaultReactiveDomain(),
      "polygon_map1",
      choices = choices_pol_c1)
    
  }else{
    choices_pol_c2 = unique(data$polygon)
    
    updateSelectizeInput(
      getDefaultReactiveDomain(),
      "polygon_map1",
      choices = choices_pol_c2)
  }
})


#leaflet Map1
output$double_map_1 <- renderLeaflet({
  
   leaflet(options = leafletOptions(zoomSnap = 0.20, zoomDelta = 0.20)) %>% 
    addProviderTiles(providers$CartoDB, group = "CARTO") %>% 
    # addProviderTiles(providers$Esri , group = "ESRI") %>%
    setView(lng= 90.344352, lat = 23.6943117, zoom = 7)  %>% 
     syncWith("combined_map")

})

#To render leafelt map before proxy observer updates
outputOptions(output, "double_map_1", suspendWhenHidden = FALSE)


#Labelling for  Map
labels_map1 <- reactive({
  if(input$domain_map1 == "Development Outcomes"){  
    paste0(glue::glue("<b>District</b>: { bgd_shp_comp()$district } </br>"), glue::glue("<b> { map_data1()$indicator_1 }: </b>"), "\n", glue::glue("{ round(map_data1()$value, 2)  }"), " ", glue::glue("{ map_data1()$unit }"), sep = "") %>% 
      lapply(htmltools::HTML) 
  }else if(input$polygon_map1 == "Upazila" & input$domain_map1 == "Natural Hazards"){
    paste0(glue::glue("<b>Upazila</b>: { bgd_shp_comp()$upazila } </br>"), glue::glue("<b> { map_data1()$indicator_1 }: </b>"), "\n", glue::glue("{ round(map_data1()$value, 3) }"), " ", glue::glue("{ map_data1()$unit }"),  sep = "") %>% 
      lapply(htmltools::HTML) 
  }else if(input$polygon_map1 == "District" & input$domain_map1 == "Natural Hazards"){
    paste0(glue::glue("<b>District</b>: { bgd_shp_comp()$district } </br>"), glue::glue("<b> { map_data1()$indicator_1 }: </b>"), "\n", glue::glue("{ round(map_data1()$value, 3) }"), " ", glue::glue("{ map_data1()$unit }"), sep = "") %>% 
      lapply(htmltools::HTML) 
  }else if(input$polygon_map1 == "District" & input$domain_map1 == "Relative Wealth Index"){
    paste0(glue::glue("<b>District</b>: { bgd_shp_comp()$district } </br>"), glue::glue("<b> { map_data1()$indicator_1 }: </b>"), "\n", glue::glue("{ round(map_data1()$value, 2) }"), " ", glue::glue("{ map_data1()$unit }"), sep = "") %>% 
      lapply(htmltools::HTML) 
  }else if(input$polygon_map1 == "Upazila" & input$domain_map1 == "Relative Wealth Index"){
    paste0(glue::glue("<b>Upazila</b>: { bgd_shp_comp()$upazila } </br>"), glue::glue("<b> { map_data1()$indicator_1 }: </b>"), "\n", glue::glue("{ round(map_data1()$value, 2) }"), " ", glue::glue("{ map_data1()$unit }"), sep = "") %>% 
      lapply(htmltools::HTML) 
  }
})

#Map Leaflet

  pal_new1 <- reactive({
    req(unique(map_data1()$context) %in% c("negative", "positive"))
    if (unique(map_data1()$context) == "negative"){
      c('#2c7bb6', '#abd9e9', '#ffffbf', '#fdae61', '#d7191c')
    } else {
      c('#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6')
    }
    
  })
 
  #breaks defined
  #since no variation in coastal floods so using the previous coloring approach instead of quintiles
  breaks_map1 <- reactive({
    req(unique(map_data1()$context) %in% c("negative", "positive"))
    if(
      unique(map_data1()$indicator_1) == "Expected mortality from coastal floods (population count)"||
      unique(map_data1()$indicator_1) == "Expected mortality from coastal floods (% of population)" ||
      unique(map_data1()$indicator_1) == "Expected increse of mortality from air pollution (population count)"
    ){
      
      seq(min(map_data1()$value),
          max(map_data1()$value),
          (max(map_data1()$value)/3))
    }
    else if( unique(map_data1()$indicator_1) == "Expected annual impact on builtup area due to coastal floods (% of builtup)" ||
             unique(map_data1()$indicator_1) == "Expected annual impact on builtup area due to coastal floods (hectares)"
             ){
      seq(min(map_data1()$value),
          max(map_data1()$value),
          (max(map_data1()$value)/2))
      
    } else if(unique(map_data1()$indicator_1) == "Expected mortality from river floods (% of population)"||
              unique(map_data1()$indicator_1) == "Expected mortality from river floods (population count)"||
              unique(map_data1()$indicator_1) == "Expected damage on builtup from river floods (hectares)"||
              unique(map_data1()$indicator_1) == "Expected damage on builtup from river floods (% of builtup)"||
              unique(map_data1()$indicator_1) == "Expected damage on agricultural land from river floods (hectares)"
              ){
      quantile(map_data1()$value, seq(0, 1, 1 / (7)), na.rm = TRUE) %>%
        unique()
      
    }else if(unique(map_data1()$indicator_1) == "Builtup exposed to landslide hazard (Builtup count)"||
             unique(map_data1()$indicator_1) == "Population exposed to landslide hazard (population count)"||
             unique(map_data1()$indicator_1) == "Expected annual impact on builtup area due to tropical cyclone (hectares)"||
             unique(map_data1()$indicator_1) == "Expected annual impact on builtup area due to tropical cyclone (% of builtup)"
             ){
      quantile(map_data1()$value, seq(0, 1, 1 / (8)), na.rm = TRUE) %>%
        unique()
    } else{ quantile(map_data1()$value, seq(0, 1, 1 / (input$bins)), na.rm = TRUE) %>%
        unique()
    }
  })
  
  pal1 <- reactive({
    colorBin(palette = pal_new1(),
             bins= breaks_map1(), 
             na.color = "grey",
             domain = NULL,
             pretty = F,
             reverse=F)
    
  })
  
  pal_leg1 <- reactive({
    colorBin(palette = pal_new1(),
             bins= breaks_map1(), 
             na.color = "grey",
             domain = (map_data1()[,"value"]),
             pretty = F,
             reverse=F)
    
  })
 
  observe({
    
    # comp_map1 <- reactive({
    # req(input$domain_map1 == "Natural Hazards" || input$domain_map1 == "Development Outcomes")
    # req(input$my_tab == "my_tab1")
    
    leafletProxy("double_map_1", data= bgd_shp_comp()) %>% 
    clearShapes() %>%
    addPolygons(label= labels_map1(),
                labelOptions = labelOptions(
                  style = list("font-weight"= "normal",   
                               padding= "3px 8px",
                               "color"= "black"), 
                  textsize= "10px",
                  direction = "auto",
                  opacity = 0.8
                ),
                fillColor =  ~pal1()(map_data1()$value),
                fillOpacity = 1,
                stroke = TRUE,
                color= "white",
                weight = 1,
                opacity = 0.9,
                fill = TRUE,
                dashArray = c(5,5),
                smoothFactor = 0.8,
                highlightOptions = highlightOptions(weight= 5,
                                                    fillOpacity = 1,
                                                    opacity= 1,
                                                    bringToFront = TRUE), 
                group = "Polygons") %>%
                addScaleBar("bottomleft")

    leafletProxy("double_map_1", data= map_data1()) %>% 
    clearControls() %>% 
    addLegend("bottomright",
              pal= pal_leg1(),
              values= map_data1()$value,
              title = 
                if(unique(map_data1()$unit) != ""){
                  glue("Legend", " ", "{ unique(map_data1()$unit)  }")
                }else{
                  "Legend"
                },
              opacity= 1,
              labFormat = labelFormat(
                between = "  :  ",
                digits = 2)
    )
})


# combineWidgets(map_l1, map_l2)

# #Source Map1  
output$source_comp1 <- renderText({
  paste0(" MAP 1", 
         "\n",
         " Source: ", glue("{ unique(map_data1()$source) }"),
         "\n",
         " Definition:",  glue("{ unique(map_data1()$definition) }"))
})


# 
# #Screen shot
observeEvent(input$screenshot_comp,{
  screenshot(selector = "#double_map2")
})

