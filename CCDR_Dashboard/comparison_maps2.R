#Comparison Map 2

#Map2

#shape file used
bgd_shp_comp2 <-  reactive({
  bgd_shp[[input$polygon_map2]]
})

map_data2 <- reactive({
  # req(input$domain_map2 == "Natural Hazards" || input$domain_map2 =="Development Outcomes")
  data %>%
    filter(polygon   %in%  input$polygon_map2,
           domain    %in%  input$domain_map2,
           indicator_1 %in%  input$indicator_map2)
})

#Updating Indicators based on Domain
observeEvent(input$domain_map2,{
  if( input$domain_map2 == "Development Outcomes"){
    choices_dev_c2 =  data %>%
      filter(domain == "Development Outcomes") %>%
      distinct(indicator_1) %>%
      pull(indicator_1)
    
    updateSelectInput(
      getDefaultReactiveDomain(),
      "indicator_map2",
      choices = choices_dev_c2
    )
  }else if(input$domain_map2 == "Relative Wealth Index"){
    choices_dev_c2 =  data %>% 
      filter(domain == "Relative Wealth Index") %>% 
      distinct(indicator_1) %>% 
      pull(indicator_1)
    
    updateSelectInput(
      getDefaultReactiveDomain(),
      "indicator_map2",    
      choices = choices_dev_c2
    )
  }else {
    updateSelectInput(
      getDefaultReactiveDomain(),
      "indicator_map2",
      choices = hazards_options[-11]
      )
  }
})

observeEvent(input$domain_map2,{
observeEvent(input$polygon_map2,{
  if(input$polygon_map2 == "District" & input$domain_map2 == "Natural Hazards"){
    dis_haz_choices_c3 = (list(`River flooding` = list("Expected mortality from river floods (population count)", 
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
      # hazards_options[-c(11, 24,25,26)]
    updateSelectInput(
      getDefaultReactiveDomain(),
      "indicator_map2",
      choices = dis_haz_choices_c3,
      selected =  dis_haz_choices_c3$`Agriculture & Built-up Area`[[2]]
    )
  }else if(input$polygon_map2 == "Upazila" & input$domain_map2 == "Natural Hazards"){
    upa_haz_choices_c4 =  (list(`River flooding` = list("Expected mortality from river floods (population count)", 
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
      "indicator_map2",
      choices = upa_haz_choices_c4)
  }
})
})

#Updating spatial level based on selected domain: so that when domain goes to development,
# District is selected by default
observeEvent(input$domain_map2,{
  if(input$domain_map2 == "Development Outcomes"){
    choices_pol_c3 = unique(data$polygon)[1]
    
    updateSelectizeInput(
      getDefaultReactiveDomain(),
      "polygon_map2",
      choices = choices_pol_c3)
    
  }else{
    choices_pol_c4 = unique(data$polygon)
    
    updateSelectizeInput(
      getDefaultReactiveDomain(),
      "polygon_map2",
      choices = choices_pol_c4)
  }
})

#Leaflet map2
output$double_map_2 <- renderLeaflet({
  leaflet(options = leafletOptions(zoomSnap = 0.20, 
                                   zoomDelta = 0.20)) %>% 
    addProviderTiles(providers$CartoDB, group = "CARTO") %>%
    setView(lng= 90.344352, lat = 23.6943117, zoom = 7) %>% 
    syncWith("combined_map")
  
})


outputOptions(output, "double_map_2", suspendWhenHidden = FALSE)
  # req(input$domain_map2 == "Natural Hazards" || input$domain_map2 =="Development Outcomes")
  labels_map2 <- reactive({
    if(input$domain_map2 == "Development Outcomes"){
      paste0(glue::glue("<b>District</b>: { bgd_shp_comp2()$district } </br>"), glue::glue("<b> { map_data2()$indicator_1 }: </b>"), "\n", glue::glue("{ round(map_data2()$value, 2)  }"), " ", glue::glue("{ map_data2()$unit }"), sep = "") %>%
        lapply(htmltools::HTML)
    }else if(input$polygon_map2 == "Upazila" & input$domain_map2 == "Natural Hazards"){
      paste0(glue::glue("<b>Upazila</b>: { bgd_shp_comp2()$upazila } </br>"), glue::glue("<b> { map_data2()$indicator_1 }: </b>"), "\n", glue::glue("{ round(map_data2()$value, 3) }"), " ", glue::glue("{ map_data2()$unit }"),  sep = "") %>%
        lapply(htmltools::HTML)
    }else if(input$polygon_map2 == "District" & input$domain_map2 == "Natural Hazards"){
      paste0(glue::glue("<b>District</b>: { bgd_shp_comp2()$district } </br>"), glue::glue("<b> { map_data2()$indicator_1 }: </b>"), "\n", glue::glue("{ round(map_data2()$value, 3) }"), " ", glue::glue("{ map_data2()$unit }"), sep = "") %>%
        lapply(htmltools::HTML)
    }else if(input$polygon_map2 == "District" & input$domain_map2 == "Relative Wealth Index"){
      paste0(glue::glue("<b>District</b>: { bgd_shp_comp2()$district } </br>"), glue::glue("<b> { map_data2()$indicator_1 }: </b>"), "\n", glue::glue("{ round(map_data2()$value, 2) }"), " ", glue::glue("{ map_data2()$unit }"), sep = "") %>%
        lapply(htmltools::HTML)
    }else if(input$polygon_map2 == "Upazila" & input$domain_map2 == "Relative Wealth Index"){
      paste0(glue::glue("<b>Upazila</b>: { bgd_shp_comp2()$upazila } </br>"), glue::glue("<b> { map_data2()$indicator_1 }: </b>"), "\n", glue::glue("{ round(map_data2()$value, 2) }"), " ", glue::glue("{ map_data2()$unit }"), sep = "") %>%
        lapply(htmltools::HTML)
    }
  })
  
  pal_new2 <- reactive({
    req(unique(map_data2()$context) %in% c("negative", "positive"))
    if (unique(map_data2()$context) == "negative"){
      c('#2c7bb6', '#abd9e9', '#ffffbf', '#fdae61', '#d7191c')
    } else {
      c('#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6')
    }
    
  })
  
  breaks_map2 <- reactive({
    req(unique(map_data2()$context) %in% c("negative", "positive"))
    if(
      unique(map_data2()$indicator_1) == "Expected mortality from coastal floods (population count)"||
      unique(map_data2()$indicator_1) == "Expected mortality from coastal floods (% of population)" ||
      unique(map_data2()$indicator_1) == "Expected increse of mortality from air pollution (population count)"
    ){
      
      seq(min(map_data2()$value),
          max(map_data2()$value),
          (max(map_data2()$value)/3))
    }
    else if( unique(map_data2()$indicator_1) == "Expected annual impact on builtup area due to coastal floods (% of builtup)"||
             unique(map_data2()$indicator_1) == "Expected annual impact on builtup area due to coastal floods (hectares)"){
      seq(min(map_data2()$value),
          max(map_data2()$value),
          (max(map_data2()$value)/2))
      
    } else if(unique(map_data2()$indicator_1) == "Expected mortality from river floods (% of population)"||
              unique(map_data2()$indicator_1) == "Expected mortality from river floods (population count)"||
              unique(map_data2()$indicator_1) == "Expected damage on builtup from river floods (hectares)"||
              unique(map_data2()$indicator_1) == "Expected damage on builtup from river floods (% of builtup)"||
              unique(map_data2()$indicator_1) == "Expected damage on agricultural land from river floods (hectares)"
              ){
      quantile(map_data2()$value, seq(0, 1, 1 / (7)), na.rm = TRUE) %>%
        unique()
      
    }else if(unique(map_data2()$indicator_1) == "Builtup exposed to landslide hazard (Builtup count)"||
             unique(map_data2()$indicator_1) == "Population exposed to landslide hazard (population count)"||
             unique(map_data2()$indicator_1) == "Expected annual impact on builtup area due to tropical cyclone (hectares)"||
             unique(map_data2()$indicator_1) == "Expected annual impact on builtup area due to tropical cyclone (% of builtup)"
             ){
      quantile(map_data2()$value, seq(0, 1, 1 / (8)), na.rm = TRUE) %>%
        unique()
    } else{ quantile(map_data2()$value, seq(0, 1, 1 / (input$bins)), na.rm = TRUE) %>%
        unique()
    }
  })
  
  pal2 <- reactive({
    colorBin(palette = pal_new2(),
             bins= breaks_map2(), 
             na.color = "grey",
             domain = NULL,
             pretty = F,
             reverse=F)
    
  })
  
  pal_leg2 <- reactive({
    colorBin(palette = pal_new2(),
             bins= breaks_map2(), 
             na.color = "grey",
             domain = map_data2()$value ,
             pretty = F,
             reverse=F)
    
  })
  
  
  observe({ 
  # req(input$my_tab == "my_tab1")  
    
  leafletProxy("double_map_2", data= bgd_shp_comp2()) %>%
    clearShapes() %>% 
    addPolygons(label= labels_map2(),
                labelOptions = labelOptions(
                  style = list("font-weight"= "normal",
                               padding= "3px 8px",
                               "color"= "black"),
                  textsize= "10px",
                  direction = "auto",
                  opacity = 0.8
                ),
                fillColor =  ~pal2()(map_data2()$value),
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
  
  leafletProxy("double_map_2", data = map_data2()) %>% 
    clearControls() %>% 
    addLegend("bottomright",
              pal= pal_leg2(),
              values= map_data2()$value,
              title =
                if(unique(map_data2()$unit) != ""){
                  glue("Legend", " ", "{ unique(map_data2()$unit)  }")
                }else{
                  "Legend"
                },
              opacity= 1,
              labFormat = labelFormat(
                between = "  :  ",
                digits = 2)
    )
})

# #Source Map 2
  output$source_comp2 <- renderText({
    paste0(" MAP 2", 
           "\n",
           " Source: ", glue("{ unique(map_data2()$source) }",),
           "\n",
           " Definition: ", glue("{ unique(map_data2()$definition) }"))
  })
