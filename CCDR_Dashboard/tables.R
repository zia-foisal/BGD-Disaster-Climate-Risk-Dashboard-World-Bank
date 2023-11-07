#Climate Dashboard Tables
#This file maintains the Code for Tables in the App

##############################################.        
#### Modal  ----
###############################################.
observeEvent(input$help_tables, {
  showModal(modalDialog(
    title = "How to use these tables",
    p("These tables give district and upazila level estimates of selected CCDR indicators in each division"), 
    p("Upazila level estimates are available only for natural hazards"),
    p("All indicators are rounded-off to 2 decimal points"),
    # p("All the natural hazards indicators are rounded-off to 4 decimal points"),
    size = "m", easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
})

#Code

#Updating Spatial Level based on selected domain
observeEvent(input$table_domain,{
if(input$table_domain == "Development Outcomes"){
  choices_tab_pol1 = spatial_level[1]
  
  updateSelectInput(
    getDefaultReactiveDomain(),
    "table_polygon",
    choices = choices_tab_pol1
  )
}else{
  choices_tab_pol2 = spatial_level
  updateSelectInput(
    getDefaultReactiveDomain(),
    "table_polygon",
    choices = choices_tab_pol2
  )
}
})

#Updating Indicators based on selected domain
observeEvent(input$table_domain, {
  if(input$table_domain == "Development Outcomes"){
    choices_ind_tab1 = data %>% 
      filter(domain == "Development Outcomes") %>% 
      distinct(indicator_1) %>% 
      pull(indicator_1)
      
    updateSelectInput(
        getDefaultReactiveDomain(),
        "table_indicator",
        choices = choices_ind_tab1
      )
  }else if(input$table_domain == "Relative Wealth Index"){
    choices_ind_tab2 = data %>% 
      filter(domain == "Relative Wealth Index") %>% 
      distinct(indicator_1) %>% 
      pull(indicator_1)
    
    updateSelectInput(
      getDefaultReactiveDomain(),
      "table_indicator",
      choices = choices_ind_tab2
    )
  }else{
    choices_ind_tab3 = data %>% 
      filter(domain == "Natural Hazards") %>% 
      distinct(indicator_1) %>% 
      pull(indicator_1)
    
    updateSelectInput(
      getDefaultReactiveDomain(),
      "table_indicator",
      choices = choices_ind_tab3
      )
  }
})

#Updating again to make tesil indicators from district go away, and vice versa
observeEvent(input$table_domain,{   #<<<<<<<<<<<
observeEvent(input$table_polygon,{
  if(input$table_polygon == "District" & input$table_domain == "Natural Hazards"){
  choices_haz_tab1 = (list(
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
      "table_indicator",
      choices = choices_haz_tab1)
  }else if(input$table_polygon == "Upazila" & input$table_domain == "Natural Hazards") {
    choices_haz_tab2 = (list(`River flooding` = list("Expected annual impact on builtup area due to river floods (hectares)",
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

        updateSelectInput(
      getDefaultReactiveDomain(),
      "table_indicator",
      choices = choices_haz_tab2
      )
  }else if((input$table_polygon == "Upazila" || input$table_polygon == "District")  & input$table_domain == "Relative Wealth Index") {
    choices_haz_tab3 = data %>% 
      filter(domain == "Relative Wealth Index") %>% 
      distinct(indicator_1) %>% 
      pull(indicator_1)
    
    updateSelectInput(
      getDefaultReactiveDomain(),
      "table_indicator",
      choices = choices_haz_tab3
    )
  }else{
    choices_dev_tab4 = data %>% 
      filter(domain == "Development Outcomes") %>% 
      distinct(indicator_1) %>% 
      pull(indicator_1)
    updateSelectInput(
      getDefaultReactiveDomain(),
      "table_indicator",
      choices = choices_dev_tab4
    )
  }
})
})

#Table Data
tables_climate <- reactive({
  if(input$table_polygon == "District"){
  data %>% 
    filter(division ==  input$table_division,
           polygon ==   input$table_polygon,
           indicator_1 == input$table_indicator,
           !is.na(value)) %>% 
  select(division, district, domain, indicator=indicator_1, value, unit,source, -polygon, -upazila, -context, -indicator) %>% 
  janitor::clean_names(case = "title") 
  }else{
    data %>% 
      filter(division ==  input$table_pdivision,
             polygon ==   input$table_polygon,
             indicator_1 == input$table_indicator,
             !is.na(value)) %>% 
      select(division, district, upazila, domain, indicator=indicator_1, value, unit, -polygon,-context,-source, -indicator) %>% 
      janitor::clean_names(case = "title") 
  }
})

output$tables_main <- renderDataTable({
  DT::datatable( 
    tables_climate(), 
    extensions = "Buttons",
    options= list(pageLength=38,
                  lengthChange = FALSE,
                  dom = "Blfrtip",
                  buttons = c("copy", "csv", "excel", "pdf")))
}) 

#Download Tables
output$downloadtable <- downloadHandler(
  filename = function(){
    paste0("Table_", input$table_division , "_", input$table_indicator, ".csv")
  },
  content = function(file){
    write.csv(tables_climate(), file)
  }
  
)
#Source table
output$source_table <- renderText({
  
  paste("Source: ", glue("{ unique(tables_climate()$Source) }"))
})
