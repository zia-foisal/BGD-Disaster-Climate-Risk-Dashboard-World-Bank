#Bar Charts (District)
##############################################.        
#### Modal  ----
###############################################.
observeEvent(input$help_bar, {
  showModal(modalDialog(
    title = "How to use these bar charts",
    p("These interactive bar charts give district and Upazila level estimates of CCDR-Bangladesh indicators for the selected division"), 
    p("Upazila level estimates are available only for natural hazards"),
    p("All indicators are rounded to 2 decimal points"),
      size = "m", easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
})

#Update Spatial Input Based on selected domain
observeEvent(input$domain_bar,{
 
  if(input$domain_bar == "Development Outcomes"){ 
  updateSelectInput(
    getDefaultReactiveDomain(),
    "polygon_bar",
    choices = unique(data$polygon)[1]
  )
  }else{
    updateSelectInput(
      getDefaultReactiveDomain(),
      "polygon_bar",
      choices = unique(data$polygon)
    )
  }
})
  
#Update Indicators based on spatial level
observeEvent(input$polygon_bar,{
  observeEvent(input$domain_bar,{
if(input$domain_bar == "Natural Hazards" & input$polygon_bar == "District"){
    bar_choices_haz1 =  (list(
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
      # hazards_options[-c(11, 24,25,26)]
    updateSelectInput(
      getDefaultReactiveDomain(),
      "indicator_bar",
      choices =bar_choices_haz1
    )
}else if(input$domain_bar == "Natural Hazards" & input$polygon_bar == "Upazila"){
  bar_choices_haz2 = (list(
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
    "indicator_bar",
    choices = bar_choices_haz2
  )
}else if(input$domain_bar == "Relative Wealth Index" & (input$polygon_bar == "Upazila" || input$polygon_bar == "District")){
  bar_choices_haz2 = (list(`Relative Wealth Index` = list("Mean Relative Wealth Index")))
  updateSelectInput(
    getDefaultReactiveDomain(),
    "indicator_bar",
    choices = bar_choices_haz2
  )
}else{ 
  updateSelectInput(
    getDefaultReactiveDomain(),
    "indicator_bar",
    choices = development_options
  )
}
  })
})

bar_chart_data <- function(){
data %>% 
 filter(
        polygon == input$polygon_bar, 
        division == input$division_bar,
        domain == input$domain_bar,
        indicator_1 == input$indicator_bar,
        !is.na(value)) %>% 
     arrange(desc(value))
}

##Bar chart
output$bar_chart <- renderPlot({
   if(input$polygon_bar == "District"){
    bar_chart_data() %>%
      mutate(district = fct_reorder(district , value)) %>%
      ggplot(aes(y=district, x=value)) +
      geom_col(fill="seagreen", width = 0.6, alpha=0.7)+
       labs(y="", x= input$indicator_bar)+
       theme(
         axis.line = element_line(color='black'),
         plot.background = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_blank())
   }else{
     bar_chart_data() %>%
       mutate(upazila = fct_reorder(upazila , value)) %>%
       ggplot(aes(y=upazila, x=value))+
       geom_col(fill="seagreen", width = 0.6, alpha=0.7 )+
       labs(y="", x= input$indicator_bar)+
       theme(
         axis.line = element_line(color='black'),
         plot.background = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_blank())
   }
})



  

