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
    bar_choices_haz1 =  (list(`River flooding` = list("Expected mortality from river floods (population count)", 
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
                              `Relative Wealth Index` = list('Mean Relative Wealth Index') ))
      # hazards_options[-c(11, 24,25,26)]
    updateSelectInput(
      getDefaultReactiveDomain(),
      "indicator_bar",
      choices =bar_choices_haz1
    )
}else if(input$domain_bar == "Natural Hazards" & input$polygon_bar == "Upazila"){
  bar_choices_haz2 = (list(`River flooding` = list("Expected mortality from river floods (population count)", 
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



  

