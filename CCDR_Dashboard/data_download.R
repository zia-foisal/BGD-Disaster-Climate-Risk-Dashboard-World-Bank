#Data Download

#Download Natural Hazards dataset 
output$download_nat <- downloadHandler(
  filename = "Natural_Hazards.xlsx",
  content = function(file){
    file.copy("www/Pakistan_Natural Hazard Exposure and Impact_Districts Tehsils.xlsx", file)
  }
)


#Download Development Outcomes dataset 
output$download_dev <- downloadHandler(
  filename = "Development_outcomes.xlsx",
  content = function(file){
    file.copy("www/pak_sub_ADM2_handover.xlsx", file)
  }
)

#Download Data inventory
output$download_glossary <- downloadHandler(
  filename = "Data_Inventory.xlsx",
  content = function(file){
    file.copy("www/Climate_Dashboard_Inventory.xlsx", file)
  }
)
################################################################################