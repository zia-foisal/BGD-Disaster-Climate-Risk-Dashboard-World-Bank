
pti_shps1 <- reactive({
  pti_shps[["admin2_District"]]
})

#Lealfet static options
output$main_map <- leaflet::renderLeaflet({
  # message("rendering local map")
  leaflet::leaflet(options = leaflet::leafletOptions(zoomSnap = 0.20, zoomDelta = 0.20)) %>%
    leaflet::addProviderTiles(provider =  "CartoDB.Voyager", group = "CARTO") %>%
    leaflet::setView(lng= 90.344352, lat = 23.6943117, zoom = 7)
})

#Script for running Principal Component Analysis and prepping district level standing in PC planes

#If no NAs in the selected columns, as it is calculation. Otherwise removing NAs


data_pca_final <- shiny::reactive({
  
  req(input$features_selected)
  
  data_pca_updated <- data_pca %>% 
    dplyr::select(district, 
                  input$features_selected)
  
  if(any(is.na(data_pca_updated))) {
    data_pca_updated <- data_pca_updated %>% 
      na.omit()
  }else{
    data_pca_updated
  }
})


#Recipe for algorithm
#District as ID,
#Normalizing predictors to compare variability across disparate features
#PCA step
pca_rec <- shiny::reactive({
  recipes::recipe(~., data = data_pca_final()) %>%
    recipes::update_role(district, new_role = "id") %>%
    recipes::step_normalize(recipes::all_numeric_predictors()) %>%
    recipes::step_pca(recipes::all_numeric_predictors())
})


#Prepping PCA Recipe to execute steps

pca_prep <- shiny::reactive({
  recipes::prep(pca_rec())
})

#Getting contribution of features to respective PCAs
tidied_pca <- shiny::reactive({
  recipes::tidy(pca_prep(), 2)
})

#District level PCA scores
pca_scores <- shiny::reactive({
  recipes::juice(pca_prep())
})

#To put omitted districts as NAs for Maps
districts_for_nas <- shiny::reactive({
  districts
  })

na_districts <- shiny::reactive({
  districts_for_nas() %>%         ##As a function insert
    dplyr::anti_join(pca_scores()) %>%
    dplyr::mutate(PC1 = NA)
})

#Updated pca score with NA districts scores as NAs
pca_scores_updated <- shiny::reactive({
  pca_scores() %>%
    dplyr::bind_rows(na_districts()) %>%
    dplyr::arrange(district)
})
#Data for Maps
map_data_pca <- shiny::reactive({
  pca_scores_updated()
})


#Labelling
labels_map_1 <- reactive({
  paste0(glue("<b>District</b>: { pti_shps1()$admin2Name } </br>"), "\n",
         glue("<b>Weighting scheme: </b> Principal Component Analysis (1)"), "<br/>",
         glue("<b>PTI score:</b> "), "\n",
         glue("{ round(map_data_pca()$PC1, 4)  }"), sep = "") %>%
    lapply(htmltools::HTML)
})

pal_new_pca <- reactive({
  req(input$bins_pca)
    rev(colorRampPalette(colors = c('#2c7bb6', '#abd9e9', '#ffffbf', '#fdae61', '#d7191c'), space = "Lab")(input$bins_pca))
})

#breaks defined
breaks_pca <- reactive({
  req(input$bins_pca)
   # req(unique(map_data()$context) %in% c("negative", "positive"))
  quantile(map_data_pca()$PC1, seq(0, 1, 1 / (input$bins_pca)), na.rm = TRUE) %>%
      unique()

})

pal_pca <- reactive ({
  leaflet::colorBin(palette =  pal_new_pca(),
                    bins = breaks_pca(),
                    na.color = "grey",
                    domain = NULL,
                    map_data_pca()[,"PC1"],
                    pretty = F,
                    reverse=T
  )
  
})

# Pal_legend
pal_leg_pca <- reactive ({
  leaflet::colorBin(palette = pal_new_pca(),
                    bins= breaks_pca(),
                    na.color = "grey",
                    domain =(map_data_pca()[,"PC1"]),
                    pretty = F,
                    reverse=T
                   
  )
})

#Dynamic leaflet
# shiny::observeEvent(input$features_selected,{
observe({
  
  req(input$bins_pca) || req(input$features_selected)
  
  leaflet::leafletProxy("main_map", data=pti_shps1(),
                        deferUntilFlush = TRUE) %>%
    removeControl("legend") %>% 
    leaflet::clearShapes() %>%
    leaflet::addPolygons(label= labels_map_1(),
                         labelOptions = leaflet::labelOptions(
                           style = list("font-weight"= "normal",
                                        padding= "3px 8px",
                                        "color"= "black"),
                           textsize= "10px",
                           direction = "auto",
                           opacity = 0.9
                           
                         ),
                         fillColor =  ~pal_pca()(map_data_pca()$PC1),
                         fillOpacity = 1,
                         stroke = TRUE,
                         color= "white",
                         weight = 1,
                         opacity = 0.9,
                         fill = TRUE,
                         dashArray = c(5,5),
                         smoothFactor = 0.8,
                         highlightOptions = leaflet::highlightOptions(weight= 2.5,
                                                                      color = "darkgrey",
                                                                      fillOpacity = 1,
                                                                      opacity= 1,
                                                                      bringToFront = TRUE),
                         group = "Polygons")
  
    
  labels_pca_legend <- str_c("Priority", " ", seq(1, input$bins_pca))  %>% 
  rev()
  
  labels_pca_legend[[length(labels_pca_legend)]] <-
    str_c(labels_pca_legend[[length(labels_pca_legend)]]," ", "(top priority)")
  
  if(any(is.na(map_data_pca()$PC1))){
    labels_pca_legend <- append("No data (NA)", labels_pca_legend)
  }
  
  if(any(is.na(map_data_pca()$PC1))){
    pal_new_pca_updated <- reactive(append(pal_new_pca(), "grey"))
  }else{
    pal_new_pca_updated <- reactive(pal_new_pca())
  }
  
  
  leaflet::leafletProxy("main_map", data= map_data_pca()) %>%
 
    leaflet::clearControls() %>%
    leaflet::addLegend("bottomright",
                       # pal= pal_leg_pca(),
                       values= map_data_pca()$PC1,
                       colors = (pal_new_pca_updated()),
                       labels = rev(labels_pca_legend),
                       title = "PCA Scores",
                       opacity= 1,
                       layerId = "legend"
    )
                         
    
})

#Plot for PCs
#By final 5 PCs juiced up

output$var_explained_pcs <- shiny::renderPlot({
  
  min_vars_for_var_exp <-  (input$features_selected)
  
  shiny::validate(
    shiny::need(!is.null(min_vars_for_var_exp), "Choose atleast one variable in the first tab for the chart to show up!")
  )
  
  sdev_id <- function(){
    pca_scores() %>%
      dplyr::select(-district) %>%
      purrr::map_df(., purrr::possibly(var, NA_integer_)) %>%
      tidyr::pivot_longer(everything(), names_to = "component", values_to = "value") %>%
      # group_by(component) %>%
      dplyr::mutate(
        percent_var = value/sum(value),
        cumsum = sum(percent_var))
  }
  
  sdev_id()  %>%
    # mutate(component = forcats::fct_reorder(factor(component), percent_var)) %>%
    ggplot2::ggplot(ggplot2::aes(component, percent_var))+
    ggplot2::geom_col(alpha=0.5, fill = "seagreen", width = 0.4)+
    # geom_point(aes(component,percent_var))+
    ggplot2::labs(x="Principal Components",
                  y="Variane Explained")+
    ggplot2::scale_y_continuous(labels = scales::percent_format())+
    theme(
        axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
})

output$pca_download <- downloadHandler(
  filename = function(){
    paste0("PCA Scores", ".csv")
  },
  content = function(file){
    write.csv(map_data_pca(), file)
  }
  
)



