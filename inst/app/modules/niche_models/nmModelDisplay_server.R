nmModelDisplay <- function(input, output, session, sidebar_nm_options) {
  
  ns <- session$ns
  
  # Retrieve sidebar options ------------------------------------------------
  focalSpecies <- reactiveVal()

  observe({

    focalSpecies(sidebar_nm_options()$focalSpecies)

  }) |>
    bindEvent(sidebar_nm_options(),
              ignoreInit = TRUE)
  
  
  # Retrieve Data -----------------------------------------------------------
  measures_rval <- reactiveVal()
  aleData_rval <- reactiveVal()
  featureImportance_rval <- reactiveVal()
  
  observe({
    
    measures <- targets::tar_read(name = "AllMeasures", store = tar_store) |>
      dplyr::filter(species == "NBNSYS0000004288")
    
    aleData <- targets::tar_read(name = "AllALEData", store = tar_store) |>
      dplyr::filter(species == "NBNSYS0000004288")
    
    featureImportance <- targets::tar_read(name = "AllFeatureImportance", store = tar_store) |>
      dplyr::filter(species == "NBNSYS0000004288")
    
    measures_rval(measures)
    aleData_rval(aleData)
    featureImportance_rval(featureImportance)

  }) |>
    bindEvent(focalSpecies(),
              ignoreInit = TRUE)
  
  
  # Feature importance plot -------------------------------------------------
  observe({
    
    featureImportance <- featureImportance_rval()
    
    output$feature_importance_plot <- plotly::renderPlotly({
      
      feature_importance_plot <- create_feature_importance_plot(fe_data = featureImportance)
      
      feature_importance_plotly <- plotly::ggplotly(feature_importance_plot)
      
      return(feature_importance_plotly)
      
    })

  }) |>
    bindEvent(featureImportance_rval(),
              ignoreInit = TRUE)
  
  # return(nmModelRunOutput_rval)
  
}