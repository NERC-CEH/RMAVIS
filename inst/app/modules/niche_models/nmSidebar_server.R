nmSidebar <- function(input, output, session) {
  
  ns <- session$ns
  
# Compose list of inputs to return from module ----------------------------
  nmSidebar_options <- reactiveVal()
  
  observe({
    
    nmSidebar_options_list <- list(
      "runNMAnalysis" = input$runNMAnalysis,
      "focalSpecies" = input$focalSpecies,
      "selectedModelDisplay" = input$selectedModelDisplay,
      "selectedVariablesDisplay" = input$selectedVariablesDisplay,
      "identifyPredDrivers" = input$identifyPredDrivers,
      "selectedModelPredict" = input$selectedModelPredict
      
    )
    
    nmSidebar_options(nmSidebar_options_list)
    
  }) |>
    bindEvent(input$runNMAnalysis,
              input$focalSpecies,
              input$selectedModelDisplay,
              input$selectedVariablesDisplay,
              input$identifyPredDrivers,
              input$selectedModelPredict,
              ignoreInit = TRUE)

  
# Return sidebar options --------------------------------------------------
  return(nmSidebar_options)
  
}



