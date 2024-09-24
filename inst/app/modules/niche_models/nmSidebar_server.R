nmSidebar <- function(input, output, session) {
  
  ns <- session$ns
  
# Compose list of inputs to return from module ----------------------------
  nmSidebar_options <- reactiveVal()
  
  observe({
    
    nmSidebar_options_list <- list(
      "runNMAnalysis" = input$runNMAnalysis,
      "focalSpecies" = input$focalSpecies
    )
    
    nmSidebar_options(nmSidebar_options_list)
    
  }) |>
    bindEvent(input$runNMAnalysis,
              input$focalSpecies,
              ignoreInit = TRUE)

  
# Return sidebar options --------------------------------------------------
  return(nmSidebar_options)
  
}



