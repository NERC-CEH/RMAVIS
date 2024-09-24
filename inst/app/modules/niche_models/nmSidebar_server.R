sidebar <- function(input, output, session) {
  
  ns <- session$ns
  
# Compose list of inputs to return from module ----------------------------
  nmSidebar_options <- reactiveVal()
  
  observe({
    
    nmSidebar_options_list <- list(
      "focalSpecies" = input$focalSpecies
    )
    
    nmSidebar_options(nmSidebar_options_list)
    
  }) |>
    bindEvent(input$focalSpecies,
              ignoreInit = FALSE)

  
# Return sidebar options --------------------------------------------------
  return(nmSidebar_options)
  
}



