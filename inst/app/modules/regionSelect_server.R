regionSelect <- function(input, output, session) {
  
  ns <- session$ns
  
  # Create Reactive Objects -------------------------------------------------
  region <- reactiveVal()
  
  # Retrieve Region ---------------------------------------------------------
  observe({
    
    region(input$region)
    
  }) |>
    shiny::bindEvent(input$region,
                     ignoreInit = FALSE,
                     ignoreNULL = FALSE)
  
  
  # Return Setup Data -------------------------------------------------------
  return(region)
  
}
