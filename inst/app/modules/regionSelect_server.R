regionSelect <- function(input, output, session) {
  
  ns <- session$ns
  
  # Create Reactive Objects -------------------------------------------------
  region <- reactiveVal()
  # regionModules <- reactiveVal(
  #   tibble::tribble(
  #     ~great_britain, ~minnesota, ~module,
  #     TRUE, "", "",
  #     TRUE, "", "",
  #     TRUE, "", "",
  #     TRUE, "", "",
  #     TRUE, "", "",
  #     TRUE, "", "",
  #     TRUE, "", "",
  #     TRUE, "", "",
  #     TRUE, "", "",
  #     TRUE, "", "",
  #     TRUE, "", "",
  #     TRUE, "", "",
  #     TRUE, "", "",
  #     TRUE, "", "",
  #     TRUE, "", "",
  #     TRUE, "", "",
  #   )
  # )
  
  # Retrieve Region ---------------------------------------------------------
  observe({
    
    region(input$region)
    
  }) |>
    shiny::bindEvent(input$region,
                     ignoreInit = FALSE,
                     ignoreNULL = FALSE)
  
  # Select modules ----------------------------------------------------------
  # observe({
  #   
  #   
  #   
  # }) |>
  #   shiny::bindEvent(input$region,
  #                    ignoreInit = FALSE,
  #                    ignoreNULL = FALSE)
  
  
  # Return Setup Data -------------------------------------------------------
  return(region)
  
}
