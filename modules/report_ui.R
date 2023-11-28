reportUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      shiny::downloadButton(
        outputId = ns("generateReport"),
        label = "Download Report",
        class = NULL,
        icon = shiny::icon("book")
        )
      )
    )
  
}