homeUI <- function(id) {
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      
      width = 12,
      
      shiny::div(
        
        shiny::br(),
        
        shiny::includeMarkdown("README.md")
        
      )
    )
  )
  
}
