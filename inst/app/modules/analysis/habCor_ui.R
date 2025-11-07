habCorUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      shiny::h5("Habitat Correspondence"),
      
      shiny::div(
        reactable::reactableOutput(outputId = ns("habCorTable"))
      )
    )
  )
  
}