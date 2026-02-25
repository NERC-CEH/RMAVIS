speciesFreqUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      shiny::div(
        
        id = ns("speciesFrequencyTable_div"),
        
        shiny::h5("Species Frequency"),
        
        shiny::div(
          reactable::reactableOutput(outputId = ns("speciesFrequencyTable"))
        )
        
      )
    )
  )
}