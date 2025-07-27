rmavisTaxonNamesLookupUI <- function(id) {
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      shiny::div(
        reactable::reactableOutput(outputId = ns("rmavisTaxonNamesLookupTable"))
      )
      
    )
  )
  
}
