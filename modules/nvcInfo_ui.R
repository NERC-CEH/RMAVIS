nvcInfoUI <- function(id) {
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      shiny::h5("NVC Code Lookup Table"),
      
      shiny::div(
        rhandsontable::rHandsontableOutput(outputId = ns("nvcInfoLookupTable"))
      )
    )
  )
  
}
