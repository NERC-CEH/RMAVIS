habCorUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      shiny::div(
        rhandsontable::rHandsontableOutput(outputId = ns("habCorTable"))
        #, style = "margin-right: 5px !important;"
      )
    )
  )
  
}