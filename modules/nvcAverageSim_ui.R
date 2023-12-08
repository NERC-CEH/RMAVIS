nvcAverageSimUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      shiny::h5("Site Similarities"),
      
      shiny::div(
        rhandsontable::rHandsontableOutput(outputId = ns("nvcAverageSimSiteTable"))
        #, style = "margin-right: 5px !important;"
      ),
      
      shiny::div(shiny::br()),
      
      shiny::h5("Group Similarities"),
      
      shiny::div(
        rhandsontable::rHandsontableOutput(outputId = ns("nvcAverageSimTable"))
        #, style = "margin-right: 5px !important;"
      )
    )
  )
  
}