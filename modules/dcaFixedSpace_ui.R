dcaFixedSpaceUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,

      shiny::div(
        shiny::h5("Fixed Reference Space"),
        plotly::plotlyOutput(outputId = ns("dcaFixedSpacePlot"))
      )

    )
  )
}