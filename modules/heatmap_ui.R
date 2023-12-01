heatmapUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      shiny::h5("Heatmap"),
      
      shiny::div(
        shiny::plotOutput(outputId = ns("heatmap")), # , height = "100%", width = "100%"
        style = "margin-left: -10px !important; "
      )
      
    )
  )
  
}