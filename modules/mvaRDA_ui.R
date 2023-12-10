mvaRDAUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      bslib::layout_columns(
        
        col_widths = c(6, 6),
        
        fill = FALSE,
        
        fillable = TRUE,
        
        shiny::div(
          
          shiny::h5("Redundancy Analysis"),
          
          plotly::plotlyOutput(outputId = ns("mvaRDAPlot"), height = "600px")
          
        ),
        
        shiny::div(
          
          # shiny::h5(" "),
          shiny::div(shiny::br()),
          shiny::div(shiny::br()),
          
          rhandsontable::rHandsontableOutput(outputId = ns("rdaAnovaTable"))
          
        )
        
      )
    )
  )
}