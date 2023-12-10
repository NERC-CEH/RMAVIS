mvaNationalRefUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      bslib::layout_columns(
        
        col_widths = c(6, 6),
        
        fill = FALSE,
        
        fillable = TRUE,
        
        shiny::div(
          
          shiny::h5("National Reference"),
          
          plotly::plotlyOutput(outputId = ns("mvaNationalRefPlot"), height = "600px")
          
        ),
        
        shiny::div(
          
          shiny::h5(" "),
          
          # rhandsontable::rHandsontableOutput(outputId = ns("rdaAnovaTable"))
          
        )
      )

    )
  )
}
