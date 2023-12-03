ordinationAnalysisUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      shiny::h5("Detrended Correspondence Analysis"),
      
      # shiny::div(
      #   shiny::h5("Method 1"),
      #   plotly::plotlyOutput(outputId = ns("method1DCAPlot"))
      # ),
      # 
      # shiny::div(
      #   shiny::h5("Method 2"),
      #   plotly::plotlyOutput(outputId = ns("method2DCAPlot"))
      # )
      
      shiny::div(shiny::br()),

      bslib::layout_columns(

        col_widths = c(6, 6),

        shiny::div(
          shiny::h5("Method 1"),
          plotly::plotlyOutput(outputId = ns("method1DCAPlot"))
        ),

        shiny::div(
          shiny::h5("Method 2"),
          plotly::plotlyOutput(outputId = ns("method2DCAPlot"))
        )

      )
    )
  )
}