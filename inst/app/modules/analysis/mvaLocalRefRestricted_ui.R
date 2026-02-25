mvaLocalRefRestrictedUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
        
      shiny::div(
        
        shiny::h5("Local Reference (Restricted)"),
        
        plotly::plotlyOutput(outputId = ns("mvaLocalRefRestrictedPlot"), height = "600px")
    
      )
      
    )
  )
}
