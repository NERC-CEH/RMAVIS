mvaLocalRefUnrestrictedUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
        
      shiny::div(
        
        shiny::h5("Local Reference (Unrestricted)"),
        
        plotly::plotlyOutput(outputId = ns("mvaLocalRefUnrestrictedPlot"), height = "600px")
        
      )
      
    )
  )
}
