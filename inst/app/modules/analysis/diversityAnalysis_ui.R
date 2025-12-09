diversityAnalysisUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      shiny::div(
        
        id = ns("diversityTableYear_div"),
        
        shiny::h5("Diversity Measures - by Year"),
        
        shiny::div(
          reactable::reactableOutput(outputId = ns("diversityTableYear"))
        ),
        
      ),
      
      shiny::div(
        
        id = ns("diversityTableGroup_div"),
        
        shiny::h5("Diversity Measures - by Group"),
        
        shiny::div(
          reactable::reactableOutput(outputId = ns("diversityTableGroup"))
        ),
        
        shiny::div(shiny::br())
        
      ),
      
      shiny::div(
        
        id = ns("diversityTableQuadrat_div"),
        
        shiny::h5("Diversity Measures - by Quadrat"),
        
        shiny::div(
          reactable::reactableOutput(outputId = ns("diversityTableQuadrat"))
        ),
        
        shiny::div(shiny::br())
        
      )
      
    )
  )
}
