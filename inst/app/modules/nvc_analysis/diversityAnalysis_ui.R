diversityAnalysisUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      shiny::div(
        
        id = ns("diversitySummaryTable_div"),
        
        shiny::h5("Summary Table"),
        
        shiny::div(
          reactable::reactableOutput(outputId = ns("diversitySummaryTable"))
        ),
        
        shiny::div(shiny::br())
        
      ),
      
      shiny::div(
        
        id = ns("diversityIndicesTable_div"),
        
        shiny::h5("Quadrat Diversity Indices Table"),
        
        shiny::div(
          reactable::reactableOutput(outputId = ns("diversityIndicesTable"))
        ),
        
        shiny::div(shiny::br())
        
      ),
      
      shiny::div(
        
        id = ns("speciesRichnessSiteTable_div"),
        
        shiny::h5("Species Richness, by Site (Gamma Diversity)"),
        
        shiny::div(
          reactable::reactableOutput(outputId = ns("speciesRichnessSiteTable"))
        ),
        
      ),
      
      # shiny::div(shiny::br()),
      # 
      # shiny::h5("Beta Diversity"),
      # 
      # shiny::div(
      #   reactable::reactableOutput(outputId = ns(""))
      # ),
      
      shiny::div(
        
        id = ns("speciesRichnessGroupTable_div"),
        
        shiny::h5("Species Richness, by Group"),
        
        shiny::div(
          reactable::reactableOutput(outputId = ns("speciesRichnessGroupTable"))
        ),
        
        shiny::div(shiny::br())
        
      ),
      
      shiny::div(
        
        id = ns("speciesRichnessQuadratTable_div"),
        
        shiny::h5("Species Richness, by Quadrat (Alpha Diversity)"),
        
        shiny::div(
          reactable::reactableOutput(outputId = ns("speciesRichnessQuadratTable"))
        ),
        
        shiny::div(shiny::br())
        
      )
      
    )
  )
}
