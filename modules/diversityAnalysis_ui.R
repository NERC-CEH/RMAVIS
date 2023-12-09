diversityAnalysisUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      shiny::div(
        
        id = ns("speciesRichnessSiteTable_div"),
        
        shiny::h5("Species Richness - by Site (Gamma Diversity)"),
        
        shiny::div(
          rhandsontable::rHandsontableOutput(outputId = ns("speciesRichnessSiteTable"))
        )
        
      ),
      
      shiny::div(shiny::br()),
      
      shiny::div(
        
        id = ns("speciesRecordedOneYearOnlyTable_div"),
        
        shiny::h5("Species Recorded In One Year Only"),
        
        shiny::div(
          rhandsontable::rHandsontableOutput(outputId = ns("speciesRecordedOneYearOnlyTable"))
        )
        
      ),
      
      # shiny::div(shiny::br()),
      # 
      # shiny::h5("Beta Diversity"),
      # 
      # shiny::div(
      #   rhandsontable::rHandsontableOutput(outputId = ns(""))
      # ),
      
      shiny::div(shiny::br()),
      
      shiny::div(
        
        id = ns("speciesRichnessGroupTable_div"),
        
        shiny::h5("Species Richness - by Group"),
        
        shiny::div(
          rhandsontable::rHandsontableOutput(outputId = ns("speciesRichnessGroupTable"))
        )
        
      ),
      
      shiny::div(shiny::br()),
      
      shiny::div(
        
        id = ns("speciesRichnessQuadratTable_div"),
        
        shiny::h5("Species Richness - by Quadrat (Alpha Diversity)"),
        
        shiny::div(
          rhandsontable::rHandsontableOutput(outputId = ns("speciesRichnessQuadratTable"))
        )
      )
    )
  )
}