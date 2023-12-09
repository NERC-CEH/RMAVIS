diversityAnalysisUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      shiny::h5("Species Richness - by Site (Gamma Diversity)"),
      
      shiny::div(
        rhandsontable::rHandsontableOutput(outputId = ns("speciesRichnessSiteTable"))
      ),
      
      shiny::div(shiny::br()),
      
      shiny::h5("Species Recorded In One Year Only"),
      
      shiny::div(
        rhandsontable::rHandsontableOutput(outputId = ns("speciesUniqueToYearTable"))
      ),
      
      # shiny::div(shiny::br()),
      # 
      # shiny::h5("Beta Diversity"),
      # 
      # shiny::div(
      #   rhandsontable::rHandsontableOutput(outputId = ns(""))
      # ),
      
      shiny::div(shiny::br()),
      
      shiny::h5("Species Richness - by Group"),
      
      shiny::div(
        rhandsontable::rHandsontableOutput(outputId = ns("speciesRichnessGroupTable"))
      ),
      
      shiny::div(shiny::br()),
      
      shiny::h5("Species Richness - by Quadrat (Alpha Diversity)"),
      
      shiny::div(
        rhandsontable::rHandsontableOutput(outputId = ns("speciesRichnessQuadratTable"))
      )
      
    )
  )
  
}