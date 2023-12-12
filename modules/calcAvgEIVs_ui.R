calcAvgEIVsUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      shiny::div(
        id = ns("unweightedMeanHEValuesSite_div"),
        
        shiny::h5("Unweighted Mean Hill-Ellenberg Values, by Site"),
        
        shiny::div(
          rhandsontable::rHandsontableOutput(outputId = ns("unweightedMeanHEValuesSiteTable"))
        ),
        
        shiny::div(shiny::br())
        
      ),
      
      shiny::div(
        id = ns("weightedMeanHEValuesSite_div"),
        
        shiny::h5("Weighted Mean Hill-Ellenberg Values, by Site"),
        
        shiny::div(
          rhandsontable::rHandsontableOutput(outputId = ns("weightedMeanHEValuesSiteTable"))
        ),
        
        shiny::div(shiny::br())
      
      ),
      
      shiny::div(
        id = ns("weightedMeanHEValuesGroup_div"),
        
        shiny::h5("Weighted Mean Hill-Ellenberg Values, by Group"),
        
        shiny::div(
          rhandsontable::rHandsontableOutput(outputId = ns("weightedMeanHEValuesGroupTable"))
        ),
        
        shiny::div(shiny::br())
        
      ),
      
      shiny::div(
        id = ns("unweightedMeanHEValuesGroup_div"),
        
        shiny::h5("Unweighted Mean Hill-Ellenberg Values, by Group"),
        
        shiny::div(
          rhandsontable::rHandsontableOutput(outputId = ns("unweightedMeanHEValuesGroupTable"))
        ),
        
        shiny::div(shiny::br())
        
      ),
      
      shiny::div(
        id = ns("weightedMeanHEValuesQuadrat_div"),
        
        shiny::h5("Weighted Mean Hill-Ellenberg Values, by Quadrat"),
        
        shiny::div(
          rhandsontable::rHandsontableOutput(outputId = ns("weightedMeanHEValuesQuadratTable"))
        ),
        
        shiny::div(shiny::br())
        
      ),
      
      shiny::div(
        id = ns("unweightedMeanHEValuesQuadrat_div"),
        
        shiny::h5("Unweighted Mean Hill-Ellenberg Values, by Quadrat"),
        
        shiny::div(
          rhandsontable::rHandsontableOutput(outputId = ns("unweightedMeanHEValuesQuadratTable"))
        ),
        
        shiny::div(shiny::br())
        
      )
    )
  )
}