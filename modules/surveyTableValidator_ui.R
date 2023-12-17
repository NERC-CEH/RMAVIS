surveyTableValidatorUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      shiny::h5("Correct Species Names"),
      
      shiny::div(shiny::br()),
      
      shiny::div(
        shiny::actionButton(inputId = ns("correctSpecies"),
                            label = "Correct Species")
      ),
      
      shiny::div(shiny::br()),
      
      shiny::div(
        rhandsontable::rHandsontableOutput(outputId = ns("speciesCorrectionTable"), height = "300px")
      ),
      
      shiny::div(shiny::br()),
      
      shiny::h5("Validation Checks"),
      
      shiny::div(shiny::br()),
      
      bslib::layout_columns(

        col_widths = c(6, 6),

        fill = FALSE,

        fillable = TRUE,
        
        shiny::div(
          
          shiny::htmlOutput(outputId = ns("speciesInAcceptedText")),
          
          shiny::htmlOutput(outputId = ns("coverSuppliedText")),
          
          shiny::htmlOutput(outputId = ns("yearCompleteText")),
          
          shiny::htmlOutput(outputId = ns("groupCompleteText")),
          
          shiny::htmlOutput(outputId = ns("quadratCompleteText")),
          
          shiny::htmlOutput(outputId = ns("speciesCompleteText")),
          
          shiny::htmlOutput(outputId = ns("speciesQuadratDuplicatesText")),
          
          shiny::htmlOutput(outputId = ns("quadratIDUniqueText")),
          
          shiny::htmlOutput(outputId = ns("groupIDUniqueText"))
          
        ),
        
        shiny::div(
          
          shiny::htmlOutput(outputId = ns("okToProceedText"))
          
        )
        
      )
    )
  )
}