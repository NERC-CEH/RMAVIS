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
          
          shiny::uiOutput(outputId = ns("speciesInAcceptedText")),
          
          shiny::uiOutput(outputId = ns("coverSuppliedText")),
          
          shiny::uiOutput(outputId = ns("yearCompleteText")),
          
          shiny::uiOutput(outputId = ns("groupCompleteText")),
          
          shiny::uiOutput(outputId = ns("quadratCompleteText")),
          
          shiny::uiOutput(outputId = ns("speciesCompleteText")),
          
          shiny::uiOutput(outputId = ns("quadratIDUniqueText")),
          
          shiny::uiOutput(outputId = ns("groupIDUniqueText"))
          
        ),
        
        shiny::div(
          
          shiny::uiOutput(outputId = ns("okToProceedText"))
          
        )
        
      )
    )
  )
}