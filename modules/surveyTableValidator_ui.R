surveyTableValidatorUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      shiny::h5("Actions"),
      
      shiny::div(shiny::br()),
      
      bslib::layout_columns(
        
        col_widths = c(3, 3, 3, 3),
      
        shiny::div(
          shiny::actionButton(inputId = ns("adjustSpecies"),
                              label = "Adjust Species")
        ),
        
        shiny::div(
          shiny::actionButton(inputId = ns("combineDuplicates"),
                              label = "Combine Duplicates")
        )
        
      ),
      
      shiny::div(shiny::br()),
      
      shiny::h5("Validation Checks"),
      
      shiny::div(shiny::br()),
      
      bslib::layout_columns(

        col_widths = c(6, 6),

        fill = FALSE,

        fillable = TRUE,
        
        shiny::div(
          
          bslib::layout_columns(
            
            col_widths = c(1, 11),
            
            fill = FALSE,
            
            fillable = TRUE,
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Species Accepted",
              id = ns("speciesInAcceptedTextInfo"),
              shiny::markdown(
                "
                If TRUE all species names in the survey table are accepted names.
                If FALSE one or more of the species names are not accepted.
                These names should be corrected using the 'Species Adjustment Table' and 'Adjust Species' button.
                "
              ),
              placement = "bottom"
            ),
            
            shiny::htmlOutput(outputId = ns("speciesInAcceptedText")),
            
          ),
          
          # bslib::layout_columns(
          #   
          #   col_widths = c(1, 11),
          #   
          #   fill = FALSE,
          #   
          #   fillable = TRUE,
          #   
          #   bslib::popover(
          #     bsicons::bs_icon("info-circle"),
          #     title = "",
          #     id = ns(""),
          #     shiny::markdown(
          #       "
          #       If TRUE
          #       If FALSE
          #       
          #       "
          #     ),
          #     placement = "bottom"
          #   ),
          #   
          # ),
          
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
        
      ),
      
      shiny::div(shiny::br()),
      
      shiny::h5("Species Adjustment Table"),
      
      shiny::div(shiny::br()),
      
      shiny::div(
        rhandsontable::rHandsontableOutput(outputId = ns("speciesAdjustmentTable")) # , height = "300px"
      )
    )
  )
}
