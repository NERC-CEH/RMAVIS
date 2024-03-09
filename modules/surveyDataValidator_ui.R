surveyDataValidatorUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      shiny::h5("Actions"),
      
      shiny::div(shiny::br()),
      
      bslib::layout_columns(
        
        col_widths = c(3, 3, 3),
      
        shiny::div(
          shiny::actionButton(inputId = ns("adjustSpecies"),
                              label = "Adjust Species")
        ),
        
        shiny::div(
          shiny::actionButton(inputId = ns("combineDuplicates"),
                              label = "Combine Duplicates")
        ),
        
        shiny::div(
          shiny::actionButton(inputId = ns("reallocateGroups"),
                              label = "Re-allocate Groups")
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
            
            shiny::htmlOutput(outputId = ns("speciesInAcceptedText"))
            
          ),
          
          bslib::layout_columns(

            col_widths = c(1, 11),

            fill = FALSE,

            fillable = TRUE,

            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Cover Supplied",
              id = ns("coverSuppliedTextInfo"),
              shiny::markdown(
                "
                If TRUE cover values are supplied.
                
                If FALSE cover values are not supplied.
                
                Note that cover values are not required to proceed.

                "
              ),
              placement = "bottom"
            ),
            
            shiny::htmlOutput(outputId = ns("coverSuppliedText"))

          ),
          
          bslib::layout_columns(
            
            col_widths = c(1, 11),
            
            fill = FALSE,
            
            fillable = TRUE,
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Year Column Complete",
              id = ns("yearCompleteTextInfo"),
              shiny::markdown(
                "
                If TRUE the Year column is complete and contains no missing values.
                
                If FALSE the Year column contains missing values.

                "
              ),
              placement = "bottom"
            ),
            
            shiny::htmlOutput(outputId = ns("yearCompleteText"))
            
          ),
          
          bslib::layout_columns(
            
            col_widths = c(1, 11),
            
            fill = FALSE,
            
            fillable = TRUE,
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Group Column Complete",
              id = ns("groupCompleteTextInfo"),
              shiny::markdown(
                "
                If TRUE the Group column is complete and contains no missing values.
                
                If FALSE the Group column contains missing values.

                "
              ),
              placement = "bottom"
            ),
            
            shiny::htmlOutput(outputId = ns("groupCompleteText"))
            
          ),
          
          bslib::layout_columns(
            
            col_widths = c(1, 11),
            
            fill = FALSE,
            
            fillable = TRUE,
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Quadrat Column Complete",
              id = ns("quadratCompleteTextInfo"),
              shiny::markdown(
                "
                If TRUE the Quadrat column is complete and contains no missing values.
                
                If FALSE the Quadrat column contains missing values.

                "
              ),
              placement = "bottom"
            ),
            
            shiny::htmlOutput(outputId = ns("quadratCompleteText"))
            
          ),
          
          bslib::layout_columns(
            
            col_widths = c(1, 11),
            
            fill = FALSE,
            
            fillable = TRUE,
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Species Column Complete",
              id = ns("speciesCompleteTextInfo"),
              shiny::markdown(
                "
                If TRUE the Species column is complete and contains no missing values.
                
                If FALSE the Species column contains missing values.

                "
              ),
              placement = "bottom"
            ),
            
            shiny::htmlOutput(outputId = ns("speciesCompleteText"))
            
          ),
          
          bslib::layout_columns(
            
            col_widths = c(1, 11),
            
            fill = FALSE,
            
            fillable = TRUE,
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Species Quadrat Duplicates",
              id = ns("speciesQuadratUniqueTextInfo"),
              shiny::markdown(
                "
                If TRUE all species are recorded only once per quadrat.
                
                If FALSE there are quadrats with a species recorded twice or more. Species must only be recorded once per quadrat.

                "
              ),
              placement = "bottom"
            ),
            
            shiny::htmlOutput(outputId = ns("speciesQuadratUniqueText"))
            
          ),
          
          bslib::layout_columns(
            
            col_widths = c(1, 11),
            
            fill = FALSE,
            
            fillable = TRUE,
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Quadrat Names Unique",
              id = ns("quadratIDUniqueTextInfo"),
              shiny::markdown(
                "
                If TRUE all quadrat names/IDs are unique within each year.
                
                If FALSE there are duplicate quadrat names/IDs within a year.

                "
              ),
              placement = "bottom"
            ),
            
            shiny::htmlOutput(outputId = ns("quadratIDUniqueText"))
            
          ),
          
          bslib::layout_columns(
            
            col_widths = c(1, 11),
            
            fill = FALSE,
            
            fillable = TRUE,
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Group Names Unique",
              id = ns("groupIDUniqueTextInfo"),
              shiny::markdown(
                "
                If TRUE all group names/IDs are unique within each year.
                
                If FALSE there are duplicate quadrat names/IDs within a year.

                "
              ),
              placement = "bottom"
            ),
            
            shiny::htmlOutput(outputId = ns("groupIDUniqueText"))
            
          )
          
        ),
        
        shiny::div(
          
          bslib::layout_columns(
            
            col_widths = c(1, 11),
            
            fill = FALSE,
            
            fillable = TRUE,
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "",
              id = ns("okToProceedTextInfo"),
              shiny::markdown(
                "
                If TRUE all of the required validation conditions are met and RMAVIS is ok to proceed with the analysis. The 'Run Analysis' button is enabled.
                
                If FALSE one or more of the required validation conditions are not met and RMAVIS is not ok to proceed with the analysis. The 'Run Analysis' button is disabled.

                "
              ),
              placement = "bottom"
            ),
            
            shiny::htmlOutput(outputId = ns("okToProceedText"))
            
          )
        )
        
      ),
      
      shiny::div(shiny::br()),
      
      shiny::h5("Species Adjustment Table"),
      
      shiny::div(shiny::br()),
      
      shiny::div(
        rhandsontable::rHandsontableOutput(outputId = ns("speciesAdjustmentTable")) # , height = "300px"
      ),
      
      shiny::div(shiny::br()),
      
      shiny::h5("Group Re-allocation Table"),
      
      shiny::div(shiny::br()),
      
      shiny::div(
        rhandsontable::rHandsontableOutput(outputId = ns("reallocateGroupsTable")) # , height = "300px"
      )
      
    )
  )
}
