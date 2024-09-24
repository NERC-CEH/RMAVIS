nmSidebarUI <- function(id){
  
  ns <- NS(id)
  
  bslib::sidebar(
    
    width = 320,
    
    shiny::h5("Options"),
    

# Run Analysis ------------------------------------------------------------
    shiny::actionButton(inputId = ns("runNMAnalysis"),
                        label = "Run Analysis"
                        ),

# Species Selection -------------------------------------------------------
    bslib::accordion(
      
      open = FALSE,
      
      bslib::accordion_panel(
        
        "Species Selection", 
        
        icon = bsicons::bs_icon("flower1"),
        
        shiny::div(
          
          id = ns("speciesSelection_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::selectizeInput(inputId = ns("focalSpecies"), 
                                  label = "Focal Species", 
                                  choices = tibble::deframe(RMAVIS::acceptedSpecies)[tibble::deframe(RMAVIS::acceptedSpecies) %in% modelled_species], 
                                  selected = NULL, 
                                  multiple = FALSE),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Species Selection",
              id = ns("speciesSelectionInfo"),
              shiny::markdown(
                "
                    Select the species to view and use the ENM.
                "
              ),
              placement = "bottom"
            )
            
          ),
          
          shiny::div(shiny::br())
          
          
        )
        
      )
      
    )

  ) # close sidebar
  
}
