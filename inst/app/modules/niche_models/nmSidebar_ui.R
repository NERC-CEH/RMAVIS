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
        
      ),
      
      bslib::accordion_panel(
        
        "Model Options", 
        
        icon = bsicons::bs_icon("bezier"),
        
        shiny::div(
          
          id = ns("selectedModels_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::selectizeInput(inputId = ns("selectedModels"), 
                                  label = "Selected Models", 
                                  choices = c("GAM", "NNet", "GLM", "RF", "MARS", "SVM", "XGB", "WE"), 
                                  selected = NULL, 
                                  multiple = TRUE),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Model Selection",
              id = ns("modelSelectionInfo"),
              shiny::markdown(
                "
              Select the models...
              "
              ),
              placement = "bottom"
            )
            
          ),
          
          shiny::div(shiny::br())
          
        ),
        
        shiny::div(
          
          id = ns("selectedVariables_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::selectizeInput(inputId = ns("selectedVariables"), 
                                  label = "Selected Variables", 
                                  choices = c("cov4", "fert", "mja", "mju", "ph", "prec", "wet"), 
                                  selected = NULL, 
                                  multiple = TRUE),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Variable Selection",
              id = ns("Variable Selection Info"),
              shiny::markdown(
                "
              Select the variables...
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
