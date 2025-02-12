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
        
        "Model Display", 
        
        icon = bsicons::bs_icon("eyeglasses"),
        
        # shiny::div(
        #   
        #   id = ns("selectedModelsDisplay_div"),
        #   
        #   bslib::layout_columns(
        #     
        #     col_widths = c(11, 1),
        #     
        #     shiny::selectizeInput(inputId = ns("selectedModelDisplay"), 
        #                           label = "Model", 
        #                           choices = c("GAM", "NNet", "GLM", "RF", "MARS", "SVM", "XGB", "WE"), 
        #                           selected = c("GAM", "NNet", "GLM", "RF", "MARS", "SVM", "XGB", "WE"), 
        #                           multiple = TRUE),
        #     
        #     bslib::popover(
        #       bsicons::bs_icon("info-circle"),
        #       title = "Model Selection",
        #       id = ns("modelSelectionInfo"),
        #       shiny::markdown(
        #         "
        #       Select the models to inspect.
        #       "
        #       ),
        #       placement = "bottom"
        #     )
        #     
        #   ),
        #   
        #   shiny::div(shiny::br())
        #   
        # ),
        
        shiny::div(

          id = ns("selectedVariablesDisplay"),
          
          bslib::layout_columns(

            col_widths = c(11, 1),

            shiny::selectizeInput(inputId = ns("selectedVariablesDisplay"),
                                  label = "Variables",
                                  choices = c("F", "N", "R", "S", "DG", "DS", "L", "Tmin01", "Tmax07", "MAP"),
                                  selected = c("F", "N", "R", "S", "DG", "DS", "L", "Tmin01", "Tmax07", "MAP"),
                                  multiple = TRUE),

            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Variable Selection",
              id = ns("Variable Selection Info"),
              shiny::markdown(
                "
              Select the variables to inspect.
              "
              ),
              placement = "bottom"
            )

          ),

          shiny::div(shiny::br())

        ),
        
        shiny::div(
          
          id = ns("selectedMarginalEffectsPlot_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::selectizeInput(inputId = ns("selectedMarginalEffectsPlot"),
                                  label = "Marginal Effects Plot",
                                  choices = c("ALE", "PDP", "CP"),
                                  selected = "ALE",
                                  multiple = FALSE),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Marginal Effects Plot Selection",
              id = ns("selectedMarginalEffectsPlot_info"),
              shiny::markdown(
              "
              Select the marginal effects plot to view. Three options are available:
              
              1) **Accumulated Local Effects Plot (ALE)**. Broadly, this can be interpreted as *the effect of a given variable on the total probability of the species occurence.*
              
              2) **Partial Dependence Plot (PDP)**. Broadly, this can be interpreted as *...*
              
              3) **Ceritus Paribus Plot (CP)**. Broadly, this can be interpreted as *the probability of occurrence for that variable value, with all other variables held constant.*
              
              Please see the documentation for more details.
              "
              ),
              placement = "bottom"
            )
            
          ),
          
          shiny::div(shiny::br())
          
        )
        
      ),
      
      bslib::accordion_panel(
        
        "Model Prediction", 
        
        icon = bsicons::bs_icon("bezier"),
        
        shiny::div(
          
          id = ns("identifyPredDrivers_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shinyWidgets::switchInput(inputId = ns("identifyPredDrivers"),
                                      label = "Drivers",
                                      value = TRUE,
                                      onLabel = "Yes",
                                      offLabel = "No"),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Identify Prediction Drivers",
              id = ns("identify_prediction_drivers_info"),
              shiny::markdown(
              "
              Optionally identify the effects of each individual variable on driving the model prediction result/s.
              Please note that this may take a long period of time, 
              particularly for multiple model runs and when using the Weighted Ensemble.
              "
              ),
              placement = "bottom"
            )
            
          ),
          
          shiny::div(shiny::br())
          
        )#,
        
        # shiny::div(
        #   
        #   id = ns("selectedModelPredict_div"),
        #   
        #   bslib::layout_columns(
        #     
        #     col_widths = c(11, 1),
        #     
        #     shiny::selectizeInput(inputId = ns("selectedModelPredict"), 
        #                           label = "Selected Model", 
        #                           choices = c("GAM", "NNet", "GLM", "RF", "MARS", "SVM", "XGB", "WE"), 
        #                           selected = "WE", 
        #                           multiple = FALSE),
        #     
        #     bslib::popover(
        #       bsicons::bs_icon("info-circle"),
        #       title = "Model Selection",
        #       id = ns("modelSelectionInfo"),
        #       shiny::markdown(
        #         "
        #       Select the model to use when generating predictions.
        #       "
        #       ),
        #       placement = "bottom"
        #     )
        #     
        #   ),
        #   
        #   shiny::div(shiny::br())
        #   
        # )
        
      )
      
    )

  ) # close sidebar
  
}
