deSidebarUI <- function(id){
  
  ns <- NS(id)
  
  bslib::sidebar(
    
    width = 320,
    
    shiny::h5("Options"),

# Survey Data -------------------------------------------------------------
    bslib::accordion(
      
      open = FALSE,
      
      bslib::accordion_panel(
        
        "Survey Data", 
        
        open = TRUE,
        
        icon = bsicons::bs_icon("clipboard-data"),
        
        shiny::div(
          
          id = ns("inputMethod_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::selectizeInput(inputId = ns("inputMethod"), 
                                  label = "Input Method", 
                                  choices = RMAVIS:::inputMethod_options, 
                                  selected = "none", 
                                  multiple = FALSE),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Input Method",
              id = ns("inputmethodInfo"),
              shiny::markdown(
                "
                Three input methods are provided:
                1. 'Manual entry'.
                2. 'Example'.
                3. 'Upload'.
                "
              ),
              placement = "bottom"
            )
            
          ),
          
          shiny::div(shiny::br())
          
          
        ),
        
        shiny::div(
          
          id = ns("exampleData_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::selectizeInput(inputId = ns("selectedExampleData"), 
                                  label = "Example Dataset", 
                                  choices = RMAVIS:::example_data_options, 
                                  selected = "none", 
                                  multiple = FALSE),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Example Dataset",
              id = ns("selectedExampleDataInfo"),
              shiny::markdown(
                "
                Four example datasets are currently provided:
                1. 'Parsonage Down'.
                2. 'Whitwell Common'.
                3. 'Leith Hill Place Wood'.
                4. 'Newborough Warren'.
                "
              ),
              placement = "bottom"
            )
            
          ),
          
          shiny::div(shiny::br())
          
        ),
        
        shiny::div(
          
          id = ns("uploadData_div"),
          
          shiny::div(shiny::h6("Upload Data")),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::actionButton(inputId = ns("uploadData"),
                                label = "Upload"),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Upload Data",
              id = ns("uploadDataInfo"),
              shiny::markdown(
                "
                Clicking the 'Upload' button opens a pop-up interface
                in which more details are provided.
                "
              ),
              placement = "bottom"
            )
          ),
          
          shiny::div(shiny::br())
          
        ),
        
        shiny::div(
          
          id = ns("coverScale_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::selectizeInput(inputId = ns("coverScale"),
                                  label = "Cover Scale",
                                  choices = RMAVIS:::coverScale_options,
                                  selected = "percentage",
                                  multiple = FALSE),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Cover Scale",
              id = ns("coverScaleInfo"),
              shiny::markdown(
                "
                Select the cover scale for data entry.
                
                At present four options are available:
                
                1. Percentage
                2. Proportional
                3. Domin
                4. Braun-Blanquet (5 point)
                
                The format of the cover values entered in the table
                will be checked against the selected cover scale
                in the validation module.
                
                "
              ),
              placement = "bottom"
            )
            
          ),
          
          shiny::div(shiny::br())
          
          
        ),

        shiny::div(
          
          id = ns("validatesurveyData_div"),
          
          shiny::div(shiny::h6("Validation")),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::actionButton(inputId = ns("validatesurveyData"),
                                label = "Validate Survey Data"),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Validate Survey Table Data",
              id = ns("validatesurveyDataInfo"),
              shiny::markdown(
                "
                Open a popup window to validate the data present in the Survey Data Table.
                All validation checks must pass before the 'Run Analysis' button is enabled
                and RMAVIS is ok to proceed.
                "
              ),
              placement = "bottom"
            )
            
          ),
          
          shiny::div(shiny::br())
          
        ),
        
        shiny::div(
          
          id = ns("clearTable_div"),
          
          shiny::div(shiny::h6("Clear Survey Data Table")),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::actionButton(inputId = ns("clearTable"),
                                label = "Clear"),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Clear Survey Data Table",
              id = ns("clearTableInfo"),
              shiny::markdown(
                "
                Clear the survey data table.
                "
              ),
              placement = "bottom"
            )
            
          )
          
        )
        
      ),
      
      # Download Options --------------------------------------------------------
      bslib::accordion_panel(
        
        "Download", 
        
        icon = bsicons::bs_icon("download"),
        
        ## Download Accepted Taxa -----------------------------------------------
        shiny::div(
          
          id = ns("downloadAcceptedTaxa_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::downloadButton(
              outputId = ns("downloadAcceptedTaxa"),
              label = "Accepted Taxa",
              class = NULL,
              icon = NULL
            ),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Download Accepted Taxa Data",
              shiny::markdown(
                "
                Download a csv containing the taxon names accepted by RMAVIS.
                "
              ),
              placement = "bottom"
            )
          )
        ),
        
        shiny::div(shiny::br()),
        
        ## Download Taxonomic Backbone -----------------------------------------------
        shiny::div(
          
          id = ns("downloadTaxonomicBackbone_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::downloadButton(
              outputId = ns("downloadTaxonomicBackbone"),
              label = "Taxonomic Backbone",
              class = NULL,
              icon = NULL
            ),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Download Taxonomic Backbone",
              shiny::markdown(
                "
                Download a csv containing the taxonomic backbone used by RMAVIS.
                "
              ),
              placement = "bottom"
            )
          )
        ),
        
        shiny::div(shiny::br()),
        
        ## Download Taxon Lookup -----------------------------------------------
        shiny::div(
          
          id = ns("downloadTaxonLookup_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::downloadButton(
              outputId = ns("downloadTaxonLookup"),
              label = "Taxon Lookup",
              class = NULL,
              icon = NULL
            ),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Download Taxon Lookup",
              shiny::markdown(
                "
                Download a csv containing a lookup between the accepted taxa and the synonyms associated with the accepted taxa.
                "
              ),
              placement = "bottom"
            )
          )
        ),
        
        shiny::div(shiny::br()),
        
        ## Download Survey Data ----------------------------------------------------
        shiny::div(
          
          id = ns("downloadSurveyData_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            downloadButton(
              outputId = ns("downloadSurveyData"),
              label = "Survey Data",
              class = NULL,
              icon = NULL
            ),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Download Survey Data",
              shiny::markdown(
                "
                Download the survey data displayed in the 'Survey Data' section of RMAVIS.
                This data will contain any changes made in the survey data validation process
                and allow reproduction of the results of the current RMAVIS session
                at a later date.
                "
              ),
              placement = "bottom"
            )
          )
        )
        
      ) # Close Download Options
      
    )

  )
  
}
