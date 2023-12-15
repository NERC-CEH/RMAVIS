sidebarUI <- function(id){
  
  ns <- NS(id)
  
  bslib::sidebar(
    
    width = 320,
    
    shiny::h5("Options"),
    

# Run Analysis ------------------------------------------------------------
    shiny::actionButton(inputId = ns("runAnalysis"),
                        label = "Run Analysis"),

    

# Survey Data -------------------------------------------------------------
    bslib::accordion(
      
      open = FALSE,
      
      bslib::accordion_panel(
        
        "Survey Data", 
        
        icon = bsicons::bs_icon("clipboard-data"),
        
        shiny::div(
          
          id = ns("inputMethod_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::selectizeInput(inputId = ns("inputMethod"), 
                                  label = "Input Method", 
                                  choices = inputMethod_options, 
                                  selected = "none", 
                                  multiple = FALSE),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Input Method",
              id = ns("inputmethodInfo"),
              paste0(""),
              placement = "bottom"
            )
            
          ),
          
          shiny::div(shiny::br())
          
          
        ),
        
        shiny::div(
          
          id = ns("exampleData_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::selectizeInput(inputId = ns("exampleData"), 
                                  label = "Example Data", 
                                  choices = example_data_options, 
                                  selected = "none", 
                                  multiple = FALSE),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Example Data",
              id = ns("exampleDataInfo"),
              paste0(""),
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
            
            # shiny::fileInput(inputId = ns("uploadDataInput"),
            #                  label = "browse",
            #                  accept = c("text/csv",
            #                             "text/comma-separated-values,text/plain",
            #                             ".csv"
            #                  )
            # ),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Upload Data",
              id = ns("uploadDataInfo"),
              paste0(""),
              placement = "bottom"
            )
          ),
          
          shiny::div(shiny::br())
          
        ),

        shiny::div(
          
          id = ns("validateSurveyTable_div"),
          
          shiny::div(shiny::h6("Validation")),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::actionButton(inputId = ns("validateSurveyTable"),
                                label = "Validate Survey Data"),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Validate Survey Table Data",
              id = ns("validateSurveyTableInfo"),
              paste0("Open a popup window to validate the data present in the Survey Data Table.
                    All validation checks must pass before the 'Run Analysis' button is enabled
                    and pseudoMAVIS is ok to proceed."),
              placement = "bottom"
            )
            
          )
        )
        
      ),
      

# NVC Assignment ----------------------------------------------------------
      bslib::accordion_panel(
        
        "NVC Assignment", 
        
        icon = bsicons::bs_icon("ui-checks-grid"),
        
        # shiny::selectizeInput(inputId = ns("nvcAssignMethods"),
        #                       label = "Methods",
        #                       choices = nvcAssignMethods_options,
        #                       selected = "pseudoQuadratSite",
        #                       multiple = TRUE),
        
        shiny::div(
          
          id = ns("resultsViewNVCAssign_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::selectizeInput(inputId = ns("resultsViewNVCAssign"),
                                  label = "Results to View",
                                  choices = resultsViewNVCAssign_options,
                                  selected = c("nvcAssignSitePseudo"),
                                  multiple = FALSE),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Results to View",
              id = ns("resultsToViewNVCAssignInfo"),
              paste0(""),
              placement = "bottom"
            )
            
          ),
          
          shiny::div(shiny::br())
          
        ),
        
        shiny::div(
          
          id = ns("restrictHabitatInfo_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::selectizeInput(inputId = ns("habitatRestriction"),
                                  label = "Restrict Habitat",
                                  choices = habitatRestriction_options,
                                  selected = NULL,
                                  multiple = TRUE
            ),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Restrict Habitat",
              id = ns("restrictHabitatInfo"),
              paste0(""),
              placement = "bottom"
            )
          ),
          
          shiny::div(shiny::br())
          
        ),
        
        shiny::div(
          
          id = ns("nTopResults_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::selectizeInput(inputId = ns("nTopResults"), 
                                  label = "Number of Top Results", 
                                  choices = c(1:10), 
                                  selected = 5, 
                                  multiple = FALSE),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Number of Top Results",
              id = ns("nTopResultsInfo"),
              paste0(""),
              placement = "bottom"
            )
          )
          
        )
        
        
      ),
      

# Habitat Correspondence --------------------------------------------------
      bslib::accordion_panel(
        
        "Habitat Correspondence", 
        
        icon = bsicons::bs_icon("sliders"),
        
        shiny::div(
          
          id = ns("habCorClass_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::selectizeInput(inputId = ns("habCorClass"),
                                  label = "Classification",
                                  choices = all_habCor_classifications,
                                  selected = "UKHab - Level5",
                                  multiple = FALSE),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Classification",
              paste0("Select a habitat classification you wish to retrieve correspondence",
                     " values for using the fitted NVC communities and sub-communities.",
                     " Note that all community level codes associated with sub-communities",
                     " are also used, even if they aren't fitted. This is to account for the",
                     " incomplete coverage of NVC sub-communities in the JNCC habitat correspondences."),
              placement = "bottom"
            )
          )
        )
        
      ),
      

# Floristic Tables --------------------------------------------------------
      bslib::accordion_panel(
        
        "Floristic Tables", 
        
        icon = bsicons::bs_icon("table"),
        
        shiny::div(
          
          id = ns("restrictNVCFlorTablesOpts_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::checkboxInput(inputId = ns("restrictNVCFlorTablesOpts"),
                                 label = "Restrict",
                                 value = TRUE),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Restrict",
              paste0("Restrict the NVC community option below",
                     " to the fitted NVC communities."),
              placement = "bottom"
            )
            
          ),
          
          shiny::div(shiny::br())
          
        ),
        
        shiny::div(
          
          id = ns("nvcFloristicTable_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::selectizeInput(inputId = ns("nvcFloristicTable"), 
                                  label = "NVC Table", 
                                  choices = nvc_community_codes, 
                                  selected = "A1", 
                                  multiple = FALSE),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "NVC Community",
              paste0("Select a NVC community, the floristic table of which will be",
                     "  displayed alongside the composed floristic table."),
              placement = "bottom"
            )
            
          ),
          
          shiny::div(shiny::br())
          
        ),
        
        shiny::div(
          
          id = ns("composedFloristicTable_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::selectizeInput(inputId = ns("composedFloristicTable"), 
                                  label = "Composed Table", 
                                  choices = NULL,
                                  selected = NULL,
                                  # choices = c("2023 - Rothiemurchus & ROTHIEM"), # NULL,
                                  # selected = "2023 - Rothiemurchus & ROTHIEM", # NULL,
                                  multiple = TRUE,
                                  options = list(maxItems = 1)),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Composed Table",
              paste0("Select a year/group/quadrat ID to compose a floristic table."),
              placement = "bottom"
            )
            
          ),
          
          shiny::div(shiny::br())
          
        ),
        
        
        shiny::div(
          
          id = ns("matchSpecies_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::selectizeInput(inputId = ns("matchSpecies"), 
                                  label = "Match Species",
                                  choices = matchSpecies_options, 
                                  selected = "", 
                                  multiple = FALSE),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Cross Tabulate",
              paste0("Select one of three options:"),
              placement = "bottom"
            )
            
          )
        )
        
      ),


# Frequency ---------------------------------------------------------------
      bslib::accordion_panel(
        
        "Frequency", 
        
        icon = bsicons::bs_icon("graph-up-arrow")
        
        
      ),


# EIVs --------------------------------------------------------------------
      bslib::accordion_panel(
        
        "EIVs", 
        
        icon = bsicons::bs_icon("water"),
        
        shiny::div(
          
          id = ns("resultsViewEIVs_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::selectizeInput(inputId = ns("resultsViewEIVs"),
                                  label = "Results to View",
                                  choices = resultsViewEIVs_options,
                                  selected = c("unweightedMeanHEValuesSite"),
                                  multiple = FALSE),
            
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Results to View",
              paste0(""),
              placement = "bottom"
            )
        
          )
        )
        
      ),


# Diversity ---------------------------------------------------------------
      bslib::accordion_panel(
        
        "Diversity", 
        
        icon = bsicons::bs_icon("tree"),
        
        shiny::div(
          
          id = ns("resultsViewDiversity_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::selectizeInput(inputId = ns("resultsViewDiversity"),
                                  label = "Results to View",
                                  choices = resultsViewDiversity_options,
                                  selected = c("diversitySummaryTable"),
                                  multiple = FALSE),
            
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Results to View",
              paste0(""),
              placement = "bottom"
            )
          
          )
        )
        
      ),
      

# MVA ---------------------------------------------------------------------
      bslib::accordion_panel(
        
        "MVA", 
        
        icon = bsicons::bs_icon("arrows-angle-expand"),
        
        shiny::div(
          
          id = ns("dcaAxisSelection_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::selectizeInput(inputId = ns("dcaAxisSelection"),
                                  label = "Axis Selection",
                                  choices = dcaAxisSelection_options,
                                  selected = "dca1dca2",
                                  multiple = FALSE),
            
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Axis Selection",
              paste0(""),
              placement = "bottom"
            )
            
          ),
          
          shiny::div(shiny::br())
          
        ),
        
        shiny::div(
          
          id = ns("globalReferenceSpaces_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::selectizeInput(inputId = ns("globalReferenceSpaces"),
                                  label = "Global Reference Spaces",
                                  choices = globalReferenceSpaces_options,
                                  selected = NULL,
                                  multiple = TRUE),
            
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Global Reference Spaces",
              paste0(""),
              placement = "bottom"
            )
            
          ),
          
          shiny::div(shiny::br())
          
        ),
        
        shiny::div(
          
          id = ns("selectSurveyMethod_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::selectizeInput(inputId = ns("selectSurveyMethod"),
                                  label = "Survey Quadrat Selection",
                                  choices = surveyQuadratSelection_options,
                                  selected = NULL,
                                  multiple = FALSE),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Survey Quadrat Selection",
              paste0(""),
              placement = "bottom"
            )
            
          ),
          
          shiny::div(shiny::br())
          
        ),
        
        shiny::div(
          
          id = ns("selectSurveyYears_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::selectizeInput(inputId = ns("selectSurveyYears"),
                                  label = "Select Survey Years",
                                  choices = selectSurveyYears_options,
                                  selected = NULL,
                                  multiple = TRUE,
                                  options = list(minItems = 1)),
            
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Select Survey Years",
              paste0(""),
              placement = "bottom"
            )
            
          ),
          
          shiny::div(shiny::br())
          
        ),
        
        shiny::div(
          
          id = ns("selectSurveyGroups_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::selectizeInput(inputId = ns("selectSurveyGroups"),
                                  label = "Select Survey Groups",
                                  choices = selectSurveyGroups_options,
                                  selected = c("F", "N"),
                                  multiple = TRUE,
                                  options = list(minItems = 1)),
            
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Select Survey Groups",
              paste0(""),
              placement = "bottom"
            )
            
          ),
          
          shiny::div(shiny::br())
          
        ),
        
        shiny::div(
          
          id = ns("selectSurveyQuadrats_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::selectizeInput(inputId = ns("selectSurveyQuadrats"),
                                  label = "Select Survey Quadrats",
                                  choices = selectSurveyQuadrats_options,
                                  selected = NULL,
                                  multiple = TRUE,
                                  options = list(minItems = 1)),
            
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Select Survey Quadrats",
              paste0(""),
              placement = "bottom"
            )
            
          ),
          
          shiny::div(shiny::br())
          
        ),
        
        shiny::div(
          
          id = ns("ccaVars_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::selectizeInput(inputId = ns("ccaVars"),
                                  label = "CCA Variables",
                                  choices = ccaVars_options,
                                  selected = "Moisture (F) x Nitrogen (N)",
                                  multiple = FALSE),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "CCA",
              paste0(""),
              placement = "bottom"
            )
            
          ),
          
          shiny::div(shiny::br())
          
        ),
        
        shiny::div(
          
          id = ns("dcaVars_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::checkboxGroupInput(inputId = ns("dcaVars"),
                                      label = "Axis Scores",
                                      choices = dcaVars_options,
                                      selected = c("referenceSpace", "surveyQuadrats", "hillEllenberg")),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "DCA",
              paste0(""),
              placement = "bottom"
            )
          )
        )

      ),


# Report Options ----------------------------------------------------------
      bslib::accordion_panel(
        
        "Report", 
        
        icon = bsicons::bs_icon("filetype-pdf"),
        
        shiny::div(
          
          id = ns("reportAuthorName_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::textInput(inputId = ns("reportAuthorName"),
                             label = "Author"),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Report Author",
              paste0(""),
              placement = "bottom"
            )
          ),
          
          shiny::div(shiny::br())
          
          
        ),
        
        shiny::div(
          
          id = ns("reportProjectName_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::textInput(inputId = ns("reportProjectName"),
                             label = "Project Name"),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Project Name",
              paste0(""),
              placement = "bottom"
            )
          ),
          
          shiny::div(shiny::br())
          
        ),
        
        shiny::div(
          
          id = ns("generateReport_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::downloadButton(
              outputId = ns("generateReport"),
              label = "Download Report",
              class = NULL,
              icon = NULL
            ),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Download Report",
              paste0(""),
              placement = "bottom"
            )
          ),
          
          shiny::div(shiny::br())
          
        ),
        
        shiny::div(
          
          id = ns("reportOptions_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::checkboxGroupInput(inputId = ns("reportOptions"),
                                      label = "Report Options",
                                      choices = reportOptions_options,
                                      selected = c("nvcAssignmentResultsSite", 
                                                   "composedFloristicTablesSite", 
                                                   "speciesFrequencyTable")
            ),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Report Options",
              paste0(""),
              placement = "bottom"
            )
          )
        )
        
        # reportUI(id = ns("report_id_1"))
        
      ),

# Download Options --------------------------------------------------------
      bslib::accordion_panel(
        
        "Download", 
        
        icon = bsicons::bs_icon("download"),
        
        shiny::downloadButton(
          outputId = ns("downloadSpeciesData"),
          label = "Download Accepted Species",
          class = NULL,
          icon = NULL
        )

      )
    )
  )
  
}
