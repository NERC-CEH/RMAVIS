sidebarUI <- function(id){
  
  ns <- NS(id)
  
  bslib::sidebar(
    
    width = 320,
    
    shiny::h5("Options"),
    

# Run Analysis ------------------------------------------------------------
    shiny::actionButton(inputId = ns("runAnalysis"),
                        label = "Run Analysis"#,
                        # disabled = ''
                        ),

    

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
            
            shiny::selectizeInput(inputId = ns("exampleData"), 
                                  label = "Example Data", 
                                  choices = example_data_options, 
                                  selected = "none", 
                                  multiple = FALSE),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Example Data",
              id = ns("exampleDataInfo"),
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
              shiny::markdown(
                "
                Open a popup window to validate the data present in the Survey Data Table.
                All validation checks must pass before the 'Run Analysis' button is enabled
                and pseudoMAVIS is ok to proceed.
                "
              ),
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
              shiny::markdown(
                "
                Three sets of NVC assigment results are currently available:
                - 'Site, Pseudo-quadrat'
                - 'Group, Pseudo-quadrat'
                - 'Quadrat, Pseudo-quadrat'
                "
              ),
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
              shiny::markdown(
                "
                Optionally restrict the NVC assignment process to one or more
                broad NVC habitat types. This is reccomended to increase the
                assignment speed, but only the site being
                analysed unequivocally conforms to the selected NVC habitats.
                "
              ),
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
              shiny::markdown(
                "
                Select the number of top results to display per Site, Group, or
                Quadrat.
                "
              ),
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
              shiny::markdown(
                "
                Select a habitat classification you wish to retrieve correspondence,
                values for using the fitted NVC communities and sub-communities.
                Note that all community level codes associated with sub-communities
                are also used, even if they aren't directly assigned. This is to account for the,
                incomplete coverage of NVC sub-communities in the JNCC habitat correspondences.
                "
              ),
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
              shiny::markdown(
                "
                Restrict the NVC communities available for selection in the
                'NVC Table' option below to the top-fitting NVC communities
                as returned in the 'NVC Assignment' tab.
                "
              ),
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
                                  choices = NULL, 
                                  selected = "A1", 
                                  multiple = FALSE),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "NVC Community",
              shiny::markdown(
                "
                Select an NVC community, the floristic table of which will be
                displayed alongside the composed floristic table.
                "
              ),
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
              shiny::markdown(
                "
                Select one from the list of floristic tables composed from the survey
                data.
                "
              ),
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
              title = "Match Species",
              shiny::markdown(
                "
                Three options for arranging the composed and NVC floristic tables
                are provided:
                - 'No': Displayes the tables side-by-side, ordered by Constancy.
                - 'Composed to NVC': Aligns the species in the composed table that
                are present in the selected NVC community with the NVC floristic table.
                Omits species which are not present in the NVC community.
                - 'NVC to Composed': Aligns the species in the NVC floristic table that
                are present in the composed table with the composed table. Omits
                species which are not present in the composed table.
                "
              ),
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
              shiny::markdown(
                "
                Select the Ecological Indicator Value (EIV) results to view.
                Six options are currently provided.
                - 'Unweighted Mean Hill-Ellenberg Values, by Site':
                - 'Weighted Mean Hill-Ellenberg Values, by Site':
                - 'Unweighted Mean Hill-Ellenberg Values, by Group':
                - 'Weighted Mean Hill-Ellenberg Values, by Group':
                - 'Unweighted Mean Hill-Ellenberg Values, by Quadrat':
                - 'Weighted Mean Hill-Ellenberg Values, by Quadrat':
                "
              ),
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
              shiny::markdown(
                "
                Select the diveristy metric results to view.
                Five options are currently provided.
                - 'Site Summary Table':
                - 'Quadrat Diversity Indices Table':
                - 'Species Richness, by Site':
                - 'Species Richness, by Group':
                - 'Species Richness, by Quadrat':
                "
              ),
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
              shiny::markdown(
                "
                Select the Detrended Correspondence Analysis (DCA) axis scores
                to display, all combinations of the first three axes are available:
                - 'DCA1 vs DCA2'
                - 'DCA1 vs DCA3'
                - 'DCA2 vs DCA3'
                "
              ),
              placement = "bottom"
            )
            
          ),
          
          shiny::div(shiny::br())
          
        ),
        
        shiny::div(
          
          id = ns("nationalReferenceSpaces_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::selectizeInput(inputId = ns("nationalReferenceSpaces"),
                                  label = "National Reference Spaces",
                                  choices = nationalReferenceSpaces_options,
                                  selected = NULL,
                                  multiple = TRUE),
            
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "National Reference Spaces",
              shiny::markdown(
                "
                Select the NVC communities to display the national reference spaces
                for. By default the top fitting NVC communities are displayed.
                Please see the documentation for a definition of the
                'National Reference Spaces' along with interpretation guidance.
                "
              ),
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
              shiny::markdown(
                "
                Select the survey quadrat selection method. Three options are 
                provided:
                - 'Select Years':
                - 'Select Groups':
                - 'Select Quadrats':
                "
              ),
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
              shiny::markdown(
                "
                Select survey quadrats for one or more of the years present in 
                the survey data.
                "
              ),
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
              shiny::markdown(
                "
                Select survey quadrats for one or more of the groups present in 
                the survey data.
                "
              ),
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
              shiny::markdown(
                "
                Select survey quadrats for one or more of the quadrats present in 
                the survey data.
                "
              ),
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
              shiny::markdown(
                "
                Select the variables with which to perform a 
                Constrained Correspondence Analysis (CCA).
                "
              ),
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
              shiny::markdown(
                "
                Select the variables to display in the multivariate analysis plots:
                
                Seven options are provided:
                - 'Survey Quadrats': the DCA scores of the survey quadrats.
                - 'Pseudo-Quadrats': the DCA scores of the pseudo-quadrats (Local Reference only).
                - 'Reference Space': the convex hulls formed from the pseudo-quadrat DCA scores.
                - 'Species': the DCA scores of the species.
                - 'Unique Survey Species': the DCA scores of the species unique to the survey data, but absent from the best fitting NVC communities pseudo-quadrats (Local Reference (unrestricted) only).
                - 'Hill-Ellenberg': the CCA result axis scores for the Hill-Ellenberg selected in the 'CCA Variables' option.
                - 'Survey Quadrat Change': arrows drawn between each quadrat by year, showing the movement of the quadrat in the ordination space.
                
                Please see the documentation for more details.
                "
              ),
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
              shiny::markdown(
                "
                Please enter the name of the person/s using pseudoMAVIS, for attribution in the report.
                "
              ),
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
              shiny::markdown(
                "
                Please enter the name of the project/site being analysed in pseudoMAVIS.
                "
              ),
              placement = "bottom"
            )
          ),
          
          shiny::div(shiny::br())
          
        ),
        
        shiny::div(
          
          id = ns("reportOptions_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shinyWidgets::pickerInput(inputId = ns("reportOptions"),
                                      label = "Report Options",
                                      choices = reportOptions_options,
                                      selected = c("nvcAssignmentResultsSite", 
                                                   "composedFloristicTablesSite", 
                                                   "speciesFrequencyTable"),
                                      options = shinyWidgets::pickerOptions(
                                        dropdownAlignRight = TRUE#,
                                        # showContent = FALSE
                                      ),
                                      choicesOpt = list(
                                        style = rep_len("font-size: 75%; line-height: 1.6;", length(unlist(reportOptions_options)))
                                      ),
                                      multiple = TRUE
            ),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Report Options",
              shiny::markdown(
                "
                Select the pseudoMAVIS outputs to include in the report.
                
                Please note that selecting the 'Survey Table' option may produce
                a long report if a large quantity of data is submitted to
                pseudoMAVIS.
                "
              ),
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
              icon = NULL#,
              # disabled = ''
            ),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Download Report",
              shiny::markdown(
                "
                Download a pdf report of the current pseudoMAVIS analyses results.
                "
              ),
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
        
        shiny::div(
          
          id = ns("downloadSpeciesData_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::downloadButton(
              outputId = ns("downloadSpeciesData"),
              label = "Download Accepted Species",
              class = NULL,
              icon = NULL
            ),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Download Accepted Species Data",
              shiny::markdown(
                "
                Download a csv containing the species names accepted by pseudoMAVIS.
                "
              ),
              placement = "bottom"
            )
          )
        ),
        
        shiny::div(shiny::br()),
        
        shiny::div(
          
          id = ns("downloadSurveyData_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            downloadButton(
              outputId = ns("downloadSurveyData"),
              label = "Download Survey Data",
              class = NULL,
              icon = NULL
            ),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Download Survey Data",
              shiny::markdown(
                "
                Download the survey data displayed in the 'Survey Data' section of pseudoMAVIS.
                This data will contain any changes made in the survey data validation process
                and allow reproduction of the results of the current pseudoMAVIS session
                at a later date.
                "
              ),
              placement = "bottom"
            )
          )
        )
      )
    )
  )
  
}
