sidebarUI <- function(id){
  
  ns <- NS(id)
  
  bslib::sidebar(
    
    width = 320,
    
    shiny::h5("Options"),
    

# Run Analysis ------------------------------------------------------------
    shiny::actionButton(inputId = ns("runAnalysis"),
                        label = "Run Analysis",
                        disabled = ''
                        ),

    # shiny::div(shiny::br()),

    shiny::div(
      
      id = ns("selectNVCtypes_div"),
      
      shiny::h5("NVC Types"),
      
      bslib::layout_columns(
        
        col_widths = c(11, 1),
        
        shinyWidgets::pickerInput(inputId = ns("selectNVCtypes"),
                                  label = NULL,
                                  choices = RMAVIS:::nvcType_options,
                                  selected = c("Original"),
                                  multiple = TRUE
        ),
        
        bslib::popover(
          bsicons::bs_icon("info-circle"),
          title = "Select NVC types",
          id = ns("selectNVCtypesInfo"),
          shiny::markdown(
            "
            Select the NVC unit types for analysis.
            At present three sets of units are available:
            -  Original, which contains the NVC communities as they appear originally in the NVC volumes (with updated taxonomy).
            -  Calthion, which contains the wet mesotrophic grassland communities as described in Wallace and Prosser (2017) and Prosser et al (2023).
            -  SOWG, which contains the Scottish Oceanic Wet Grassland communities as described in Wallace et al (2023).
            "
          ),
          placement = "bottom"
        )
        
      )
      
    ),

    bslib::accordion(
      
      open = FALSE,

# NVC Assignment ----------------------------------------------------------
      bslib::accordion_panel(
        
        "NVC Assignment", 
        
        icon = bsicons::bs_icon("ui-checks-grid"),
        
        shiny::div(
          
          id = ns("assignQuadrats_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shinyWidgets::switchInput(inputId = ns("assignQuadrats"),
                                      label = "Assign",
                                      value = FALSE,
                                      onLabel = "Yes",
                                      offLabel = "No",
                                      disabled = TRUE),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Assign Quadrats",
              id = ns("assignQuadratsInfo"),
              shiny::markdown(
                "
                Toggle whether individual quadrats in the survey data are assigned
                NVC units.
                
                This option is set to 'Yes' and disabled if the number of 
                quadrats for any year is less than 2, as similarities for the 
                Site and Groups by year will not be calculated using the 
                Czekanowski coefficient of similarity and so similarity values 
                must be calculated using the Jaccard coefficient and reference
                pseudo-quadrats.
                
                If the number of quadrats for all years is 2 or greater the
                button is enabled and the user may optionally choose to
                calculate similarities for individual quadrats.
                
                "
              ),
              placement = "bottom"
            )
            
          ),
          
          shiny::div(shiny::br())
          
          
        ),
        
        shiny::div(
          
          id = ns("removeLowFreqTaxa_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shinyWidgets::switchInput(inputId = ns("removeLowFreqTaxa"),
                                      label = "Remove",
                                      value = TRUE,
                                      onLabel = "Yes",
                                      offLabel = "No",
                                      disabled = FALSE),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Remove Low Frequency Taxa",
              id = ns("removeLowFreqTaxaInfo"),
              shiny::markdown(
                "
                In the NVC taxa which occurred in less than 5% of the plots which constituted each NVC unit were removed from the final floristic tables.
                
                Consequently, by default taxa which occur in less than 5% of the plots in each Group in the survey data are also removed.
                
                At present this does not effect the individual plot Jaccard similarity calculations, 
                but does effect the Group and Year Czekanowski similarities and composition of the Floristic tables.
                
                This ensures that similarities are not biased towards more species-rich communities; 
                however, conversely, if the number of survey quadrats is lower than the number of plots used to define a NVC unit, 
                particulary a species rich unit (e.g. CG2b), it may be preferable to not remove low frequency taxa to ensure that there is not a bias towards more species-poor communities.
                It is left to the user to make this decision, though in practice the results usually only display minor variations.
                "
              ),
              placement = "bottom"
            )
            
          ),
          
          shiny::div(shiny::br())
          
          
        ),
        
        shiny::div(
          
          id = ns("resultsViewNVCAssign_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::selectizeInput(inputId = ns("resultsViewNVCAssign"),
                                  label = "Results to View",
                                  choices = RMAVIS:::resultsViewNVCAssign_options,
                                  selected = c("nvcAssignSiteCzekanowski"),
                                  multiple = FALSE),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Results to View",
              id = ns("resultsToViewNVCAssignInfo"),
              shiny::markdown(
                "
                Three sets of NVC assigment results are currently available:
                -  Quadrat, Jaccard
                -  Site, Czekanowski
                -  Group, Czekanowski
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
                                  choices = RMAVIS:::habitatRestriction_options,
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
                broad NVC habitat types. This is recommended to increase the
                assignment speed, but only if the site being
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
                                  choices = RMAVIS:::habitat_correspondence_classifications,
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
          
          id = ns("floristicTablesViewOpts_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::selectizeInput(inputId = ns("floristicTablesView"), 
                                  label = "View Options", 
                                  choices = RMAVIS:::floristicTablesView_options, 
                                  selected = "singleComposedVsNVC", 
                                  multiple = FALSE),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "View Options",
              shiny::markdown(
                "
                Select a set of floristic tables to view, one of two options:
                1. Single Composed vs NVC
                2. Multiple Composed
                "
              ),
              placement = "bottom"
            )
            
          ),
          
          shiny::div(shiny::br())
          
        ),
        
        shiny::div(
          
          id = ns("floristicTablesSetViewOpts_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::selectizeInput(inputId = ns("floristicTablesSetView"), 
                                  label = "View Options", 
                                  choices = RMAVIS:::floristicTablesSetView_options, 
                                  selected = "all", 
                                  multiple = FALSE),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "View Options",
              shiny::markdown(
                "
                Select a set of composed floristic tables to view.
                "
              ),
              placement = "bottom"
            )
            
          ),
          
          shiny::div(shiny::br())
          
        ),
        
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
                                  selected = NULL, # character(0)
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
                                  choices = RMAVIS:::matchSpecies_options, 
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
                                  choices = RMAVIS:::resultsViewEIVs_options,
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
                                  choices = RMAVIS:::resultsViewDiversity_options,
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
        
        icon = bsicons::bs_icon("transparency"),
        
        shiny::div(
          
          id = ns("dcaAxisSelection_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::selectizeInput(inputId = ns("dcaAxisSelection"),
                                  label = "Axis Selection",
                                  choices = RMAVIS:::dcaAxisSelection_options,
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
          
          id = ns("selectedReferenceSpaces_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::selectizeInput(inputId = ns("selectedReferenceSpaces"),
                                  label = "Selected Reference Spaces",
                                  choices = character(0),
                                  selected = character(0),
                                  multiple = TRUE),
            
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Selected Reference Spaces",
              shiny::markdown(
                "
                Select the NVC communities to display the reference spaces
                for. By default the top fitting NVC communities are displayed.
                Please see the documentation for a definition of the
                'Reference Spaces' along with interpretation guidance.
                "
              ),
              placement = "bottom"
            )
            
          ),
          
          shiny::div(shiny::br())
          
        ),
        
        shiny::div(
          
          id = ns("groupSurveyPlots_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            shiny::selectizeInput(inputId = ns("groupSurveyPlots"),
                                  label = "Group Survey Plots",
                                  choices = RMAVIS:::groupSurveyPlots_options,
                                  selected = NULL,
                                  multiple = FALSE),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Group Survey Plots",
              shiny::markdown(
                "
                Choose whether to group the survey plots by selecting one of the
                following options:
                - 'No': The individual plots are left ungrouped
                - 'Group': Mean DCA axis scores are calculated from the individual plots
                by group.
                - 'Year': Mean DCA axis scores are calculated from the individual plots
                by year.
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
                                  choices = RMAVIS:::surveyQuadratSelection_options,
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
                                  choices = RMAVIS:::selectSurveyYears_options,
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
                                  choices = RMAVIS:::selectSurveyGroups_options,
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
                                  choices = RMAVIS:::selectSurveyQuadrats_options,
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
                                  choices = RMAVIS:::ccaVars_options,
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
                                      choices = RMAVIS:::dcaVars_options,
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
                - 'Reference Centroids': the centroids formed from the pseudo-quadrat DCA scores.
                - 'Species': the DCA scores of the species.
                - 'Unique Survey Species': the DCA scores of the species unique to the survey data, but absent from the best fitting NVC communities pseudo-quadrats (Local Reference (unrestricted) only).
                - 'Hill-Ellenberg': the CCA result axis scores for the Hill-Ellenberg selected in the 'CCA Variables' option.
                - 'Trajectory': arrows drawn between each set of selected sample plots by year, showing thetrajectory of the sample plots in the ordination space.
                
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
                Please enter the name of the person/s using RMAVIS, for attribution in the report.
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
                Please enter the name of the project/site being analysed in RMAVIS.
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
                                      choices = RMAVIS:::reportOptions_options,
                                      selected = c("nvcAssignmentResultsSite_Czekanowski", 
                                                   "composedFloristicTablesSite", 
                                                   "speciesFrequencyTable"),
                                      options = shinyWidgets::pickerOptions(
                                        dropdownAlignRight = TRUE
                                      ),
                                      choicesOpt = list(
                                        style = rep_len("font-size: 75%; line-height: 1.6;", length(unlist(RMAVIS:::reportOptions_options)))
                                      ),
                                      multiple = TRUE
            ),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Report Options",
              shiny::markdown(
                "
                Select the RMAVIS outputs to include in the report.
                
                Please note that selecting the 'Survey Table' option may produce
                a *very* long report if a large quantity of data is submitted to
                RMAVIS.
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
              icon = NULL,
              disabled = ''
            ),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Download Report",
              shiny::markdown(
                "
                Download a pdf report of the current RMAVIS analyses results.
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
        
        ## Download NVC Assignment Results -----------------------------------------
        shiny::div(
          
          id = ns("downloadRMAVISResults_div"),
          
          bslib::layout_columns(
            
            col_widths = c(11, 1),
            
            downloadButton(
              outputId = ns("downloadRMAVISResults"),
              label = "RMAVIS Results",
              class = NULL,
              icon = NULL
            ),
            
            bslib::popover(
              bsicons::bs_icon("info-circle"),
              title = "Download RMAVIS Results",
              shiny::markdown(
                "
                
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
