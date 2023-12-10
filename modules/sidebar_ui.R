sidebarUI <- function(id){
  
  ns <- NS(id)
  
  bslib::sidebar(
    
    width = 320,
    
    shiny::h5("Options"),
    # shiny::hr(),
    shiny::actionButton(inputId = ns("runAnalysis"),
                        label = "Run Analysis"),
    # shiny::hr(),
    
    # shiny::div(shiny::br()),
    
    bslib::accordion(
      
      open = FALSE,
      
      bslib::accordion_panel(
        
        "Survey Data", 
        
        icon = bsicons::bs_icon("clipboard-data"),
        
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
        
        bslib::layout_columns(
          
          col_widths = c(11, 1),
          
          shiny::actionButton(inputId = ns("uploadData"),
                              label = "Upload Data"),
          
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
          
        )
      ),
      
      bslib::accordion_panel(
        
        "NVC Assignment", 
        
        icon = bsicons::bs_icon("ui-checks-grid"),
        
        # shiny::selectizeInput(inputId = ns("nvcAssignMethods"),
        #                       label = "Methods",
        #                       choices = nvcAssignMethods_options,
        #                       selected = "pseudoQuadratSite",
        #                       multiple = TRUE),
        
        bslib::layout_columns(
          
          col_widths = c(11, 1),

          shiny::selectizeInput(inputId = ns("habitatRestriction"),
                                label = "Restrict Habitat",
                                choices = habitatRestriction_options,
                                selected = NULL,
                                multiple = TRUE
                                ),
                    
          # shinyWidgets::pickerInput(inputId = ns("habitatRestriction"),
          #                           label = "Restrict Habitat",
          #                           choices = habitatRestriction_options,
          #                           selected = NULL,
          #                           multiple = TRUE,
          #                           options = shinyWidgets::pickerOptions(dropupAuto = FALSE)),
          
          bslib::popover(
            bsicons::bs_icon("info-circle"),
            title = "Restrict Habitat",
            id = ns("restrictHabitatInfo"),
            paste0(""),
            placement = "bottom"
          )
          
        ),
        
        shiny::div(shiny::br()),
        
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
      ),
      
      bslib::accordion_panel(
        
        "Habitat Correspondence", 
        
        icon = bsicons::bs_icon("sliders"),
        
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
        
      ),
      
      bslib::accordion_panel(
        
        "Floristic Tables", 
        
        icon = bsicons::bs_icon("table"),
        
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
        
        bslib::layout_columns(
          
          col_widths = c(11, 1),
          
          shiny::selectizeInput(inputId = ns("crossTabulate"), 
                                label = "Cross Tabulate",
                                choices = crossTabulate_options, 
                                selected = "", 
                                multiple = FALSE),
          
          bslib::popover(
            bsicons::bs_icon("info-circle"),
            title = "Cross Tabulate",
            paste0("Select one of three options:"),
            placement = "bottom"
          )
          
        )
        
      ),
      
      bslib::accordion_panel(
        
        "EIVs", 
        
        icon = bsicons::bs_icon("water"),
        
        shiny::selectizeInput(inputId = ns("resultsViewEIVs"),
                                           label = "Results to View",
                                           choices = resultsViewEIVs_options,
                                           selected = c("unweightedMeanHillEllenbergValuesSite"),
                                           multiple = TRUE),
        
        
      ),
      
      bslib::accordion_panel(
        
        "Diversity", 
        
        icon = bsicons::bs_icon("tree"),
        
        shiny::selectizeInput(inputId = ns("resultsViewDiversity"),
                                           label = "Results to View",
                                           choices = resultsViewDiversity_options,
                                           selected = c("speciesRichnessSite", "speciesFrequency"),
                                           multiple = TRUE),
        
        
      ),
      
      bslib::accordion_panel(
        
        "Multivariate Analysis", 
        
        icon = bsicons::bs_icon("arrows-angle-expand"),
        
        # shinyWidgets::pickerInput(inputId = ns("globalReferenceSpaces"),
        #                           label = "Global Reference Spaces",
        #                           choices = globalReferenceSpaces_options,
        #                           selected = NULL,
        #                           multiple = TRUE,
        #                           options = shinyWidgets::pickerOptions(dropupAuto = FALSE)),
        
        shiny::selectizeInput(inputId = ns("globalReferenceSpaces"),
                              label = "Global Reference Spaces",
                              choices = globalReferenceSpaces_options,
                              selected = NULL,
                              multiple = TRUE),
        
        shiny::selectizeInput(inputId = ns("selectSurveyMethod"),
                              label = "Survey Quadrat Selection",
                              choices = surveyQuadratSelection_options,
                              selected = NULL,
                              multiple = FALSE),
        
        shiny::selectizeInput(inputId = ns("selectSurveyYears"),
                              label = "Select Survey Years",
                              choices = selectSurveyYears_options,
                              selected = NULL,
                              multiple = TRUE,
                              options = list(minItems = 1)),
        
        shiny::selectizeInput(inputId = ns("selectSurveyQuadrats"),
                              label = "Select Survey Quadrats",
                              choices = selectSurveyQuadrats_options,
                              selected = NULL,
                              multiple = TRUE,
                              options = list(minItems = 1)),
        
        shiny::selectizeInput(inputId = ns("selectSurveyGroups"),
                              label = "Select Survey Groups",
                              choices = selectSurveyGroups_options,
                              selected = c("F", "N"),
                              multiple = TRUE,
                              options = list(minItems = 1)),
        
        bslib::layout_columns(
          
          col_widths = c(11, 1),
          
          shiny::selectizeInput(inputId = ns("ccaVars"),
                                label = "CCA Variables",
                                choices = ccaVars_options,
                                selected = c("F", "N"),
                                multiple = TRUE,
                                options = list(minItems = 2,
                                               maxItems = 3)),
          
          bslib::popover(
            bsicons::bs_icon("info-circle"),
            title = "CCA",
            paste0(""),
            placement = "bottom"
          )
          
        ),
        
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
        
      ),
      
      bslib::accordion_panel(
        
        "Download Options", 
        
        icon = bsicons::bs_icon("download"),
        
        shiny::downloadButton(
          outputId = ns("generateReport"),
          label = "Download Report",
          class = NULL,
          icon = NULL
        ),
        
        shiny::div(shiny::br()),
        
        shiny::downloadButton(
          outputId = ns("downloadSpeciesData"),
          label = "Download Accepted Species",
          class = NULL,
          icon = NULL
        )
        
        # reportUI(id = ns("report_id_1"))
        
      )
    )
  )
  
}
