sidebarUI <- function(id){
  
  ns <- NS(id)
  
  bslib::sidebar(
    
    width = 320,
    
    shiny::h5("Model Options"),
    # shiny::hr(),
    shiny::actionButton(inputId = ns("runAnalysis"),
                        label = "Run Analysis"),
    # shiny::hr(),
    
    # shiny::div(shiny::br()),
    
    bslib::accordion(
      
      open = FALSE,
      
      bslib::accordion_panel(
        
        "Input Data", 
        
        icon = bsicons::bs_icon("wrench-adjustable"),
        
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
                                choices = exampleDataOptions, 
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
          
        ),
        
        shiny::div(shiny::br()),
        
        bslib::layout_columns(
          
          col_widths = c(11, 1),
          
          shiny::selectizeInput(inputId = ns("habitatRestriction"), 
                                label = "Restrict Habitat", 
                                choices = habitatRestriction_options, 
                                selected = NA, 
                                multiple = TRUE),
          
          bslib::popover(
            bsicons::bs_icon("info-circle"),
            title = "Restrict Habitat",
            id = ns("restrictHabitatInfo"),
            paste0(""),
            placement = "bottom"
          )
          
        ),
        
        bslib::layout_columns(
          
          col_widths = c(11, 1),
          
          shiny::selectizeInput(inputId = ns("nTopResults"), 
                                label = "Number of Top Results", 
                                choices = c(1:10), 
                                selected = 3, 
                                multiple = FALSE),
          
          bslib::popover(
            bsicons::bs_icon("info-circle"),
            title = "Number of Top Results",
            id = ns("nTopResultsInfo"),
            paste0(""),
            placement = "bottom"
          )
          
        ),
        
        bslib::layout_columns(
          
          col_widths = c(11, 1),
          
          shiny::selectizeInput(inputId = ns("groupMethod"), 
                                label = "Grouping Method", 
                                choices = groupMethod_options,
                                selected = c("year", "site", "quadrat.group"), 
                                multiple = TRUE),
          
          bslib::popover(
            bsicons::bs_icon("info-circle"),
            title = "Grouping Method",
            id = ns("groupMethodInfo"),
            paste0(""),
            placement = "bottom"
          )
          
        )
        
        # shiny::actionButton(inputId = ns("resetTable"),
        #                     label = "Reset Table"),
        # shiny::selectizeInput(inputId = ns("assignmentMethods"),
        #                       label = "NVC Assignment Methods",
        #                       choices = assignmentMethod_options,
        #                       selected = c("pseudoQuadratGroup", "pseudoQuadratQuadrat"),
        #                       multiple = TRUE),
        # shiny::selectizeInput(inputId = ns("coverMethod"), 
        #                       label = "Cover Method", 
        #                       choices = coverMethod_options, 
        #                       selected = "directPercentage", 
        #                       multiple = FALSE),
        # shiny::selectizeInput(inputId = ns("nvcFittingMethod"), 
        #                       label = "Method", 
        #                       choices = c("Pseudo-Quadrat" = "pseudoQuadrat",
        #                                   "Floristic Table" = "floristicTable"), 
        #                       selected = "pseudoQuadrat", 
        #                       multiple = FALSE),
        
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
                                label = "NVC Community", 
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
            paste0("Select a year/site/quadrat ID to compose a floristic table."),
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
        
        "DCA", 
        
        icon = bsicons::bs_icon("sliders"),
        
        bslib::layout_columns(
          
          col_widths = c(11, 1),
          
          shiny::checkboxGroupInput(inputId = ns("dcaVars"),
                                    label = "Axis Scores",
                                    choices = dcaVars_options,
                                    selected = c("referenceSpace", "surveyQuadrats")),
          
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
          icon = shiny::icon("book")
        ),
        
        shiny::div(shiny::br()),
        
        shiny::downloadButton(
          outputId = ns("downloadSpeciesData"),
          label = "Download Accepted Species",
          class = NULL,
          icon = shiny::icon("book")
        )
        
        # reportUI(id = ns("report_id_1"))
        
        )
    )
  )
  
}