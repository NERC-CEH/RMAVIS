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
        
        # shiny::actionButton(inputId = ns("resetTable"),
        #                     label = "Reset Table"),
        shiny::selectizeInput(inputId = ns("inputMethod"), 
                              label = "Input Method", 
                              choices = inputMethod_options, 
                              selected = "none", 
                              multiple = FALSE),
        shiny::selectizeInput(inputId = ns("exampleData"), 
                              label = "Example Data", 
                              choices = exampleDataOptions, 
                              selected = "none", 
                              multiple = FALSE),
        shiny::selectizeInput(inputId = ns("dataEntryFormat"), 
                              label = "Data Entry Format", 
                              choices = dataEntryFormat_options, 
                              selected = "table", 
                              multiple = FALSE),
        shiny::fileInput(inputId = ns("uploadData"),
                         label = "Upload .csv",
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv"
                                    )
                         ),
        # shiny::selectizeInput(inputId = ns("assignmentMethods"),
        #                       label = "NVC Assignment Methods",
        #                       choices = assignmentMethod_options,
        #                       selected = c("pseudoQuadratGroup", "pseudoQuadratSample"),
        #                       multiple = TRUE),
        # shiny::selectizeInput(inputId = ns("coverMethod"), 
        #                       label = "Cover Method", 
        #                       choices = coverMethod_options, 
        #                       selected = "directPercentage", 
        #                       multiple = FALSE),
        shiny::selectizeInput(inputId = ns("habitatRestriction"), 
                              label = "Restrict Habitat", 
                              choices = habitatRestriction_options, 
                              selected = NA, 
                              multiple = TRUE),
        # shiny::selectizeInput(inputId = ns("nvcFittingMethod"), 
        #                       label = "Method", 
        #                       choices = c("Pseudo-Quadrat" = "pseudoQuadrat",
        #                                   "Floristic Table" = "floristicTable"), 
        #                       selected = "pseudoQuadrat", 
        #                       multiple = FALSE),
        shiny::selectizeInput(inputId = ns("nTopResults"), 
                              label = "Number of Top Results", 
                              choices = c(1:10), 
                              selected = 3, 
                              multiple = FALSE),
        shiny::selectizeInput(inputId = ns("groupMethod"), 
                              label = "Grouping Method", 
                              choices = groupMethod_options,
                              selected = c("year", "site"), 
                              multiple = TRUE)
        
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
            paste0("Select a year/site/sample ID to compose a floristic table."),
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
        
        "Download Options", 
        
        icon = bsicons::bs_icon("download"),
        
        shiny::downloadButton(
          outputId = ns("generateReport"),
          label = "Download Report",
          class = NULL,
          icon = shiny::icon("book")
        )
        
        # reportUI(id = ns("report_id_1"))
        
        )
    )
  )
  
}