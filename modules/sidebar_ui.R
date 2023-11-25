sidebarUI <- function(id){
  
  ns <- NS(id)
  
  bslib::sidebar(
    
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
        shiny::selectizeInput(inputId = ns("coverMethod"), 
                              label = "Cover Method", 
                              choices = coverMethod_options, 
                              selected = "directPercentage", 
                              multiple = FALSE),
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
                              choices = c(5:10), 
                              selected = 5, 
                              multiple = FALSE),
        shiny::checkboxInput(inputId = ns("groupSample"), 
                             label = "Group by Sample",
                             value = TRUE)
        
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
            paste0("Restrict the NVC communities options below",
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
          
          shiny::selectizeInput(inputId = ns("crossTabulate"), 
                                label = "Cross Tabulate",
                                choices = c("No" = "No",
                                            "Composed to NVC" = "compToNVC",
                                            "NVC to Composed" = "NVCToComp"), 
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
      )
      
    )
  )
  
}