vcInfoSidebarUI <- function(id){
  
  ns <- NS(id)
  
  bslib::sidebar(
    
    width = 375,
    
    shiny::h5("Download"),

# Survey Data -------------------------------------------------------------
    bslib::accordion(
      
      bslib::accordion_panel(
      
        "Download", 
        
        icon = bsicons::bs_icon("download"),
          
          ## Download Survey Data ----------------------------------------------------
          shiny::div(
            
            id = ns("downloadVCInformation"),
            
            bslib::layout_columns(
              
              col_widths = c(11, 1),
              
              downloadButton(
                outputId = ns("downloadVCInformation"),
                label = "VC Information",
                class = NULL,
                icon = NULL
              ),
              
              bslib::popover(
                bsicons::bs_icon("info-circle"),
                title = "Download VC Information",
                shiny::markdown(
                  "
                  Download a .xlsx file containing the floristic tables, community names lookup, and community attributes for the selected VC system (GB-NVC or MNNPC).
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
