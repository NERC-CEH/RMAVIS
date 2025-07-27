nvcInfoSidebarUI <- function(id){
  
  ns <- NS(id)
  
  bslib::sidebar(
    
    width = 320,
    
    shiny::h5("Download"),

# Survey Data -------------------------------------------------------------
    bslib::accordion(
      
      bslib::accordion_panel(
      
        "Download", 
        
        icon = bsicons::bs_icon("download"),
          
          ## Download Survey Data ----------------------------------------------------
          shiny::div(
            
            id = ns("downloadNVCInformation"),
            
            bslib::layout_columns(
              
              col_widths = c(11, 1),
              
              downloadButton(
                outputId = ns("downloadNVCInformation"),
                label = "NVC Information",
                class = NULL,
                icon = NULL
              ),
              
              bslib::popover(
                bsicons::bs_icon("info-circle"),
                title = "Download NVC Information",
                shiny::markdown(
                  "
                  Download a .xlsx file containing the NVC floristic tables, community names lookup, and community attributes.
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
