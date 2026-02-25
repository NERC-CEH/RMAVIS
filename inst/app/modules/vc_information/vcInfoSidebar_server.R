vcInfoSidebar <- function(input, output, session, setupData, vcCommNamesLookup, vcFlorTabs, vcCommAttr) {
  
  ns <- session$ns
  
# Retrieve setup data -----------------------------------------------------
  region <- reactiveVal()
  
  observe({
    
    region(setupData()$region)
    
  }) |>
    bindEvent(setupData(),
              ignoreInit = FALSE)


# Download VC Information -------------------------------------------------
  output$downloadVCInformation <- downloadHandler(
    
    filename = function() {
      
      paste0("RMAVIS.VC.Information.",
             stringr::str_to_upper(region()),
             ".v1-2-0",
             ".xlsx",
             sep="")
      
    },
    
    content = function(file) {
      
      sheets <- list(
        "names_lookup" = vcCommNamesLookup(),
        "floristic_tables" = vcFlorTabs(),
        "community_attributes" = vcCommAttr() 
      )
      
      writexl::write_xlsx(x = sheets, path = file)
      
    }
  )
  
}



