vcInfoSidebar <- function(input, output, session, region, vcCommNamesLookup, vcFlorTabs, vcCommAttr) {
  
  ns <- session$ns

  # Download Taxon Lookup ---------------------------------------------------
  output$downloadVCInformation <- downloadHandler(
    
    filename = function() {
      
      paste0("RMAVIS.VC.Information.",
             "v1-2-0",
             ".xlsx",
             sep="")
      
    },
    
    content = function(file) {
      
      sheets <- list(
        "names_lookup" = nvcCommNamesLookup(),
        "floristic_tables" = nvcFlorTabs(),
        "community_attributes" = nvcCommAttr() 
      )
      
      writexl::write_xlsx(x = sheets, path = file)
      
    }
  )
  
}



