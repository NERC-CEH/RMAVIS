nvcInfoSidebar <- function(input, output, session, nvcCommNamesLookup, nvcFlorTabs, nvcCommAttr) {
  
  ns <- session$ns

  # Download Taxon Lookup ---------------------------------------------------
  output$downloadNVCInformation <- downloadHandler(
    
    filename = function() {
      
      paste0("RMAVIS.NVC.Information.",
             "v1-1-3",
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



