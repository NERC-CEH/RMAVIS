taxaLookup <- function(input, output, session, region) {
    
  ns <- session$ns
  
  taxa_lookup <- reactiveVal()
  
  observe({
    
    if(region() == "gbnvc"){
      
      tl <- UKVegTB::taxa_lookup |>
        dplyr::select(
          "Informal.Group" = "informal_group",
          "Taxon.Name" = "taxon_name",
          "TVK" = "TVK",
          "Recommended.Taxon.Name" = "recommended_taxon_name",
          "Recommended.TVK" = "recommended_TVK"
        ) |>
        dplyr::arrange(Taxon.Name)
      
    } else if(region() == "mnnpc"){
      
      tl <- MNNPC::mnnpc_taxa_lookup |>
        dplyr::select(
          "Informal.Group" = "informal_group",
          "Taxon.Name" = "taxon_name",
          "Recommended.Taxon.Name" = "recommended_taxon_name",
          "Analysis.Group" = "analysis_group",
          "Analysis.Group.Includes" = "analysis_group_includes"
        ) |>
        dplyr::arrange(Taxon.Name)
    }
    
    taxa_lookup(tl)
    
  }) |>
    bindEvent(region(),
              ignoreInit = FALSE,
              ignoreNULL = TRUE)
  

  # Names lookup table ------------------------------------------------------
  output$taxaLookupTable <- reactable::renderReactable({
    
    taxaLookupTable <- reactable::reactable(data = taxa_lookup(),
                                            filterable = TRUE,
                                            pagination = TRUE,
                                            defaultPageSize = 30,
                                            highlight = TRUE,
                                            bordered = TRUE,
                                            sortable = TRUE, 
                                            wrap = FALSE,
                                            resizable = TRUE,
                                            style = list(fontSize = "1rem"),
                                            class = "my-tbl",
                                            # style = list(fontSize = "1rem"),
                                            rowClass = "my-row",
                                            defaultColDef = reactable::colDef(
                                              headerClass = "my-header",
                                              class = "my-col",
                                              align = "center" # Needed as alignment is not passing through to header
                                            )
                                            )
    
    return(taxaLookupTable)
    
  })
  
  
  outputOptions(output, "taxaLookupTable", suspendWhenHidden = FALSE)
  
  return(taxa_lookup)
  
}
