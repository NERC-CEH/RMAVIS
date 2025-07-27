rmavisTaxonNamesLookup <- function(input, output, session) {
    
  ns <- session$ns
    
  rmavis_taxon_names_lookup <- UKVegTB::taxonomic_backbone |>
    dplyr::select(
      "Informal.Group" = "informal_group",
      "Taxon.Name" = "taxon_name",
      "TVK" = "TVK",
      "Rank" = "rank",
      "Qualifier" = "qualifier",
      "Authority" = "authority",
      "Full.Name" = "full_name"
    ) |>
    dplyr::arrange(Taxon.Name)
  

  # Names lookup table ------------------------------------------------------
  output$rmavisTaxonNamesLookupTable <- reactable::renderReactable({
    
    rmavisTaxonNamesLookupTable <- reactable::reactable(data = rmavis_taxon_names_lookup,
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
                                                        ),
                                                        columns = list(
                                                          Informal.Group = reactable::colDef(maxWidth = 125),
                                                          Taxon.Name = reactable::colDef(minWidth = 200),
                                                          Full.Name = reactable::colDef(minWidth = 200)
                                                          )
                                                        )
    
    return(rmavisTaxonNamesLookupTable)
    
  })
  
  
  outputOptions(output, "rmavisTaxonNamesLookupTable", suspendWhenHidden = FALSE)
  
}
