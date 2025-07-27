nvcTaxonNamesLookup <- function(input, output, session) {
    
  ns <- session$ns
    
  nvc_taxon_names_lookup <- RMAVIS::nvc_taxa_lookup |>
    dplyr::select(
      "Original.Name" = "original_taxon_name",
      "Original.TVK" = "original_TVK",
      "Change" = "change",
      "Recommended.Name" = "recommended_full_name",
      "Recommended.TVK" = "recommended_TVK",
      "Strata" = "strata",
      "NVC.Taxon.Name" = "nvc_taxon_name"
    ) |>
    dplyr::arrange(Original.Name)
  

  # Names lookup table ------------------------------------------------------
  output$nvcTaxonNamesLookupTable <- reactable::renderReactable({
    
    nvcTaxonNamesLookupTable <- reactable::reactable(data = nvc_taxon_names_lookup,
                                                     filterable = TRUE,
                                                     pagination = FALSE, 
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
                                                       Strata = reactable::colDef(maxWidth = 90),
                                                       Change = reactable::colDef(maxWidth = 90)
                                                       )
                                                     )
    
    return(nvcTaxonNamesLookupTable)
    
  })
  
  
  outputOptions(output, "nvcTaxonNamesLookupTable", suspendWhenHidden = FALSE)
  
}
