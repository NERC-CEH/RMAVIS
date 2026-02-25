vcTaxonNamesLookup <- function(input, output, region, session) {
    
  ns <- session$ns
    
  vc_taxon_names_lookup <- reactiveVal(data.frame("Original.Name" = character(0),
                                                  "Original.TVK" = character(0),
                                                  "Change" = character(0),
                                                  "Recommended.Name" = character(0),
                                                  "Recommended.TVK" = character(0),
                                                  "Strata" = character(0),
                                                  "NVC.Taxon.Name" = character(0)))
  
  observe({
    
    region <- region()
    
    if(region == "gbnvc"){
      
      nl_prepped <-  RMAVIS::nvc_taxa_lookup |>
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
      
    } else if (region == "mnnpc"){
      
      nl_prepped <- data.frame("Original.Name" = character(0),
                               "Original.TVK" = character(0),
                               "Change" = character(0),
                               "Recommended.Name" = character(0),
                               "Recommended.TVK" = character(0),
                               "Strata" = character(0),
                               "NVC.Taxon.Name" = character(0))
      
    }
    
    vc_taxon_names_lookup(nl_prepped)
    
  }) |>
    shiny::bindEvent(region(),
                     ignoreInit = FALSE,
                     ignoreNULL = TRUE)
  

  # Names lookup table ------------------------------------------------------
  output$vcTaxonNamesLookupTable <- reactable::renderReactable({
    
    vcTaxonNamesLookupTable <- reactable::reactable(data = vc_taxon_names_lookup(),
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
    
    return(vcTaxonNamesLookupTable)
    
  })
  
  
  outputOptions(output, "vcTaxonNamesLookupTable", suspendWhenHidden = FALSE)
  
}
