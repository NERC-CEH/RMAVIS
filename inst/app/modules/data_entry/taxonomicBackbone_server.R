taxonomicBackbone <- function(input, output, session, region) {
    
  ns <- session$ns
  
  taxonomic_backbone <- reactiveVal()
  
  observe({
    
    if(region() == "gbnvc"){
      
      tb <- UKVegTB::taxonomic_backbone |>
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
      
    } else if(region() == "mnnpc"){
      
      tb <- MNNPC::mnnpc_taxonomic_backbone |>
        dplyr::select(
          "Informal.Group" = "informal_group",
          "ID" = "id",
          "Taxon.Name" = "taxon_name",
          "Rank" = "rank"
        ) |>
        dplyr::distinct(Informal.Group, Taxon.Name, Rank, .keep_all = TRUE) |>
        dplyr::arrange(Taxon.Name)
    }
    
    taxonomic_backbone(tb)
    
  }) |>
    bindEvent(region(),
              ignoreInit = FALSE,
              ignoreNULL = TRUE)
  

  # Names lookup table ------------------------------------------------------
  output$taxonomicBackboneTable <- reactable::renderReactable({
    
    taxonomicBackboneTable <- reactable::reactable(data = taxonomic_backbone(),
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
    
    return(taxonomicBackboneTable)
    
  })
  
  
  outputOptions(output, "taxonomicBackboneTable", suspendWhenHidden = FALSE)
  
  return(taxonomic_backbone)
  
}
