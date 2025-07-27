nvcCommNamesLookup <- function(input, output, session) {
    
  ns <- session$ns
    
  nvc_comm_names_lookup <- shiny::reactiveVal(
      dplyr::bind_rows(RMAVIS::nvc_community_attributes |> dplyr::mutate("Type" = "Original", .before = "nvc_code"),
                       RMAVIS::calthion_community_attributes |> dplyr::mutate("Type" = "Calthion", .before = "nvc_code"),
                       RMAVIS::sowg_community_attributes |> dplyr::mutate("Type" = "SOWG", .before = "nvc_code")) |>
      dplyr::select("NVC.Code" = "nvc_code", "Type", "NVC.Name" = "fullname")
    )
  

  # Names lookup table ------------------------------------------------------
  output$nvcCommNamesLookupTable <- reactable::renderReactable({
    
    nvcCommNamesLookupTable <- reactable::reactable(data = nvc_comm_names_lookup(),
                                                    filterable = FALSE,
                                                    pagination = FALSE, 
                                                    highlight = TRUE,
                                                    bordered = TRUE,
                                                    sortable = FALSE, 
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
                                                      NVC.Code = reactable::colDef(
                                                        filterable = TRUE,
                                                        filterMethod = reactable::JS(
                                                          "function filterRows(rows, columnId, filterValue) {
                                                                 return rows.filter(function(row) {
                                                                   return row.values[columnId] === filterValue;
                                                                 });
                                                               }"),
                                                        maxWidth = 150
                                                        ),
                                                      Type = reactable::colDef(
                                                        filterable = TRUE,
                                                        maxWidth = 150
                                                      )
                                                      )
                                                    )
    
    return(nvcCommNamesLookupTable)
    
  })
  
  
  outputOptions(output, "nvcCommNamesLookupTable", suspendWhenHidden = FALSE)
  
  return(nvc_comm_names_lookup)
  
}
