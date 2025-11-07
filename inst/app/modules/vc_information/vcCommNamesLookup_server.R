vcCommNamesLookup <- function(input, output, region, session) {
    
  ns <- session$ns
    
  vc_comm_names_lookup <- shiny::reactiveVal(data.frame("NVC.Code" = character(0),
                                                        "Type" = character(0),
                                                        "NVC.Name" = character(0)))
  
  observe({
    
    region <- region()
    
    if(region == "gbnvc"){
      
      names_prepped <- dplyr::bind_rows(RMAVIS::nvc_community_attributes |> dplyr::mutate("Type" = "Original", .before = "nvc_code"),
                                        RMAVIS::calthion_community_attributes |> dplyr::mutate("Type" = "Calthion", .before = "nvc_code"),
                                        RMAVIS::sowg_community_attributes |> dplyr::mutate("Type" = "SOWG", .before = "nvc_code")) |>
        dplyr::select("NVC.Code" = "nvc_code", "Type", "NVC.Name" = "fullname") |>
        dplyr::arrange(NVC.Code)
      
    } else if (region == "mnnpc"){
      
      names_prepped <- MNNPC::mnnpc_community_attributes |>
        dplyr::select("MNNPC.Code" = "npc_code", "MNNPC.Name" = "fullname") |>
        dplyr::arrange(MNNPC.Code)
      
    }
    
    vc_comm_names_lookup(names_prepped)
    
  }) |>
  shiny::bindEvent(region(),
                   ignoreInit = FALSE,
                   ignoreNULL = TRUE)

  # Names lookup table ------------------------------------------------------
  output$vcCommNamesLookupTable <- reactable::renderReactable({
    
    vcCommNamesLookupTable <- reactable::reactable(data = vc_comm_names_lookup(),
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
    
    return(vcCommNamesLookupTable)
    
  })
  
  
  outputOptions(output, "vcCommNamesLookupTable", suspendWhenHidden = FALSE)
  
  return(vc_comm_names_lookup)
  
}
