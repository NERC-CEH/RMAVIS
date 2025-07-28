nvcFlorTabs <- function(input, output, session) {
  
  ns <- session$ns

  # Establish floristic tables data -----------------------------------------
  floristic_tables <- shiny::reactiveVal(
    dplyr::bind_rows(RMAVIS::nvc_floristic_tables |> dplyr::mutate("Type" = "Original", .before = "nvc_code"),
                     RMAVIS::sowg_floristic_tables |> dplyr::mutate("Type" = "SOWG", .before = "nvc_code"),
                     RMAVIS::calthion_floristic_tables |> dplyr::mutate("Type" = "Calthion", .before = "nvc_code")) |>
    dplyr::select("NVC.Code" = "nvc_code", 
                  "Type",
                  "Taxon.Name" = "nvc_taxon_name",
                  "Constancy" = "constancy",
                  "Frequency" = "absolute_frequency",
                  "Minimum.Cover" = "minimum_cover",
                  "Mean.Cover" = "mean_cover",
                  "Maximum.Cover" = "maximum_cover")
  )
 
  

  # Floristic tables table --------------------------------------------------
  output$floristicTablesTable <- reactable::renderReactable({

    floristicTablesTable <- reactable::reactable(data = floristic_tables(),
                                                 filterable = TRUE,
                                                 pagination = TRUE,
                                                 defaultPageSize = 200,
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
                                                   # filterMethod = reactable::JS(
                                                   #   "function filterRows(rows, columnId, filterValue) {
                                                   #          return rows.filter(function(row) {
                                                   #            return row.values[columnId] === filterValue;
                                                   #          });
                                                   #        }"),
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
                                                   Taxon.Name = reactable::colDef(
                                                     filterable = TRUE,
                                                     minWidth = 225
                                                   ),
                                                   Type = reactable::colDef(
                                                     filterable = TRUE,
                                                     maxWidth = 150
                                                   )
                                                   )
                                                 )

    return(floristicTablesTable)

  })
  
  
  outputOptions(output, "floristicTablesTable", suspendWhenHidden = FALSE)
  
  return(floristic_tables)
  
}
