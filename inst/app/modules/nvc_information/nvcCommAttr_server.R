nvcCommAttr <- function(input, output, session) {
  
  ns <- session$ns
  
  # Retrieve sidebar options ------------------------------------------------
  
  # observe({
  #   
  # }) |>
  #   bindEvent(sidebar_options(), ignoreInit = TRUE)
  

  # Reactively update data --------------------------------------------------
  # communityAttributes_rval <- reactiveVal()
  # 
  # observe({
  #   
  #   setupData <- setupData()
    
    community_attributes <- RMAVIS::nvc_community_attributes |>
      dplyr::bind_rows(RMAVIS::sowg_community_attributes) |>
      dplyr::select("NVC.Code" = "nvc_code", 
                    "Number.Samples" = "num_samples",
                    "Min.Species" = "min_species",
                    "Max.Species" = "max_species",
                    "Mean.Species" = "mean_species",
                    "Total.Species" = "species_count") |>
      dplyr::mutate("Mean.Species" = round(Mean.Species, digits = 0))
      
  #   communityAttributes_rval(community_attributes)
  #   
  # }) |>
  #   bindEvent(setupData(),
  #             ignoreNULL = TRUE,
  #             ignoreInit = FALSE)
  

  # Community attributes data -----------------------------------------------
  output$communityAttributesTable <- reactable::renderReactable({
    
    communityAttributesTable <- reactable::reactable(data = community_attributes,
                                                     filterable = FALSE,
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
                                                       NVC.Code = reactable::colDef(
                                                         filterable = TRUE,
                                                         filterMethod = reactable::JS(
                                                         "function filterRows(rows, columnId, filterValue) {
                                                            return rows.filter(function(row) {
                                                              return row.values[columnId] === filterValue;
                                                            });
                                                          }"),
                                                         maxWidth = 150
                                                         )
                                                       )
                                                     )
    
    return(communityAttributesTable)
    
  })
  
  
  outputOptions(output, "communityAttributesTable", suspendWhenHidden = FALSE)
  
}
