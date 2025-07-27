nvcCommAttr <- function(input, output, session) {
  
  ns <- session$ns
  
  # Create community attributes object --------------------------------------
  nvc_cm_he_wide <- RMAVIS::nvc_cm_he |>
    dplyr::select(-sd) |>
    tidyr::pivot_wider(id_cols = nvc_code,
                       names_from = indicator, 
                       values_from = mean) |> 
    dplyr::mutate("Type" = "Original", .before = "nvc_code")
  
  calthion_cm_he_wide <- RMAVIS::calthion_cm_he |>
    dplyr::select(-sd) |>
    tidyr::pivot_wider(id_cols = nvc_code,
                       names_from = indicator, 
                       values_from = mean) |> 
    dplyr::mutate("Type" = "Calthion", .before = "nvc_code")
  
  sowg_cm_he_wide <- RMAVIS::sowg_cm_he |>
    dplyr::select(-sd) |>
    tidyr::pivot_wider(id_cols = nvc_code,
                       names_from = indicator, 
                       values_from = mean) |> 
    dplyr::mutate("Type" = "SOWG", .before = "nvc_code")
  
  all_cm_he_wide <- dplyr::bind_rows(nvc_cm_he_wide, calthion_cm_he_wide, sowg_cm_he_wide) |>
    dplyr::mutate_if(is.numeric, round, 2)
  
  community_attributes <- shiny::reactiveVal(
      dplyr::bind_rows(RMAVIS::nvc_community_attributes,
                       RMAVIS::calthion_community_attributes,
                       RMAVIS::sowg_community_attributes) |>
      dplyr::mutate("mean_species" = round(mean_species, digits = 0)) |>
      dplyr::inner_join(all_cm_he_wide, by = "nvc_code") |>
      dplyr::select("NVC.Code" = "nvc_code", 
                    "Type",
                    "Rank" = "rank",
                    "Number.Samples" = "num_samples",
                    "Min.Species" = "min_species",
                    "Max.Species" = "max_species",
                    "Mean.Species" = "mean_species",
                    "Total.Species" = "species_count",
                    "F", "L", "N", "R", "S")
  )
  

  # Community attributes data -----------------------------------------------
  output$communityAttributesTable <- reactable::renderReactable({
    
    communityAttributesTable <- reactable::reactable(data = community_attributes(),
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
                                                         ),
                                                       Type = reactable::colDef(
                                                         filterable = TRUE,
                                                         maxWidth = 150
                                                       )
                                                       )
                                                     )
    
    return(communityAttributesTable)
    
  })
  
  
  outputOptions(output, "communityAttributesTable", suspendWhenHidden = FALSE)
  
  return(community_attributes)
  
}
