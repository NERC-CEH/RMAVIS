matchBroadHabitats <- function(df, spp_col = "TVK"){
  
  # Ensure the list of species is unique and calculate the number of species
  input_spp_uniq <- unique(df[[spp_col]])
  input_spp_uniq_len <- length(input_spp_uniq)
  
  # Retrieve the habitat association data for selected species
  hab_association_data <- RMAVIS::bha_data |>
    dplyr::filter(TVK %in% input_spp_uniq)
  
  # Calculate habitat association metrics
  hab_association_counts <- hab_association_data |>
    dplyr::summarise(dplyr::across(RMAVIS:::bh_lookup$Habitat.Code, \(x) sum(x, na.rm = TRUE))) |>
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Habitat.Code", values_to = "Indicator.Count") |>
    dplyr::filter(Indicator.Count != 0) |>
    dplyr::left_join(RMAVIS:::bh_lookup, by = "Habitat.Code") |>
    dplyr::arrange(dplyr::desc(Indicator.Count)) |>
    tibble::rowid_to_column("Rank") |>
    dplyr::mutate("Matching.Coefficient" = (Indicator.Count / input_spp_uniq_len) * 100, .after = "Indicator.Count") |>
    dplyr::slice(1:10)
  
  return(hab_association_counts)
  
}