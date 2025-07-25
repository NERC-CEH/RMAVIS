#' Match species lists to Broad habitats
#'
#' @param df A data frame including species presences.
#' @param tvk_col The column in the dataframe containing the UKSI Taxon Version Keys (TVKs), see `UKVegTB::taxonomic_backbone`.
#'
#' @returns A data frame containing the top-fitting broad habitats.
#' 
#' @references Jackson, D.L., 2000. Guidance on the interpretation of the Biodiversity Broad Habitat Classification (terrestrial and freshwater types): Definitions and the relationship with other classifications (JNCC Report No. 307). JNCC, Peterborough.
#' 
#' @export
#'
#' @examples
#' data <- RMAVIS::example_data$`Parsonage Down` |> 
#'   dplyr::filter(Quadrat == "3N.1") |> 
#'   dplyr::left_join(UKVegTB::taxa_lookup[, c("taxon_name", "recommended_TVK")], by = c("Species" = "taxon_name"))
#' RMAVIS::matchBroadHabitats(df = data, tvk_col = "recommended_TVK")
matchBroadHabitats <- function(df, tvk_col = "recommended_TVK"){
  
  # Ensure the list of species is unique and calculate the number of species
  input_spp_uniq <- unique(df[[tvk_col]])
  input_spp_uniq_len <- length(input_spp_uniq)
  
  # Retrieve the habitat association data for selected species
  hab_association_data <- RMAVIS::broad_habitat_indicators |>
    dplyr::filter(recommended_TVK %in% input_spp_uniq)
  
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