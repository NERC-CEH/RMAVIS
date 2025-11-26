#' Combine taxa into aggregated groups
#' 
#' Combine taxa into aggregated groups by summing the cover using a lookup.
#'
#' @param plot_data A data frame containing vegetation plot survey data with 5 columns: Year, Group, Quadrat, Species, and Cover. See `MNNPC::mnnpc_example_data`.
#' @param agg_lookup A data frame containing a lookup between accepted taxon names present in plot_data and taxa or groups of taxa combined for analysis. See `MNNPC::mnnpc_taxa_conv`.
#' @param plot_data_taxon_col The name of the column in plot_data containing the species/taxon names.
#' @param agg_lookup_taxon_col The name of the column in agg_lookup containing the species/taxon names.
#'
#' @returns A data frame containing the aggregated data
#' @export
#'
#' @examples
#' RMAVIS::aggregate_taxa(plot_data = MNNPC::mnnpc_example_data$`St. Croix State Forest`, 
#'                        agg_lookup = MNNPC::mnnpc_taxa_conv,
#'                        plot_data_taxon_col = "Species",
#'                        agg_lookup_taxon_col = "taxon")
aggregate_taxa <- function(plot_data, agg_lookup, plot_data_taxon_col, agg_lookup_taxon_col){
  
  plot_data_agg <- dplyr::left_join(x = plot_data |> dplyr::mutate("taxon" = .data[[plot_data_taxon_col]], .keep = "unused"),
                                    y = agg_lookup |> dplyr::mutate("taxon" = .data[[agg_lookup_taxon_col]], .keep = "unused"), 
                                    by = "taxon") |>
    dplyr::summarise("Cover" = sum(Cover, na.rm = TRUE),
                    .by = c("Year", "Group", "Quadrat", "analysis_group")) |>
    dplyr::mutate("Cover" = ifelse(Cover > 100, 100, Cover)) |>
    dplyr::select(Year, Group, Quadrat, "Species" = "analysis_group", Cover)
  
  return(plot_data_agg)
  
}