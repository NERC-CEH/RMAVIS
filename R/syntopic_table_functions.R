#' Compose a set of syntopic tables
#'
#' @param surveyData A n-column dataframe containing atleast one ID columns, for example year and group.
#' @param group_cols A vector of columns to group the surveyData by and from which to compose syntopic tables
#' @param species_col_name The name of the species column
#' @param plot_col_name The name of the plot ID column
#'
#' @return A  
#' @export
#'
#' @example 
composeSyntopicTables <- function(surveyData, group_cols, species_col_name = "Species", plot_col_name = "Quadrat"){
  
  syntopicTables <- surveyData |>
    tidyr::unite(col = "ID", group_cols, sep = " - ", remove = TRUE) |>
    dplyr::select(ID, plot_col_name, species_col_name) |>
    dplyr::mutate("Present" = 1) |>
    tidyr::pivot_wider(id_cols = c(ID, species_col_name),
                       values_from = Present,
                       names_from = plot_col_name) |>
    dplyr::rowwise() |>
    dplyr::mutate("Sum" = sum(dplyr::c_across(dplyr::where(is.numeric)), na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate("Frequency" = Sum / (ncol(dplyr::pick(dplyr::everything())) - 3)) |> # -2
    dplyr::select(ID, species_col_name, Sum, Frequency) |>
    dplyr::mutate(
      "Constancy" =
        dplyr::case_when(
          Frequency <= 0.2 ~ "I",
          Frequency <= 0.4 ~ "II",
          Frequency <= 0.6 ~ "III",
          Frequency <= 0.8 ~ "IV",
          Frequency <= 1.0 ~ "V",
          TRUE ~ as.character(Frequency)
        )
    ) |>
    dplyr::select(ID, species_col_name, Constancy) |>
    dplyr::mutate("Constancy" = factor(Constancy, levels = c("V", "IV", "III", "II", "I"))) |>
    dplyr::arrange(ID, Constancy, species_col_name)
  
  return(syntopicTables)
  
}