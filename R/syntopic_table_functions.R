#' Compose a set of syntopic tables
#' 
#' Compose a set of syntopic/floristic/constancy tables for a 
#' data frame containing plot ids, species names, and any number
#' of group columns which are concatenated into a ID column
#'
#' @param surveyData A n-column dataframe containing atleast one ID columns, for example year and group.
#' @param group_cols A vector of columns to group the surveyData by and from which to compose syntopic tables
#' @param species_col_name The name of the species column
#' @param plot_col_name The name of the plot ID column
#' @param numeric_constancy If TRUE the ordinal constancy classes (I, II, III, IV, V) are transformed into numeric values (1, 2, 3, 4, 5).
#'
#' @return A three column data frame containing the groups ID, species, and constancy.
#' @export
#'
#' @examples
#' RMAVIS::composeSyntopicTables(surveyData = RMAVIS::example_data[["Parsonage Down"]], 
#'                               group_cols = c("Year", "Group"), 
#'                               species_col_name = "Species", 
#'                               plot_col_name = "Quadrat",
#'                               numeric_constancy = FALSE)
composeSyntopicTables <- function(surveyData, group_cols, species_col_name = "Species", plot_col_name = "Quadrat", numeric_constancy = FALSE){
  
  # Determine the total number of quadrats per group
  plot_n <- surveyData |>
    tidyr::unite(col = "ID", group_cols, sep = " - ", remove = TRUE) |>
    dplyr::select(ID, plot_col_name) |>
    dplyr::distinct() |>
    dplyr::group_by(ID) |>
    dplyr::tally() |>
    dplyr::ungroup()
  
  # Calculate the absolute frequency and relative frequency of species 
  # occurrence across all plots by group
  syntopicTables <- surveyData |>
    tidyr::unite(col = "ID", group_cols, sep = " - ", remove = TRUE) |>
    dplyr::select(ID, plot_col_name, species_col_name) |>
    dplyr::mutate("Present" = 1) |>
    dplyr::group_by(ID, .data[[species_col_name]]) |>
    dplyr::summarise("Frequency" = sum(Present)) |>
    dplyr::ungroup() |>
    dplyr::left_join(plot_n, by = c("ID")) |>
    dplyr::mutate("Relative Frequency" = Frequency / n) |>
    dplyr::mutate(
      "Constancy" =
        dplyr::case_when(
          `Relative Frequency` <= 0.2 ~ "I",
          `Relative Frequency` <= 0.4 ~ "II",
          `Relative Frequency` <= 0.6 ~ "III",
          `Relative Frequency` <= 0.8 ~ "IV",
          `Relative Frequency` <= 1.0 ~ "V",
          TRUE ~ as.character(`Relative Frequency`)
        )
    ) |>
    dplyr::select(ID, species_col_name, Constancy) |>
    dplyr::mutate("Constancy" = factor(Constancy, levels = c("V", "IV", "III", "II", "I"))) |>
    dplyr::arrange(ID, Constancy, species_col_name)
  
  if(numeric_constancy == TRUE){
    
    syntopicTables <- syntopicTables |>
      dplyr::mutate(
        "Constancy" = 
          dplyr::case_when(
            Constancy == "I" ~ 1,
            Constancy == "II" ~ 2,
            Constancy == "III" ~ 3,
            Constancy == "IV" ~ 4,
            Constancy == "V" ~ 5,
            TRUE ~ as.numeric(0)
          )
      )
    
  }
  
  return(syntopicTables)
  
}