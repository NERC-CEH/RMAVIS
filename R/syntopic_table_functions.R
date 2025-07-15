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
#' @param numeral_constancy If TRUE the numeric constancy classes (1, 2, 3, 4, 5) are transformed into roman numeral values (I, II, III, IV, V).
#' @param remove_low_freq_taxa If not NULL, a float greater than 0 and less than 1 indicating the minimum relative frequency of taxa to include in the floristic tables.
#'
#' @return A three column data frame containing the groups ID, species, and constancy.
#' @export
#'
#' @examples
#' RMAVIS::composeSyntopicTables(surveyData = RMAVIS::example_data[["Parsonage Down"]], 
#'                               group_cols = c("Year", "Group"), 
#'                               species_col_name = "Species", 
#'                               plot_col_name = "Quadrat",
#'                               numeral_constancy = FALSE,
#'                               remove_low_freq_taxa = 0.05)
composeSyntopicTables <- function(surveyData, group_cols, species_col_name = "Species", plot_col_name = "Quadrat", numeral_constancy = FALSE, remove_low_freq_taxa = NULL){
  
  # Determine the total number of quadrats per group
  plot_n <- surveyData |>
    tidyr::unite(col = "ID", group_cols, sep = " - ", remove = TRUE) |>
    dplyr::select(ID, plot_col_name) |>
    dplyr::distinct() |>
    dplyr::group_by(ID) |>
    dplyr::tally() |>
    dplyr::ungroup()
  
  if(is.null(remove_low_freq_taxa)){
    
    filter_threshold <- 0
    
  } else if(remove_low_freq_taxa >= 1 | remove_low_freq_taxa <= 0){
    
    filter_threshold <- 0
    
  } else {
    
    filter_threshold <- remove_low_freq_taxa
    
  }
  
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
    dplyr::filter(`Relative Frequency` > filter_threshold) |>
    dplyr::mutate(
      "Constancy" =
        dplyr::case_when(
          `Relative Frequency` <= 0.2 ~ 1,
          `Relative Frequency` <= 0.4 ~ 2,
          `Relative Frequency` <= 0.6 ~ 3,
          `Relative Frequency` <= 0.8 ~ 4,
          `Relative Frequency` <= 1.0 ~ 5,
          TRUE ~ as.numeric(`Relative Frequency`)
        )
    ) |>
    dplyr::select(ID, species_col_name, Constancy) |>
    dplyr::mutate("Constancy" = factor(Constancy, levels = c(5, 4, 3, 2, 1))) |>
    dplyr::arrange(ID, Constancy, species_col_name)
  
  if(numeral_constancy == TRUE){
    
    syntopicTables <- syntopicTables |>
      dplyr::mutate(
        "Constancy" = 
          dplyr::case_when(
            Constancy == 1 ~ "I",
            Constancy == 2 ~ "II",
            Constancy == 3 ~ "III",
            Constancy == 4 ~ "IV",
            Constancy == 5 ~ "V",
            TRUE ~ NA
          )
      ) |>
      dplyr::mutate("Constancy" = factor(Constancy, levels = c("V", "IV", "III", "II", "I")))
    
  }
  
  return(syntopicTables)
  
}