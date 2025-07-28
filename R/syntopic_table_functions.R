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
#' @param cover_col_name The name of the cover/abundance cover
#' @param factor_constancy A boolean value (TRUE/FALSE) indicating whether the constancy column should be returned as a factor or not.
#' @param numeral_constancy If TRUE the numeric constancy classes (1, 2, 3, 4, 5) are transformed into roman numeral values (I, II, III, IV, V).
#' @param remove_low_freq_taxa If not NULL, a float greater than 0 and less than 1 indicating the minimum relative frequency of taxa to include in the floristic tables.
#'
#' @return A three column data frame containing the groups ID, species, and constancy.
#' @export
#'
#' @examples
#' RMAVIS::composeSyntopicTables(surveyData = RMAVIS::example_data[["Newborough Warren"]], 
#'                               group_cols = c("Year", "Group"), 
#'                               species_col_name = "Species", 
#'                               plot_col_name = "Quadrat",
#'                               cover_col_name = "Cover",
#'                               factor_constancy = TRUE,
#'                               numeral_constancy = FALSE,
#'                               remove_low_freq_taxa = 0.05)
composeSyntopicTables <- function(surveyData, group_cols, species_col_name = "Species", plot_col_name = "Quadrat", 
                                  cover_col_name = "Cover", factor_constancy = TRUE,
                                  numeral_constancy = FALSE, remove_low_freq_taxa = NULL){
  
  if(is.null(remove_low_freq_taxa)){
    
    filter_threshold <- 0
    
  } else if(remove_low_freq_taxa >= 1 | remove_low_freq_taxa <= 0){
    
    filter_threshold <- 0
    
  } else {
    
    filter_threshold <- remove_low_freq_taxa
    
  }
  
  # Calculate the absolute frequency and relative frequency of species occurrence across all plots by group
  syntopicTables_init <- surveyData |>
    tidyr::unite(col = "ID", group_cols, sep = " - ", remove = TRUE) |>
    dplyr::group_by(ID) |>
    dplyr::mutate("n" = dplyr::n_distinct(Quadrat)) |>
    dplyr::group_by(ID, .data[[species_col_name]]) |>
    dplyr::mutate("freq" = dplyr::n()) |>
    dplyr::mutate("rel_freq" = freq / n) |>
    dplyr::summarise("rel_freq" = unique(rel_freq),
                     "min_cover" = min(.data[[cover_col_name]]),
                     "mean_cover" = mean(.data[[cover_col_name]]),
                     "max_cover" = max(.data[[cover_col_name]])) |>
    dplyr::ungroup() |>
    dplyr::filter(rel_freq > filter_threshold) |>
    dplyr::mutate(
      "constancy" =
        dplyr::case_when(
          rel_freq <= 0.2 ~ 1,
          rel_freq <= 0.4 ~ 2,
          rel_freq <= 0.6 ~ 3,
          rel_freq <= 0.8 ~ 4,
          rel_freq <= 1.0 ~ 5,
          TRUE ~ as.numeric(rel_freq)
        )
    ) |>
    dplyr::select(ID, "Species" = species_col_name, "Constancy" = "constancy",
                  "Min.Cover" = min_cover, "Mean.Cover" = mean_cover, "Max.Cover" = max_cover) 
  
  if(numeral_constancy == FALSE & factor_constancy == FALSE){
    
    syntopicTables <- syntopicTables_init  |>
      dplyr::arrange(ID, dplyr::desc(Constancy), species_col_name)
    
  } else if(numeral_constancy == FALSE & factor_constancy == TRUE){
    
    syntopicTables <- syntopicTables_init |>
      dplyr::mutate("Constancy" = factor(Constancy, levels = c(5, 4, 3, 2, 1))) |>
      dplyr::arrange(ID, Constancy, species_col_name)
    
  } else if(numeral_constancy == TRUE & factor_constancy == TRUE){
    
    syntopicTables <- syntopicTables_init |>
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
      dplyr::mutate("Constancy" = factor(Constancy, levels = c("V", "IV", "III", "II", "I"))) |>
      dplyr::arrange(ID, Constancy, species_col_name)
    
  } else if(numeral_constancy == TRUE & factor_constancy == FALSE){
    
    syntopicTables <- syntopicTables_init |>
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
      dplyr::arrange(ID, dplyr::desc(Constancy), species_col_name)
    
  }
  
  return(syntopicTables)
  
}