#' Title
#'
#' @param filepath The file path to a .txt file produced by MAVIS.
#'
#' @return
#' @export
#'
#' @examples
read_mavis_data <- function(filepath){
 
  mavisData_raw <- readr::read_table(file, 
                                     col_names = c("Group", "BRCNumber", "?", "Genus", "SpeciesEpithet", "CoverOrSuffix", "EmptyOrCover"), 
                                     skip_empty_rows = FALSE)
  
  mavisData_plotData <- mavisData |>
    dplyr::mutate(
      "Cover" = dplyr::case_when(
        !is.na(EmptyOrCover) ~ as.numeric(EmptyOrCover),
        TRUE ~ as.numeric(CoverOrSuffix)
      )
    ) |>
    dplyr::mutate(
      "SpeciesSuffix" = 
        dplyr::case_when(
          !is.na(EmptyOrCover) ~ as.character(CoverOrSuffix),
          TRUE ~ NA
        )
    ) |>
    dplyr::select(-CoverOrSuffix, -EmptyOrCover) |>
    tidyr::unite(col = "Species", Genus, SpeciesEpithet, SpeciesSuffix, na.rm = TRUE, sep = " ")
  
  mavisData_plots <- mavisData_raw |>
    dplyr::group_by(Group) |>
    dplyr::slice(1) |>
    tidyr::unite(col = "Quadrat", BRCNumber, `?`, Genus, SpeciesEpithet, CoverOrSuffix, EmptyOrCover, na.rm = TRUE, sep = " ") |>
    dplyr::mutate(Quadrat = stringr::str_remove_all(string = Quadrat, pattern = "<|>")) |>
    dplyr::ungroup()
  
  mavisData_final <- mavisData_plotData |>
    dplyr::left_join(mavisData_plots, by = "Group") |>
    dplyr::filter(!is.na(Cover)) |>
    dplyr::select(Quadrat, Species, Cover) |>
    dplyr::mutate(Year = 2023, .before = 1) |>
    dplyr::mutate(Group = "A", .after = 1) |>
    dplyr::mutate(Cover = Cover / 100)
  
  return(mavisData_final)
  
}


