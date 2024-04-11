#' Read an input file downloaded from MAVIS
#' 
#' Read an input file downloaded from the desktop MAVIS application and wrangle
#' it into the format required by RMAVIS. Namely a data frame with the folowing
#' columns:
#' \describe{
#'   \item{Year}{The year, set to the year the function was used.}
#'   \item{Group}{The group, set to "A" by default.}
#'   \item{Quadrat}{The quadrat ID.}
#'   \item{Species}{The species name.}
#'   \item{Cover}{The cover values, note that 0 is replaced with 0.1}
#' }
#'
#' @param filepath The file path to a .txt file produced by MAVIS.
#'
#' @return A five column data frame containing the MAVIS input data in RMAVIS format.
#' @export
#'
#' @examples
read_mavis_data <- function(filepath){
 
  mavisData_raw <- readr::read_table(filepath, 
                                     col_names = c("Group", "BRCNumber", "?", "Genus", "SpeciesEpithet", "CoverOrSuffix", "EmptyOrCover"), 
                                     skip_empty_rows = FALSE)
  
  mavisData_plotData <- mavisData_raw |>
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
    dplyr::mutate(Year = as.integer(format(Sys.Date(), "%Y")), .before = 1) |>
    dplyr::mutate(Group = "A", .after = 1) |>
    dplyr::mutate(
      Cover = dplyr::case_when(
        Cover == 0 ~ 0.1,
        TRUE ~ Cover
      )
    )
  
  return(mavisData_final)
  
}


