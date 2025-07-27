#' Subset a data frame of NVC data by habitat
#' 
#' Subset a data frame of NVC data such as pseudo-quadrats or syntopic tables by 
#' habitat type by supplying a vector of prefixes.
#'
#' @param nvc_data A data frame of pseudo-quadrats.
#' @param habitatRestriction A vector of habitat codes, see `RMAVIS:::habitatRestriction_options`.
#' @param col_name The name of the column containing NVC pseudo-quadrat names with NVC code prefixes.
#'
#' @return A data frame containing the supplied NVC data for selected habitats
#' @export
#'
#' @examples
#' RMAVIS::subset_nvcData(nvc_data = RMAVIS::nvc_pquads, habitatRestriction = c("U", "H"), col_name = "psq_id")
subset_nvcData <- function(nvc_data, habitatRestriction, col_name){
  
  prefixes_match <- unname(unlist(RMAVIS:::habitatRestrictionPrefixes[habitatRestriction]))
  prefixes_not_match <- unname(unlist(setdiff(RMAVIS:::habitatRestrictionPrefixes, habitatRestriction)))
  
  codes_regex <- paste0("^(", 
                        stringr::str_c(prefixes_match, collapse = "|"), 
                        ")", 
                        "[^(", 
                        stringr::str_c(prefixes_not_match, collapse = "|"), 
                        ")]")
  
  nvc_data_to_use <- nvc_data |>
    dplyr::filter(stringr::str_detect(string = .data[[col_name]], pattern = codes_regex))
  
  return(nvc_data_to_use)
  
}
