#' Subset a data frame of VC data by habitat
#' 
#' Subset a data frame of VC data such as pseudo-quadrats or syntopic tables by 
#' habitat type by supplying a vector of prefixes.
#'
#' @param nvc_data A data frame of pseudo-quadrats.
#' @param habitatRestriction A vector of habitat codes, see `RMAVIS:::habitatRestriction_options`.
#' @param col_name The name of the column containing VC pseudo-quadrat names with VC code prefixes.
#' @param habitatRestrictionPrefixes A named list of vegetation classification types e.g. M in the GB-NVC.
#'
#' @return A data frame containing the supplied VC data for selected habitats
#' @export
#'
#' @examples
#' RMAVIS::subset_vcData(vc_data = RMAVIS::nvc_pquads, habitatRestriction = c("U", "H"), col_name = "psq_id")
subset_vcData <- function(vc_data, habitatRestriction, col_name, habitatRestrictionPrefixes = RMAVIS:::habitatRestrictionPrefixes){
  
  prefixes_match <- unname(unlist(habitatRestrictionPrefixes[habitatRestriction]))
  prefixes_not_match <- unname(unlist(setdiff(habitatRestrictionPrefixes, habitatRestriction)))
  
  codes_regex <- paste0("^(", 
                        stringr::str_c(prefixes_match, collapse = "|"), 
                        ")", 
                        "[^(", 
                        stringr::str_c(prefixes_not_match, collapse = "|"), 
                        ")]")
  
  vc_data_to_use <- vc_data |>
    dplyr::filter(stringr::str_detect(string = .data[[col_name]], pattern = codes_regex))
  
  return(vc_data_to_use)
  
}
