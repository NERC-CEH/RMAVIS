#' Use NBN Bulk Species Lookup API
#' 
#' Uses the NBN Bulk Species Lookup API (https://api.nbnatlas.org/#ws87) to
#' retrieve taxonomic information on a vector or list of species names.
#'
#' @param species 
#'
#' @return
#' @export
#'
#' @examples
call_nbn_bulk_lookup <- function(species){
  
  base_url <- "https://species-ws.nbnatlas.org/species/lookup/bulk/"
  
  species <- surveyData_original$Species |> unique()
  
  response <- httr2::request(base_url) |>
    httr2::req_body_json(list("names" = species)) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  
  response_df <- data.table::rbindlist(response) |> 
    suppressWarnings() |>
    dplyr::select(identifier, guid, name, nameComplete, rank, acceptedConceptGuid, acceptedConceptName, taxonomicStatus, author) |>
    dplyr::distinct()
  
  return(response_df)
  
}
