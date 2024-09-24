retrieve_nested_element <- function(nested_list, focal_species){
  
  element <- purrr::map_depth(nested_list, 1, purrr::pluck(focal_species))|>
    purrr::discard(is.null) |>
    purrr::pluck(1)
  
  return(element)
  
}