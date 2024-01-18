#' Calculate the similarity between vegetation data using the Czekanowski index
#'
#' Calculate the Czekanowski's Quantitative Index using either a set of sample 
#' vegetation plots and a set of reference vegetation plots; or a set of composed
#' floristic/syntopic tables and a set of references floristic/syntopic tables.
#' Method follows Bray and Curtis (1957) and  Field and McFarlane (1968).
#'
#' @param samp_df A data frame containing either sample vegetation plot data with cover values, or a syntopic/floristic table composed from sample vegetation plot data.
#' @param comp_df  A data frame containing either
#' @param samp_species_col 
#' @param comp_species_col 
#' @param samp_group_name 
#' @param comp_group_name 
#' @param samp_weight_name 
#'
#' @return
#' @export
#'
#' @examples
similarityCzekanowski <- function(samp_df, comp_df, 
                                  samp_species_col, comp_species_col, 
                                  samp_group_name, comp_group_name,
                                  samp_weight_name, comp_weight_name){
  
  # Check argument types are correct
  checkmate::assertDataFrame(samp_df)
  checkmate::assertDataFrame(comp_df)
  checkmate::assert_character(samp_species_col, any.missing = FALSE)
  checkmate::assert_character(comp_species_col, any.missing = FALSE)
  checkmate::assert_character(samp_group_name, any.missing = FALSE)
  checkmate::assert_character(comp_group_name, any.missing = FALSE)
  checkmate::assert_character(samp_weight_name, any.missing = FALSE)
  checkmate::assert_character(comp_weight_name, any.missing = FALSE)
  
  # Split input data frames into iterable lists
  samp_df_split <- split(samp_df, samp_df[[samp_group_name]])
  comp_df_split <- split(comp_df, comp_df[[comp_group_name]])
  
  # Create a set of pairwise combinations for each samp_group_name and comp_group_name value.
  eval_combinations <- expand.grid(names(samp_df_split), names(comp_df_split))
  names(eval_combinations) <- c(samp_group_name, comp_group_name)
  
  # Calculate the Czekanowski for each combination of samp_group_name and comp_group_name value.
  similarity_results <- mapply(
    X = eval_combinations[[samp_group_name]], Y = eval_combinations[[comp_group_name]],
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE,
    FUN = function(X, Y){
      
      samp_data <- samp_df_split[[X]]
      comp_data <- comp_df_split[[Y]]
      
      samp_id <- unique(samp_data[[samp_group_name]])
      comp_id <- unique(comp_data[[comp_group_name]])
      
      samp_data_prepped <- samp_data[,c(samp_species_col, samp_weight_name)]
      comp_data_prepped <- comp_data[,c(comp_species_col, comp_weight_name)]
      
      eval_table <- merge(x = samp_data_prepped, y = comp_data_prepped, by = "Species", all = TRUE, suffixes = c("_samp", "_comp"))
      
      eval_table[is.na(eval_table)] <- 0
      
      eval_table["min"] <- apply(eval_table[c(paste0(samp_weight_name, "_samp"), paste0(comp_weight_name, "_comp"))], 1, min)
      
      eval_table_sum <- colSums(eval_table[,c(paste0(samp_weight_name, "_samp"), paste0(comp_weight_name, "_comp"), "min")], na.rm = TRUE)
      
      similarity <- (2 * eval_table_sum[["min"]]) / (eval_table_sum[[paste0(samp_weight_name, "_samp")]] + eval_table_sum[[paste0(comp_weight_name, "_comp")]])
      
      similarity_list <- list(samp_group_name = samp_id, comp_group_name = comp_id, "Similarity" = similarity)
      names(similarity_list) <- c(samp_group_name , comp_group_name, "Similarity")
      
      return(similarity_list)
      
    }
  )
  
  # Collapse list to data frame
  similarity_df <- do.call(rbind.data.frame, similarity_results)
  
  return(similarity_df)
  
}