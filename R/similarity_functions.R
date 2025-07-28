#' Calculate the similarity between two vegetation plot datasets using the Czekanowski coefficient
#'
#' Calculate the Czekanowski's quantitative index/coefficient of similarity 
#' using either a set of sample vegetation plots and a set of reference 
#' vegetation plots; or a set of composed floristic/syntopic tables and a set of 
#' references floristic/syntopic tables. Method follows Malloch (1998).
#'
#' @param samp_df A data frame containing either sample vegetation plot data with cover values, or a syntopic/floristic table composed from sample vegetation plot data.
#' @param comp_df  A data frame containing either
#' @param samp_species_col The name of the column in samp_df containing the species names.
#' @param comp_species_col The name of the column in comp_df containing the species names.
#' @param samp_group_name The name of the column in samp_df containing the group name.
#' @param comp_group_name The name of the column in comp_df containing the group name.
#' @param samp_weight_name The name of the column in samp_df containing the numeric weight values.
#' @param comp_weight_name The name of the column in comp_df containing the numeric weight values.
#' @param downweight_threshold A numeric value to replace with downweight_threshold.
#' @param downweight_value A numeric value to replace down_weight threshold with.
#'
#' @return A three column data frame containing the samp_group_name values, comp_group_name values, and similarity results (Similarity).
#' @export
#'
#' @examples
#' RMAVIS::similarityCzekanowski(samp_df = RMAVIS::composeSyntopicTables(surveyData = RMAVIS::example_data[["Parsonage Down"]], 
#'                                                                       group_cols = c("Year", "Group"), 
#'                                                                       species_col_name = "Species", 
#'                                                                       plot_col_name = "Quadrat",
#'                                                                       numeric_constancy = TRUE), 
#'                               comp_df = RMAVIS::subset_nvcData(nvc_data = RMAVIS::nvc_floristic_tables, 
#'                                                                habitatRestriction = c("CG"), 
#'                                                                col_name = "nvc_code"), 
#'                               samp_species_col = "Species", 
#'                               comp_species_col = "nvc_taxon_name", 
#'                               samp_group_name = "ID", 
#'                               comp_group_name = "nvc_code",
#'                               samp_weight_name = "Constancy", 
#'                               comp_weight_name = "constancy",
#'                               downweight_threshold = 1, 
#'                               downweight_value = 0.1)
similarityCzekanowski <- function(samp_df, comp_df, 
                                  samp_species_col, comp_species_col, 
                                  samp_group_name, comp_group_name,
                                  samp_weight_name, comp_weight_name,
                                  downweight_threshold = 1, downweight_value = 0.1){
  
  # Split input data frames into iterable lists
  samp_df_split <- split(samp_df, samp_df[[samp_group_name]])
  comp_df_split <- split(comp_df, comp_df[[comp_group_name]])
  
  # Create a set of pairwise combinations for each samp_group_name and comp_group_name value.
  # Can wrap names() in unique() but each samp group and comp group should be
  # unique already.
  eval_combinations <- expand.grid(names(samp_df_split), names(comp_df_split), stringsAsFactors = FALSE)
  names(eval_combinations) <- c(samp_group_name, comp_group_name)
  
  # Calculate the Czekanowski for each combination of samp_group_name and comp_group_name value.
  similarity_results <- mapply(
    X = eval_combinations[[samp_group_name]], Y = eval_combinations[[comp_group_name]],
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE,
    FUN = function(X, Y){
      
      samp_data <- samp_df_split[[X]]
      comp_data <- comp_df_split[[Y]]
      
      names(comp_data)[names(comp_data) == comp_weight_name] <- "freq_comp"
      names(samp_data)[names(samp_data) == samp_weight_name] <- "freq_samp"
      
      names(samp_data)[names(samp_data) == samp_species_col] <- comp_species_col
      
      samp_data_prepped <- samp_data[,c(comp_species_col, "freq_samp")]
      comp_data_prepped <- comp_data[,c(comp_species_col, "freq_comp")]
      
      eval_table <- merge(x = samp_data_prepped, y = comp_data_prepped, by = comp_species_col, all = TRUE)
      
      eval_table[is.na(eval_table)] <- 0
      
      # Down-weight species absent in the sample data but present in the 
      # comparison data at a constancy of downweight_threshold to downweight_value
      eval_table[["freq_comp"]][eval_table[["freq_samp"]] == 0 & eval_table[["freq_comp"]] == downweight_threshold] <- downweight_value
      
      eval_table["min"] <- apply(eval_table[c("freq_samp", "freq_comp")], 1, min)
      
      eval_table_sum <- colSums(eval_table[,c("freq_samp", "freq_comp", "min")], na.rm = TRUE)
      
      similarity <- (2 * eval_table_sum[["min"]]) / (eval_table_sum[["freq_samp"]] + eval_table_sum[["freq_comp"]])
      
      similarity_list <- list(samp_group_name = X, comp_group_name = Y, "Similarity" = similarity)
      names(similarity_list) <- c(samp_group_name , comp_group_name, "Similarity")
      
      return(similarity_list)
      
    }
  )
  
  # Collapse list to data frame
  similarity_df <- do.call(rbind.data.frame, similarity_results)
  
  # Remove rownames
  rownames(similarity_df) <- NULL
  
  return(similarity_df)
  
}


#' Calculate the similarity between two vegetation plot datasets using the Jaccard coefficient
#' 
#' Calculate the Jaccard coefficient of similarity between two sets of vegetation
#' plot data. The Jaccard coefficient is a binary, presence/absence only
#' measure of similarity.
#'
#' @param samp_df A data frame containing species presences (recorded in the samp_species_col column) by plot ID (recorded in the samp_species_col) column
#' @param comp_df A data frame containing species presences (recorded in the comp_species_col column) by plot ID (recorded in the comp_species_col) column with plot ID groups (recorded in the comp_groupID_column)
#' @param samp_species_col The name of the column in samp_df containing the species names.
#' @param comp_species_col The name of the column in comp_df containing the species names.
#' @param samp_group_name The name of the column in samp_df containing the group name.
#' @param comp_group_name The name of the column in comp_df containing the group name.
#' @param comp_groupID_name The name of the column containing the groups for each
#'                          comp_group_name value.
#' @param remove_zero_matches Exclude any results with a similarity of 0.
#' @param average_comp Take the mean average across the groups in comp_group_name by comp_groupID_name.
#'
#' @return A three column data frame containing the samp_group_name values, comp_group_name values or comp_groupID_name values, and similarity results (Similarity).
#' @export
#'
#' @examples
#' RMAVIS::similarityJaccard(samp_df = RMAVIS::example_data[["Parsonage Down"]], 
#'                           comp_df = RMAVIS::subset_nvcData(nvc_data = RMAVIS::nvc_pquads, 
#'                                                            habitatRestriction = c("CG"), 
#'                                                            col_name = "psq_id"),
#'                           samp_species_col = "Species", 
#'                           comp_species_col = "nvc_taxon_name",
#'                           samp_group_name = "Quadrat", 
#'                           comp_group_name = "psq_id",
#'                           comp_groupID_name = "nvc_code", 
#'                           remove_zero_matches = TRUE, 
#'                           average_comp = TRUE)
similarityJaccard <- function(samp_df, comp_df,
                              samp_species_col, comp_species_col = "nvc_taxon_name",
                              samp_group_name = "ID", comp_group_name = "psq_id",
                              comp_groupID_name = "nvc_code", 
                              remove_zero_matches = TRUE, average_comp = TRUE){
  
  # Create a comp_groupID_name to comp_group_name concordance to join back on later
  comp_conc <- comp_df[,c(comp_group_name, comp_groupID_name)] |> unique()
  
  # Create a set of pairwise combinations for each samp_group_name and comp_group_name value.
  eval_combinations <- expand.grid(unique(samp_df[[samp_group_name]]), unique(comp_df[[comp_group_name]]), stringsAsFactors = FALSE)
  names(eval_combinations) <- c(samp_group_name, comp_group_name)
  
  # Produce a list of species present in samp_df by samp_group_name
  samp_spp_list <- as.environment(as.list(tapply(samp_df[[samp_species_col]], samp_df[[samp_group_name]], unique)))
  
  # Produce a list of species present in comp_df by comp_group_name
  comp_spp_list <- as.environment(as.list(tapply(comp_df[[comp_species_col]], comp_df[[comp_group_name]], unique)))
  
  # Precalculate the number of species in each samp_spp_list and comp_spp_list element
  x_list <- as.environment(lapply(samp_spp_list, length))
  y_list <- as.environment(lapply(comp_spp_list, length))
  
  # Calculate the Jaccard similarity for each combination of samp_spp and comp_spp value.
  similarity_results <- mapply(
    X = eval_combinations[[samp_group_name]], Y = eval_combinations[[comp_group_name]],
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE,
    FUN = function(X, Y){
      
      # Subset species for X and Y
      samp_spp <- samp_spp_list[[X]]
      comp_spp <- comp_spp_list[[Y]]
      
      # # Standard Method
      x <- x_list[[X]]
      y <- y_list[[Y]]
      x_n_y <- sum(samp_spp %in% comp_spp)
      j <- x_n_y / (x + y - x_n_y)
      
      return(j)
      
      base::gc()
      
    }
    
  )
  
  # Collapse list to data frame
  similarity_results_df <- data.frame("Similarity" = do.call(rbind, similarity_results))
  
  # Bind similarity results to ID values
  similarity_df <- cbind(eval_combinations, similarity_results_df)
  
  # Add NVC codes
  similarity_df <- merge(similarity_df, comp_conc, by = comp_group_name)
  
  # Optionally take the mean across all similarities by comp_group_name
  if(average_comp == TRUE){
    
    similarity_df <- setNames(
      aggregate(x = similarity_df[["Similarity"]], 
                by = list(
                  samp_group_name = similarity_df[[samp_group_name]],
                  comp_groupID_name = similarity_df[[comp_groupID_name]]
                ),
                FUN = mean,
                na.rm = TRUE),
      c(samp_group_name, 
        comp_groupID_name,
        "Similarity")
    )
    
  }
  
  # Optionally remove zero matches
  if(remove_zero_matches == TRUE){
    
    similarity_df <- similarity_df[similarity_df["Similarity"] != 0, ]
    
  }
  
  # Remove rownames
  rownames(similarity_df) <- NULL
  
  # Return similarity results
  return(similarity_df)
  
}
