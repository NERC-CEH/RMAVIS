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
#' @param downweight_threshold If 0 no downeighting occurs.
#' @param downweight_value
#'
#' @return
#' @export
#'
#' @examples
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
      
      samp_data_prepped <- samp_data[,c(samp_species_col, "freq_samp")]
      comp_data_prepped <- comp_data[,c(comp_species_col, "freq_comp")]
      
      eval_table <- merge(x = samp_data_prepped, y = comp_data_prepped, by = "Species", all = TRUE)
      
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
  
  return(similarity_df)
  
}


# I need to optimise this function before moving from the current {assignNVC}
# implementation.
similarityJaccard <- function(samp_df, comp_df,
                              samp_species_col, comp_species_col,
                              samp_group_name = "ID", comp_group_name = "Pid3",
                              comp_groupID_name = "NVC", average_comp = TRUE){
  
  # Create a comp_groupID_name to comp_group_name concordance to join back on later
  comp_conc <- comp_df[,c(comp_group_name, comp_groupID_name)] |> unique()
  
  # Create a set of pairwise combinations for each samp_group_name and comp_group_name value.
  eval_combinations <- expand.grid(unique(samp_df[[samp_group_name]]), unique(comp_df[[comp_group_name]]), stringsAsFactors = FALSE)
  names(eval_combinations) <- c(samp_group_name, comp_group_name)
  
  # Produce a list of species present in samp_df by samp_group_name
  samp_spp_list <- as.environment(tapply(samp_df[[samp_species_col]], samp_df[[samp_group_name]], unique))
  
  # Produce a list of species present in comp_df by comp_group_name
  comp_spp_list <- as.environment(tapply(comp_df[[comp_species_col]], comp_df[[comp_group_name]], unique))
  
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
      
      # Contingency Table Approach as per Legendre & Legendre (2012)
      # # Species present in samp AND comp
      # a <- length(intersect(samp_spp, comp_spp))
      # 
      # # Species present in samp BUT NOT comp
      # b <- length(setdiff(samp_spp, comp_spp))
      # 
      # # Species present in comp BUT NOT samp
      # c <- length(setdiff(comp_spp, samp_spp))
      # 
      # # Jaccard coefficient of similarity
      # j <- a / (a + b + c)
      
      # Standard Method
      x <- x_list[[X]]
      y <- y_list[[Y]]
      x_n_y <- sum(samp_spp %in% comp_spp)
      # x_n_y <- length(intersect(samp_spp, comp_spp)) # %in% faster than intersect
      j <- x_n_y / (x + y - x_n_y)
      
      # !!! Both of the methods above produce identical results !!!
      
      similarity_list <- list(samp_group_name = X, comp_group_name = Y, "Similarity" = j)
      names(similarity_list) <- c(samp_group_name , comp_group_name, "Similarity")
      
      return(similarity_list)
      
    }
    
  )
  
  # Collapse list to data frame
  similarity_df <- do.call(rbind.data.frame, similarity_results)
  
  # Add NVC codes
  similarity_df <- merge(similarity_df, comp_conc, by = comp_group_name)
  
  # Optionally remove zero matches
  if(remove_zero_matches == TRUE){
    
    similarity_df <- similarity_df[similarity_df["Similarity"] != 0, ]
    
  }
  
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
  
  return(similarity_df)
  
}