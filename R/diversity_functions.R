#' ...
#' 
#' ...
#'
#' @param plot_data ...
#' @param higher_taxa ...
#' @param phylo_tree ...
#' @param phylo_taxa_lookup ...
#'
#' @returns ...
#' @export
#'
#' @examples
#' ...
calc_rdiversity_objects <- function(plot_data, higher_taxa, phylo_tree, phylo_taxa_lookup, groups = c("Year", "Group", "Quadrat")){
  
  # plot_data <- MNNPC::mnnpc_example_data$`St. Croix State Forest`
  # higher_taxa <- MNNPC::mnnpc_taxonomic_backbone |>
  #   tibble::as_tibble() |>
  #   dplyr::select("taxon_name" = "recommended_taxon_name",
  #                 "Kingdom" = "kingdom",
  #                 "Phylum" = "phylum",
  #                 "Class" = "class",
  #                 "Order" = "order",
  #                 "Family" = "family",
  #                 "Genus" = "genus") |>
  #   dplyr::distinct()
  # phylo_tree <- MNNPC::mnnpc_phylo_tree
  # phylo_taxa_lookup <- MNNPC::mnnpc_phylo_taxa_lookup
  # groups <- c("Year", "Group", "Quadrat")
  
  # Prepare standard matrix
  data_mat <- plot_data |>
    tidyr::unite(col = "ID", dplyr::any_of(groups)) |>
    tidyr::pivot_wider(id_cols = ID,
                       names_from = Species,
                       values_from = Cover,
                       values_fill = 0) |>
    tibble::column_to_rownames(var = "ID") |>
    as.matrix()
  
  # Diversity setup
  meta_naive <- rdiversity::metacommunity(t(data_mat)) 
  
  # Taxonomic diversity setup
  rank_level_names <- rev(c(setdiff(colnames(higher_taxa), "taxon_name"), "taxon_name"))
  rank_levels <- seq(from = 0, to = length(rank_level_names) - 1)
  names(rank_levels) <- rank_level_names
  
  higher_taxa_present <- higher_taxa |>
    dplyr::filter(taxon_name %in% unique(plot_data$Species))
  
  data_mat_higher_taxa <- data_mat[, higher_taxa_present$taxon_name, drop = FALSE]
  
  d_tax <- rdiversity::tax2dist(higher_taxa_present, rank_levels)
  
  s_tax <- rdiversity::dist2sim(d_tax, "linear")
  
  meta_tax <- rdiversity::metacommunity(t(data_mat_higher_taxa), s_tax) 
  
  # Phylogenetic analysis setup
  available_phylo_taxa_names <- phylo_taxa_lookup |> 
    dplyr::filter(phylo == TRUE)
    
  data_mat_phylo <- plot_data |>
    dplyr::inner_join(available_phylo_taxa_names, by = c("Species" = "taxon_name")) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(groups, "search_name")))) |>
    dplyr::summarise("Cover" = sum(Cover, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate("search_name" = stringr::str_replace_all(string = search_name, pattern = "\\s", replacement = "_")) |>
    tidyr::unite(col = "ID", dplyr::any_of(groups)) |>
    tidyr::pivot_wider(id_cols = ID,
                       names_from = search_name,
                       values_from = Cover,
                       values_fill = 0) |>
    tibble::column_to_rownames(var = "ID") |>
    as.matrix()
  
  data_mat_phylo_analysis <- rdiversity:::check_partition(data_mat_phylo) |> suppressMessages()
  
  phylo_tree_phylo <- ape::read.tree(text = phylo_tree)
  
  phylo_analysis_taxon_names <- intersect(colnames(data_mat_phylo_analysis), phylo_tree_phylo$tip.label)
  
  data_mat_phylo_use <- data_mat_phylo_analysis[, phylo_analysis_taxon_names, drop = FALSE]
  
  phylo_tree_use <- ape::keep.tip.phylo(phy = phylo_tree_phylo, tip = phylo_analysis_taxon_names)
  
  if(!is.null(phylo_tree_use)){
    
    d_phylo <- rdiversity::phy2dist(phylo_tree_use)
    s_phylo_dist <- rdiversity::dist2sim(d_phylo, "linear")
    
    meta_phylo_dist <- rdiversity::metacommunity(t(data_mat_phylo_use), s_phylo_dist)
    
  } else {
    
    meta_phylo_dist <- NULL
    
  }
  
  # Compose list of objects to return
  rdiv_objects <- list("meta_naive" = meta_naive,
                       "meta_tax" = meta_tax,
                       "meta_phylo_dist" = meta_phylo_dist)
  
  return(rdiv_objects)
  
  
}

#' ...
#' 
#' ...
#'
#' @param rdiv_objects 
#' @param q 
#'
#' @returns ...
#' @export
#'
#' @examples
#' ...
calc_rdiversity_metrics_subcom <- function(rdiv_objects, q = 0){
  
  # Retrieve objects
  meta_naive <- rdiv_objects[["meta_naive"]]
  meta_tax <- rdiv_objects[["meta_tax"]]
  meta_phylo_dist <- rdiv_objects[["meta_phylo_dist"]]
  
  # Naive diversity measures
  if(!is.null(meta_naive)){
    
    alpha_naive_norm <- rdiversity::subdiv(rdiversity::norm_alpha(meta_naive), qs = q)
    beta_naive_norm <- rdiversity::subdiv(rdiversity::norm_beta(meta_naive), qs = q)
    
    if(dim(meta_naive@similarity)[2] > 1){
      gamma_naive <- rdiversity::sub_gamma(meta_naive, q)
    } else {
      gamma_naive <- NULL
    }
    
  } else {
    
    alpha_naive_norm <- NULL
    beta_naive_norm <- NULL
    gamma_naive <- NULL
    
  }
  
  # Taxonomic diversity measures
  if(!is.null(meta_tax)){
    
    alpha_tax_norm <- rdiversity::subdiv(rdiversity::norm_alpha(meta_tax), qs = q)
    beta_tax_norm <- rdiversity::subdiv(rdiversity::norm_beta(meta_tax), qs = q)
    
    if(dim(meta_tax@similarity)[2] > 1){
      gamma_tax <- rdiversity::sub_gamma(meta_tax, q)
    } else {
      gamma_tax <- NULL
    }
    
  } else {
    
    alpha_tax_norm <- NULL
    beta_tax_norm <- NULL
    gamma_tax <- NULL
    
  }
  
  # Phylogenetic diversity measures
  if(!is.null(meta_phylo_dist)){
    
    alpha_phylo_norm_dist <- rdiversity::subdiv(rdiversity::norm_alpha(meta_phylo_dist), qs = q)
    beta_phylo_norm_dist <- rdiversity::subdiv(rdiversity::norm_beta(meta_phylo_dist), qs = q)
    
    if(dim(meta_phylo_dist@similarity)[2] > 1){
      gamma_phylo_dist <- rdiversity::sub_gamma(meta_phylo_dist, q)
    } else {
      gamma_phylo_dist <- NULL
    }
    
  } else {
    
    alpha_phylo_norm_dist <- NULL
    beta_phylo_norm_dist <- NULL
    gamma_phylo_dist <- NULL
    
  }
  
  # Collate results
  results <- dplyr::bind_rows(
    
    # Naive diversity
    alpha_naive_norm,
    beta_naive_norm,
    gamma_naive,
    
    # Taxonomic diversity
    alpha_tax_norm,
    beta_tax_norm,
    gamma_tax,
    
    # Phylogenetic diversity
    alpha_phylo_norm_dist,
    beta_phylo_norm_dist,
    gamma_phylo_dist,
    
  )
  
  return(results)
  
}

#' ...
#' 
#' ...
#'
#' @param rdiv_objects ...
#' @param q ...
#'
#' @returns...
#' @export
#'
#' @examples
#' ...
calc_rdiversity_metrics_meta <- function(rdiv_objects, q = 0){

  # Retrieve objects
  meta_naive <- rdiv_objects[["meta_naive"]]
  meta_tax <- rdiv_objects[["meta_tax"]]
  meta_phylo_dist <- rdiv_objects[["meta_phylo_dist"]]

  # Naive diversity measures
  if(!is.null(meta_naive)){
    
    norm_meta_alpha_naive <- rdiversity::norm_meta_alpha(meta_naive, qs = q)
    norm_meta_beta_naive <- rdiversity::norm_meta_beta(meta_naive, qs = q)
    
    if(dim(meta_naive@similarity)[2] > 1){
      gamma_naive <- rdiversity::meta_gamma(meta_naive, qs = q)
    } else {
      gamma_naive <- NULL
    }
    
  } else {
    
    norm_meta_alpha_naive <- NULL
    norm_meta_beta_naive <- NULL
    gamma_naive <- NULL
    
  }

  # Taxonomic diversity measures
  if(!is.null(meta_tax)){
    
    norm_meta_alpha_tax <- rdiversity::norm_meta_alpha(meta_tax, qs = q)
    norm_meta_beta_tax <- rdiversity::norm_meta_beta(meta_tax, qs = q)
    
    if(dim(meta_tax@similarity)[2] > 1){
      gamma_tax <- rdiversity::meta_gamma(meta_tax, qs = q)
    } else {
      gamma_tax <- NULL
    }
    
  } else {
    
    norm_meta_alpha_tax <- NULL
    norm_meta_beta_tax <- NULL
    gamma_tax <- NULL
    
  }

  # Phylogenetic diversity measures
  if(!is.null(meta_phylo_dist)){
    
    norm_meta_alpha_phylo_dist <- rdiversity::norm_meta_alpha(meta_phylo_dist, qs = q)
    norm_meta_beta_phylo_dist <- rdiversity::norm_meta_beta(meta_phylo_dist, qs = q)
    
    if(dim(meta_phylo_dist@similarity)[2] > 1){
      gamma_phylo_dist <- rdiversity::meta_gamma(meta_phylo_dist, qs = q)
    } else {
      gamma_phylo_dist <- NULL
    }
    
  } else {
    
    norm_meta_alpha_phylo_dist <- NULL
    norm_meta_beta_phylo_dist <- NULL
    gamma_phylo_dist <- NULL
    
  }

  # Collate results
  results <- dplyr::bind_rows(

    # Naive diversity
    norm_meta_alpha_naive,
    norm_meta_beta_naive,
    gamma_naive,

    # Taxonomic diversity
    norm_meta_alpha_tax,
    norm_meta_beta_tax,
    gamma_tax,

    # Phylogenetic diversity
    norm_meta_alpha_phylo_dist,
    norm_meta_beta_phylo_dist,
    gamma_phylo_dist

  )

  return(results)

}