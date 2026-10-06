#' Produce objects required for rdiversity
#' 
#' Produce the objects required by the `rdiversity` functions 
#' `rdiversity::subdiv`, `rdiversity::raw_alpha`, `rdiversity::raw_beta`, 
#' `rdiversity::sub_gamma`, `rdiversity::raw_meta_alpha`, `rdiversity::raw_meta_beta`,
#' `rdiversity::meta_gamma` for the calculation of the naive, taxonomic, and phylogenetic
#'  diversity of a collection of vegetation plots as implemented in the wrapper functions
#'  `RMAVIS::calc_rdiversity_metrics_subcom` and `RMAVIS::calc_rdiversity_metrics_meta`.
#'
#' @param plot_data A data frame containing vegetation plot data, e.g. `RMAVIS::RMAVIS::example_data$`Parsonage Down``
#' @param higher_taxa A data frame containing the higher taxa associated with atleast the taxa present in plot_data, e.g. the taxon_name, Kingdom, Phylum, Class, Order, Family, and Genus columns present in `UKVegTB::taxonomic_backbone`.
#' @param phylo_tree A phylogenetic tree in the format of a Newick string, e.g. `UKVegTB::phylo_tree`.
#' @param phylo_taxa_lookup A data frame containing a lookup between the taxon_name values present in the plot_data, Open Tree of Life names and codes, e.g. `UKVegTB::phylo_taxa_lookup`.
#' @param traits A data frame containing the traits of interest.
#' @param groups The grouping variables in plot_data, identifying one plot observation.
#'
#' @returns A list of four objects: meta_naive, meta_tax, meta_phylo, and meta_func
#' @export
#'
#' @examples
#' \dontrun{
#' RMAVIS::calc_rdiversity_objects(plot_data = dplyr::filter(RMAVIS::example_data$`Parsonage Down`, Year == 1970 & Quadrat == "3N.1"), 
#'                                 higher_taxa = dplyr::distinct(UKVegTB::taxonomic_backbone, taxon_name, Kingdom, Phylum, Class, Order, Family, Genus), 
#'                                 phylo_tree = UKVegTB::phylo_tree, 
#'                                 phylo_taxa_lookup = UKVegTB::phylo_taxa_lookup,
#'                                 traits = UKVegTB::traits |> dplyr::filter(trait_name %in% c("LDMC", "predicted_CSR")),
#'                                 groups = c("Year", "Group", "Quadrat"))
#' 
#' }
calc_rdiversity_objects <- function(plot_data, higher_taxa = NULL, phylo_tree = NULL, phylo_taxa_lookup = NULL, traits = NULL, groups = c("Year", "Group", "Quadrat")){
  
  # Prepare standard matrix
  data_mat <- plot_data |>
    dplyr::distinct() |>
    tidyr::unite(col = "ID", dplyr::any_of(groups)) |>
    tidyr::pivot_wider(id_cols = ID,
                       names_from = Species,
                       values_from = Cover,
                       values_fill = 0) |>
    tibble::column_to_rownames(var = "ID") |>
    as.matrix()
  
  # Naive diversity setup
  meta_naive <- rdiversity::metacommunity(t(data_mat)) 
  
  # Taxonomic diversity setup
  if(!is.null(higher_taxa)){
    
    rank_level_names <- rev(c(setdiff(colnames(higher_taxa), "taxon_name"), "taxon_name"))
    rank_levels <- seq(from = 0, to = length(rank_level_names) - 1)
    names(rank_levels) <- rank_level_names
    
    higher_taxa_present <- higher_taxa |>
      dplyr::filter(taxon_name %in% unique(plot_data$Species)) |>
      tidyr::drop_na()
    
    data_mat_higher_taxa <- data_mat[, higher_taxa_present$taxon_name, drop = FALSE]
    
    d_tax <- rdiversity::tax2dist(higher_taxa_present, rank_levels)
    
    s_tax <- rdiversity::dist2sim(d_tax, "linear")
    
    if(ncol(data_mat_higher_taxa) > 1){
      
      meta_tax <- rdiversity::metacommunity(t(data_mat_higher_taxa), s_tax)
      
    } else {
      
      meta_tax <- NULL
      
    }
    
  } else {
    
    meta_tax <- NULL
    
  }
  
  # Phylogenetic diversity setup
  if(!is.null(phylo_taxa_lookup) & !is.null(phylo_tree)){
    
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
    
    if(!is.null(phylo_tree_use) & ncol(data_mat_phylo_use) > 1){
      
      d_phylo <- rdiversity::phy2dist(phylo_tree_use)
      s_phylo_dist <- rdiversity::dist2sim(d_phylo, "linear")
      
      meta_phylo_dist <- rdiversity::metacommunity(t(data_mat_phylo_use), s_phylo_dist)
      
    } else {
      
      meta_phylo_dist <- NULL
      
    }
    
  } else {
    
    meta_phylo_dist <- NULL
    
  }
  
  # Functional diversity setup
  if(!is.null(traits)){
    
    traits_wide <- traits |>
      dplyr::filter(recommended_taxon_name %in% colnames(data_mat)) |>
      dplyr::select(recommended_taxon_name, trait_name, trait_value) |>
      dplyr::distinct(recommended_taxon_name, trait_name, .keep_all = TRUE) |>
      tidyr::pivot_wider(id_cols = recommended_taxon_name, 
                         names_from = trait_name, 
                         values_from = trait_value) |>
      tibble::column_to_rownames("recommended_taxon_name")
    
    if(nrow(traits_wide) > 1){
      
      traits_wide_gawdis <- try(gawdis::gawdis(traits_wide, w.type = "equal", silent = T))
      
      if(class(traits_wide_gawdis) == "try-error"){
        
        meta_func <- NULL
        
      } else {
        
        traits_wide_gawdis <- gawdis::gawdis(traits_wide, w.type = "equal", silent = T)
        
        functional_distance_matrix <- rdiversity::similarity(traits_wide_gawdis |> as.matrix(), "functional")
        
        missing_taxa <- setdiff(rownames(t(data_mat)), colnames(functional_distance_matrix@similarity))
        
        data_mat_w_traits <- data_mat[, colnames(functional_distance_matrix@similarity), drop = FALSE]
        
        meta_func <- rdiversity::metacommunity(t(data_mat_w_traits), functional_distance_matrix)
        
      }
      
      
    } else {
      
      meta_func <- NULL
      
    }
    
  } else {
    
    meta_func <- NULL
    
  }
  
  # Compose list of objects to return
  rdiv_objects <- list("meta_naive" = meta_naive,
                       "meta_tax" = meta_tax,
                       "meta_phylo" = meta_phylo_dist,
                       "meta_func" = meta_func)
  
  return(rdiv_objects)
  
  
}

#' Calculate diversity metrics for a subcommunity
#' 
#' Calculate a set of subcommunity diversity measures and metrics using the objects
#' produced by the function `RMAVIS::calc_rdiversity_objects`.
#'
#' @param rdiv_objects A list of three objects: meta_naive, meta_tax, and meta_phylo, produced using the function `RMAVIS::calc_rdiversity_objects`.
#' @param measures One or more of "alpha", "beta", and "gamma".
#' @param metrics One or more of "naive", "taxonomic", "phylogenetic", and "functional".
#' @param q A positive number representing a Hill-Number.
#'
#' @returns A data frame containing the subcommunity partition diversity metrics for all combinations of specified measures and metrics
#' @export
#'
#' @examples
#' \dontrun{
#' rdiv_objs <- RMAVIS::calc_rdiversity_objects(plot_data = dplyr::filter(RMAVIS::example_data$`Parsonage Down`, Year == 1970 & Quadrat == "3N.1"), 
#'                                 higher_taxa = dplyr::distinct(UKVegTB::taxonomic_backbone, taxon_name, Kingdom, Phylum, Class, Order, Family, Genus), 
#'                                 phylo_tree = UKVegTB::phylo_tree, 
#'                                 phylo_taxa_lookup = UKVegTB::phylo_taxa_lookup) |>
#'                RMAVIS::calc_rdiversity_metrics_subcom(measures = c("alpha", "beta", "gamma"), metrics = c("naive", "taxonomic", "phylogenetic"), q = 1)                                
#' }
calc_rdiversity_metrics_subcom <- function(rdiv_objects, measures = c("alpha", "beta", "gamma"), metrics = c("naive", "taxonomic", "phylogenetic", "functional"), q = 0){
  
  # rdiv_objects <- plot_data_quadrat_rdiv$rdiv_objects[[1561]]
  
  # Retrieve objects
  meta_naive <- rdiv_objects[["meta_naive"]]
  meta_tax <- rdiv_objects[["meta_tax"]]
  meta_phylo <- rdiv_objects[["meta_phylo"]]
  meta_func <- rdiv_objects[["meta_func"]]
  
  # Naive diversity measures
  if(!is.null(meta_naive) & "naive" %in% metrics){
    
    if("alpha" %in% measures){
      alpha_naive_raw <- rdiversity::subdiv(rdiversity::raw_alpha(meta_naive), qs = q)
    } else{
      alpha_naive_raw <- NULL
    }
    
    if("beta" %in% measures){
      beta_naive_raw <- rdiversity::subdiv(rdiversity::raw_beta(meta_naive), qs = q)
    } else {
      beta_naive_raw <- NULL
    }
    
    if("gamma" %in% measures & dim(meta_naive@similarity)[2] > 1){
      gamma_naive <- rdiversity::sub_gamma(meta_naive, q)
    } else {
      gamma_naive <- NULL
    }
    
  } else {
    
    alpha_naive_raw <- NULL
    beta_naive_raw <- NULL
    gamma_naive <- NULL
    
  }
  
  # Taxonomic diversity measures
  if(!is.null(meta_tax) & "taxonomic" %in% metrics){
    
    if("alpha" %in% measures){
      alpha_tax_raw <- rdiversity::subdiv(rdiversity::raw_alpha(meta_tax), qs = q)
    } else{
      alpha_tax_raw <- NULL
    }
    
    if("beta" %in% measures){
      beta_tax_raw <- rdiversity::subdiv(rdiversity::raw_beta(meta_tax), qs = q)
    } else {
      beta_tax_raw <- NULL
    }
    
    if("gamma" %in% measures & dim(meta_tax@similarity)[2] > 1){
      gamma_tax <- rdiversity::sub_gamma(meta_tax, q)
    } else {
      gamma_tax <- NULL
    }
    
  } else {
    
    alpha_tax_raw <- NULL
    beta_tax_raw <- NULL
    gamma_tax <- NULL
    
  }
  
  # Phylogenetic diversity measures
  if(!is.null(meta_phylo) & "phylogenetic" %in% metrics){
    
    if("alpha" %in% measures){
      alpha_phylo_raw_dist <- rdiversity::subdiv(rdiversity::raw_alpha(meta_phylo), qs = q)
    } else{
      alpha_phylo_raw_dist <- NULL
    }
    
    if("beta" %in% measures){
      beta_phylo_raw_dist <- rdiversity::subdiv(rdiversity::raw_beta(meta_phylo), qs = q)
    } else {
      beta_phylo_raw_dist <- NULL
    }
    
    if("gamma" %in% measures & dim(meta_phylo@similarity)[2] > 1){
      gamma_phylo_dist <- rdiversity::sub_gamma(meta_phylo, q)
    } else {
      gamma_phylo_dist <- NULL
    }
    
  } else {
    
    alpha_phylo_raw_dist <- NULL
    beta_phylo_raw_dist <- NULL
    gamma_phylo_dist <- NULL
    
  }
  
  # Phylogenetic diversity measures
  if(!is.null(meta_func) & "functional" %in% metrics){
    
    if("alpha" %in% measures){
      alpha_func_raw_dist <- rdiversity::subdiv(rdiversity::raw_alpha(meta_func), qs = q)
    } else{
      alpha_func_raw_dist <- NULL
    }
    
    if("beta" %in% measures){
      beta_func_raw_dist <- rdiversity::subdiv(rdiversity::raw_beta(meta_func), qs = q)
    } else {
      beta_func_raw_dist <- NULL
    }
    
    if("gamma" %in% measures & dim(meta_func@similarity)[2] > 1){
      gamma_func_dist <- rdiversity::sub_gamma(meta_func, q)
    } else {
      gamma_func_dist <- NULL
    }
    
  } else {
    
    alpha_func_raw_dist <- NULL
    beta_func_raw_dist <- NULL
    gamma_func_dist <- NULL
    
  }
  
  # Collate results
  results <- dplyr::bind_rows(
    
    # Naive diversity
    alpha_naive_raw,
    beta_naive_raw,
    gamma_naive,
    
    # Taxonomic diversity
    alpha_tax_raw,
    beta_tax_raw,
    gamma_tax,
    
    # Phylogenetic diversity
    alpha_phylo_raw_dist,
    beta_phylo_raw_dist,
    gamma_phylo_dist,
    
    # Functional diversity
    alpha_func_raw_dist,
    beta_func_raw_dist,
    gamma_func_dist
    
  )
  
  return(results)
  
}

#' Calculate diversity metrics for a metacommunity
#' 
#' Calculate a set of metacommunity diversity measures and metrics using the objects
#' produced by the function `RMAVIS::calc_rdiversity_objects`.
#'
#' @param rdiv_objects A list of three objects: meta_naive, meta_tax, and meta_phylo, produced using the function `RMAVIS::calc_rdiversity_objects`.
#' @param measures One or more of "alpha", "beta", and "gamma".
#' @param metrics One or more of "naive", "taxonomic", "phylogenetic", and "functional".
#' @param q A positive number representing a Hill-Number.
#'
#' @returns A data frame containing the metacommunity partition diversity metrics for all combinations of specified measures and metrics
#' @export
#'
#' @examples
#' \dontrun{
#' rdiv_objs <- RMAVIS::calc_rdiversity_objects(plot_data = dplyr::filter(RMAVIS::example_data$`Parsonage Down`, Quadrat == "3N.1"), 
#'                                 higher_taxa = dplyr::distinct(UKVegTB::taxonomic_backbone, taxon_name, Kingdom, Phylum, Class, Order, Family, Genus), 
#'                                 phylo_tree = UKVegTB::phylo_tree, 
#'                                 phylo_taxa_lookup = UKVegTB::phylo_taxa_lookup) |>
#'                RMAVIS::calc_rdiversity_metrics_meta(measures = c("alpha", "beta", "gamma"), metrics = c("naive", "taxonomic", "phylogenetic"), q = 1)                                
#' }
calc_rdiversity_metrics_meta <- function(rdiv_objects, measures = c("alpha", "beta", "gamma"), metrics = c("naive", "taxonomic", "phylogenetic", "functional"), q = 0){

  
  # rdiv_objects <- plot_data_group_rdiv$rdiv_objects[[4057]]
    
  # Retrieve objects
  meta_naive <- rdiv_objects[["meta_naive"]]
  meta_tax <- rdiv_objects[["meta_tax"]]
  meta_phylo <- rdiv_objects[["meta_phylo"]]
  meta_func <- rdiv_objects[["meta_func"]]

  # Naive diversity measures
  if(!is.null(meta_naive) & "naive" %in% metrics){
    
    if("alpha" %in% measures){
      raw_meta_alpha_naive <- rdiversity::raw_meta_alpha(meta_naive, qs = q)
    } else {
      raw_meta_alpha_naive <- NULL
    }
    
    if("beta" %in% measures){
      raw_meta_beta_naive <- rdiversity::raw_meta_beta(meta_naive, qs = q)
    } else {
      raw_meta_beta_naive <- NULL
    }
    
    if("gamma" %in% measures & dim(meta_naive@similarity)[2] > 1){
      gamma_naive <- rdiversity::meta_gamma(meta_naive, qs = q)
    } else {
      gamma_naive <- NULL
    }
    
  } else {
    
    raw_meta_alpha_naive <- NULL
    raw_meta_beta_naive <- NULL
    gamma_naive <- NULL
    
  }

  # Taxonomic diversity measures
  if(!is.null(meta_tax) & "taxonomic" %in% metrics){
    
    if("alpha" %in% measures){
      raw_meta_alpha_tax <- rdiversity::raw_meta_alpha(meta_tax, qs = q)
    } else {
      raw_meta_alpha_tax <- NULL
    }
    
    if("beta" %in% measures){
      raw_meta_beta_tax <- rdiversity::raw_meta_beta(meta_tax, qs = q)
    } else {
      raw_meta_beta_tax <- NULL
    }
    
    if("gamma" %in% measures & dim(meta_tax@similarity)[2] > 1){
      gamma_tax <- rdiversity::meta_gamma(meta_tax, qs = q)
    } else {
      gamma_tax <- NULL
    }
    
  } else {
    
    raw_meta_alpha_tax <- NULL
    raw_meta_beta_tax <- NULL
    gamma_tax <- NULL
    
  }

  # Phylogenetic diversity measures
  if(!is.null(meta_phylo) & "phylogenetic" %in% metrics){
    
    if("alpha" %in% measures){
      raw_meta_alpha_phylo_dist <- rdiversity::raw_meta_alpha(meta_phylo, qs = q)
    } else{
      raw_meta_alpha_phylo_dist <- NULL
    }
    
    if("beta" %in% measures){
      raw_meta_beta_phylo_dist <- rdiversity::raw_meta_beta(meta_phylo, qs = q)
    } else {
      raw_meta_beta_phylo_dist <- NULL
    }
    
    if("gamma" %in% measures & dim(meta_phylo@similarity)[2] > 1){
      gamma_phylo_dist <- rdiversity::meta_gamma(meta_phylo, qs = q)
    } else {
      gamma_phylo_dist <- NULL
    }
    
  } else {
    
    raw_meta_alpha_phylo_dist <- NULL
    raw_meta_beta_phylo_dist <- NULL
    gamma_phylo_dist <- NULL
    
  }
  
  # Functional diversity measures
  if(!is.null(meta_func) & "functional" %in% metrics){
    
    if("alpha" %in% measures){
      raw_meta_alpha_func_dist <- rdiversity::raw_meta_alpha(meta_func, qs = q)
    } else{
      raw_meta_alpha_func_dist <- NULL
    }
    
    if("beta" %in% measures){
      raw_meta_beta_func_dist <- rdiversity::raw_meta_beta(meta_func, qs = q)
    } else {
      raw_meta_beta_func_dist <- NULL
    }
    
    if("gamma" %in% measures & dim(meta_func@similarity)[2] > 1){
      gamma_func_dist <- rdiversity::meta_gamma(meta_func, qs = q)
    } else {
      gamma_func_dist <- NULL
    }
    
  } else {
    
    raw_meta_alpha_func_dist <- NULL
    raw_meta_beta_func_dist <- NULL
    gamma_func_dist <- NULL
    
  }

  # Collate results
  results <- dplyr::bind_rows(

    # Naive diversity
    raw_meta_alpha_naive,
    raw_meta_beta_naive,
    gamma_naive,

    # Taxonomic diversity
    raw_meta_alpha_tax,
    raw_meta_beta_tax,
    gamma_tax,

    # Phylogenetic diversity
    raw_meta_alpha_phylo_dist,
    raw_meta_beta_phylo_dist,
    gamma_phylo_dist,
    
    # Functional diversity
    raw_meta_alpha_func_dist,
    raw_meta_beta_func_dist,
    gamma_func_dist

  )

  return(results)

}