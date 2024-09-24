create_feature_importance_plot <- function(fe_data, bar_width = 5) {
  
  expl_df <- fe_data |>
    dplyr::select(-species) |>
    dplyr::group_by(model, variable) |>
    dplyr::mutate("min" = min(dropout_loss, na.rm = TRUE),
                  "q1" = quantile(dropout_loss, probs = 0.25, na.rm = TRUE),
                  "median" = median(dropout_loss, na.rm = TRUE),
                  "q3" = quantile(dropout_loss, probs = 0.75, na.rm = TRUE),
                  "max" = max(dropout_loss, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(permutation == 0)
  
  # Add an additional column that serve as a baseline
  bestFits <- expl_df |>
    dplyr::filter(variable == "_full_model_") |>
    dplyr::select(model, permutation, dropout_loss)
  
  ext_expl_df <- expl_df |>
    dplyr::full_join(bestFits, by = c("model", "permutation"))
  
  # Remove rows that starts with _ i.e. _full_model_ and _baseline_
  ext_expl_df <- ext_expl_df |>
    dplyr::filter(!(variable %in% c("_full_model_", "_baseline_")))
  
  # Order rows
  ext_expl_df <- ext_expl_df |>
    dplyr::group_by(model) |>
    dplyr::arrange(dplyr::desc(dropout_loss.x), .by_group = TRUE) |>
    dplyr::ungroup()
  
  # facets have fixed space, can be resolved with ggforce https://github.com/tidyverse/ggplot2/issues/2933
  pl <- ggplot2::ggplot(data = ext_expl_df) +
    ggplot2::geom_hline(data = bestFits, 
                        mapping = ggplot2::aes(yintercept = dropout_loss), 
                        lty= 3) +
    ggplot2::geom_linerange(mapping = ggplot2::aes(reorder(variable, dropout_loss.x), ymin = dropout_loss.y, ymax = dropout_loss.x), 
                            colour = "#66CFD3",
                            size = 4) +
    ggplot2::geom_boxplot(data = ext_expl_df,
                          mapping = ggplot2::aes(x = variable, ymin = min, lower = q1, middle = median, upper = q3, ymax = max),
                          stat = "identity", fill = "#371ea3", color = "#371ea3", width = 0.25) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~model, ncol = 3, scales = "free_y") + 
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ylab(label = NULL) +
    ggplot2::xlab(label = "Variable") +
    NULL
  
  return(pl)
  
}

create_ale_plot <- function(ale_data){
  
  ale_plot <- ggplot2::ggplot(data = ale_data) +
    ggplot2::geom_line(mapping = ggplot2::aes(x = x, y = y, color = model)) +
    ggplot2::facet_wrap(~variable, scales = "free_x", ncol = 2) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "right") +
    NULL
  
  return(ale_plot)
}