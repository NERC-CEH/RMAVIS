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
    ggplot2::ylab(label = NULL) +
    ggplot2::xlab(label = NULL) +
    NULL
  
  return(ale_plot)
}

plot_break_down <- function(x,
                            baseline = NA,
                            vcolors = DALEX::colors_breakdown_drwhy(),
                            digits = 3, 
                            rounding_function = round,
                            add_contributions = TRUE, 
                            shift_contributions = 0.05,
                            vnames = NULL){
  
  position <- cumulative <- prev <- pretty_text <- right_side <- contribution <- NULL
  
  # fix for https://github.com/ModelOriented/iBreakDown/issues/77
  colnames(x) <- gsub(colnames(x), pattern = "cummulative", replacement = "cumulative")
  
  # enrich dataframe with additional features
  tmp <- iBreakDown:::prepare_data_for_break_down_plot(x, baseline, rounding_function, digits)
  
  broken_baseline <- tmp$broken_baseline
  x <- tmp$x
  
  # fix for https://github.com/ModelOriented/iBreakDown/issues/85 check if correction is needed
  if (any(x[x$variable == "prediction", "right_side"] < broken_baseline$contribution)) {
    # put there max val
    x[x$variable == "prediction", "right_side"] <- pmax(x[x$variable == "prediction", "right_side"], broken_baseline$contribution)
  }
  if (any(x[x$variable == "intercept", "right_side"] < broken_baseline$contribution)) {
    # put there max val
    x[x$variable == "intercept", "right_side"] <- pmax(x[x$variable == "intercept", "right_side"], broken_baseline$contribution)
  }
  
  drange <- diff(range(x$cumulative))
  
  # base plot
  pl <- ggplot2::ggplot(x, mapping = ggplot2::aes(x = position + 0.5,
                                                  y = pmax(cumulative, prev),
                                                  xmin = position + 0.15, 
                                                  xmax = position + 0.85,
                                                  ymin = cumulative, 
                                                  ymax = prev,
                                                  fill = sign,
                                                  label = pretty_text)) +
    ggplot2::geom_errorbarh(data = x[x$variable_name != "", ],
                            mapping = ggplot2::aes(xmax = position - 0.85,
                                                   xmin = position + 0.85,
                                                   y = cumulative), 
                            height = 0,
                            color = "#371ea3") +
    ggplot2::geom_rect(alpha = 0.9) +
    ggplot2::geom_hline(data = broken_baseline, 
                        mapping = ggplot2::aes(yintercept = contribution), 
                        lty = 3, 
                        alpha = 0.5, 
                        color = "#371ea3") +
    ggplot2::geom_text(mapping = ggplot2::aes(y = right_side),
                       vjust = 0.5,
                       nudge_y = drange * shift_contributions,
                       hjust = -0.25,
                       color = "#371ea3") + 
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    ggplot2::scale_x_continuous(labels = x$variable, breaks = x$position + 0.5, name = "") +
    ggplot2::scale_fill_manual(values = vcolors) + 
    ggplot2::coord_flip() + 
    DALEX::theme_vertical_default_dalex() +
    ggplot2::theme(legend.position = "none")
  
  return(pl)
  
}