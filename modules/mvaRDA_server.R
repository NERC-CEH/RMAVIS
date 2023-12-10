mvaRDA <- function(input, output, session, surveyTable, surveyTableWide, avgEIVs, sidebar_options) {
  
  ns <- session$ns
  
  # Retrieve sidebar options ------------------------------------------------
  runAnalysis <- reactiveVal()
  dcaVars <- reactiveVal()
  selectSurveyMethod <- reactiveVal()
  selectSurveyYears <- reactiveVal()
  selectSurveyQuadrats <- reactiveVal()
  selectSurveyGroups <- reactiveVal()
  
  observe({
    
    runAnalysis(sidebar_options()$runAnalysis)
    dcaVars(sidebar_options()$dcaVars)
    selectSurveyMethod(sidebar_options()$selectSurveyMethod)
    selectSurveyYears(sidebar_options()$selectSurveyYears)
    selectSurveyQuadrats(sidebar_options()$selectSurveyQuadrats)
    selectSurveyGroups(sidebar_options()$selectSurveyGroups)
    
  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)
  

# Intialise RDA Anova Table -----------------------------------------------
  
  rdaAnovaTable_init <- data.frame("Variable" = character(),
                                   "Df" = character(),
                                   "Variance" = double(),
                                   "F" = double(),
                                   "Pr(>F)" = double(),
                                   "Significance" = character()
                                   )
  
  rdaAnovaTable_rval <- reactiveVal(rdaAnovaTable_init)
  
  output$rdaAnovaTable <- rhandsontable::renderRHandsontable({
    
    rdaAnovaTable <- rhandsontable::rhandsontable(data = rdaAnovaTable_init,
                                                  rowHeaders = NULL,
                                                  width = "100%"#,
                                                  # overflow = "visible"
                                                  # stretchH = "all"
                                                  ) |>
      rhandsontable::hot_col(col = colnames(rdaAnovaTable_init), halign = "htCenter") |>
      rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
      htmlwidgets::onRender("
        function(el, x) {
          var hot = this.hot
          $('a[data-value=\"mva_panel\"').on('click', function(){
            setTimeout(function() {hot.render();}, 0);
          })
        }")
    
    return(rdaAnovaTable)
    
  })
  
  

# Initialise reactive objects ---------------------------------------------
  
  rda_scores_all_rval <- reactiveVal()
  rda_summary_stats_rval <- reactiveVal()
  
# Run RDA the Survey Data and Mean Hill-Ellenberg Plot Values -------------
  observe({
    
    req(surveyTable())
    req(surveyTableWide())
    req(avgEIVs())
    
    surveyTable <- surveyTable()
    surveyTableWide <- surveyTableWide()
    avgEIVs <- avgEIVs()
    

# Prepare data ------------------------------------------------------------

    surveyTable_concordance <- surveyTable |>
      dplyr::select(Year, Group, Quadrat) |>
      dplyr::distinct() |>
      tidyr::unite(col = "ID", c(Year, Group, Quadrat), sep = " - ", remove = FALSE)
      
    avgEIVs_prepped <- avgEIVs |>
      tibble::column_to_rownames(var = "ID") |>
      dplyr::rename("F" = "Moisture.F",
                    "L" = "Light.L",
                    "N" = "Nitrogen.N",
                    "R" = "Reaction.R",
                    "S" = "Salinity.S")
 

# Run RDA -----------------------------------------------------------------

    rda_results <- vegan::rda(surveyTableWide ~ ., data = avgEIVs_prepped)


# Summary Statistics ------------------------------------------------------

    # Check for collinearity
    rda_collinearity <- sqrt(vegan::vif.cca(rda_results)) |> 
      as.data.frame() |> 
      tibble::as_tibble(rownames = "Variable") |>
      dplyr::rename("Collinearity" = 2)
    
    # Total variance explained by the RDA
    rda_rsquareAdj <- vegan::RsquareAdj(rda_results)
    
    # Test the significance of the RCA using an ANOVA to perform a permutation test
    rda_anova_all <- vegan::anova.cca(rda_results, permutations = 999) |> 
      as.data.frame() |> 
      tibble::as_tibble(rownames = "Variable")
    
    # rda_anova_axis <- vegan::anova.cca(rda_results, permutations = 999, by = "axis")
    
    rda_anova_terms <- vegan::anova.cca(rda_results, permutations = 999, by = "terms") |> 
      as.data.frame() |> 
      tibble::as_tibble(rownames = "Variable") |>
      dplyr::mutate(
        "Significance" = 
          dplyr::case_when(
            `Pr(>F)` > 0.1 ~ "",
            `Pr(>F)` > 0.05 ~ ".",
            `Pr(>F)` > 0.01 ~ "*",
            `Pr(>F)` > 0.001 ~ "**",
            `Pr(>F)` <= 0.001 ~ "***",
            TRUE ~ ""
          )
      )
    
    # Update RDA ANOVA table
    output$rdaAnovaTable <- rhandsontable::renderRHandsontable({
      
      rdaAnovaTable <- rhandsontable::rhandsontable(data = rda_anova_terms,
                                                    rowHeaders = NULL,
                                                    width = "100%"#,
                                                    # overflow = "visible",
                                                    # stretchH = "all"
                                                    ) |>
        rhandsontable::hot_col(col = colnames(rda_anova_terms), halign = "htCenter") |>
        rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) |>
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
        htmlwidgets::onRender("
        function(el, x) {
          var hot = this.hot
          $('a[data-value=\"mva_panel\"').on('click', function(){
            setTimeout(function() {hot.render();}, 0);
          })
        }")
      
      return(rdaAnovaTable)
      
    })
    
    # Compile summary statistics into list
    rda_summary_stats_list = list("rda_collinearity" = rda_collinearity,
                                  "rda_rsquareAdj" = rda_rsquareAdj,
                                  "rda_anova_all" = rda_anova_all,
                                  # "rda_anova_axis" = rda_anova_axis,
                                  "rda_anova_terms" = rda_anova_terms)
    
    rda_summary_stats_rval(rda_summary_stats_list)
    

# Wrangle scores for plotting ---------------------------------------------
    
    rda_scores <- vegan::scores(rda_results, tidy = TRUE)
    
    rda_scores_sites <- rda_scores |>
      dplyr::filter(score == "sites") |>
      dplyr::rename("ID" = label) |>
      dplyr::select(-score) |>
      dplyr::left_join(surveyTable_concordance, by = "ID") |>
      dplyr::select(-ID)
    
    rda_scores_species <- rda_scores |>
      dplyr::filter(score == "species") |>
      dplyr::select(-score) |>
      dplyr::rename("Species" = label)
    
    rda_scores_regression <- rda_scores |>
      dplyr::filter(score == "regression") |>
      dplyr::select(-score) |>
      dplyr::rename("Variable" = label)
    
    rda_scores_biplot <- rda_scores |>
      dplyr::filter(score =="biplot") |>
      dplyr::select(-score) |>
      dplyr::rename("Variable" = label)
    
    rda_scores_all <- list(
      "rda_scores_quadrats" = rda_scores_sites,
      "rda_scores_species" = rda_scores_species,
      "rda_scores_regression" = rda_scores_regression,
      "rda_scores_biplot" = rda_scores_biplot
    )
    
    rda_scores_all_rval(rda_scores_all)
    
  }) |>
    bindEvent(runAnalysis(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  

# Create RDA Plot ---------------------------------------------------------
  
  observe({
    
    req(rda_scores_all_rval())
    req(dcaVars())
    
    rda_scores_all <- rda_scores_all_rval()
    
    if(selectSurveyMethod() == "all"){
      
      rda_scores_all_quadrats_selected <- rda_scores_all$rda_scores_quadrats
      
      # arrow_plot_data_selected <- mvaNationalRefResults$arrow_plot_data
      
    } else if(selectSurveyMethod() == "selectYears"){
      
      rda_scores_all_quadrats_selected <- rda_scores_all$rda_scores_quadrats |>
        dplyr::filter(Year %in% selectSurveyYears())
      
      # arrow_plot_data_selected <- mvaNationalRefResults$arrow_plot_data |>
      #   dplyr::filter(Year %in% selectSurveyYears())
      
    } else if(selectSurveyMethod() == "selectGroups"){
      
      rda_scores_all_quadrats_selected <- rda_scores_all$rda_scores_quadrats |>
        dplyr::filter(Group %in% selectSurveyGroups())
      
      # arrow_plot_data_selected <- mvaNationalRefResults$arrow_plot_data |>
      #   dplyr::filter(Group %in% selectSurveyGroups())
      
    } else if(selectSurveyMethod() == "selectQuadrats"){
      
      rda_scores_all_quadrats_selected <- rda_scores_all$rda_scores_quadrats |>
        dplyr::filter(Quadrat %in% selectSurveyQuadrats())
      
      # arrow_plot_data_selected <- mvaNationalRefResults$arrow_plot_data |>
      #   dplyr::filter(Quadrat %in% selectSurveyQuadrats())
      
    }
    
    output$mvaRDAPlot <- plotly::renderPlotly({
      
      suppressWarnings(
        
        # Create ggplot2 plot
        mvaRDAPlot_plot <- ggplot2::ggplot() +
          {if("species" %in% dcaVars())ggplot2::geom_point(data = rda_scores_all$rda_scores_species,
                                                           color = '#32a87d',
                                                           shape = 18,
                                                           mapping = ggplot2::aes(x = RDA1, 
                                                                                  y = RDA2,
                                                                                  Species = Species))} +
          {if("surveyQuadrats" %in% dcaVars())ggplot2::geom_point(data = rda_scores_all_quadrats_selected,
                                                                  color = 'black',
                                                                  mapping = ggplot2::aes(Year = Year,
                                                                                         Group = Group,
                                                                                         Quadrat = Quadrat,
                                                                                         x = RDA1,
                                                                                         y = RDA2))} +
          {if("hillEllenberg" %in% dcaVars())ggplot2::geom_segment(data = rda_scores_all$rda_scores_biplot,
                                                                   color = 'black',
                                                                   arrow = grid::arrow(),
                                                                   mapping = ggplot2::aes(x = 0,
                                                                                          y = 0,
                                                                                          xend = RDA1,
                                                                                          yend = RDA2,
                                                                                          label = Variable))} +
          {if("hillEllenberg" %in% dcaVars())ggplot2::geom_text(data = rda_scores_all$rda_scores_biplot,
                                                                color = 'black',
                                                                # position = ggplot2::position_dodge(width = 0.9),
                                                                size = 5,
                                                                mapping = ggplot2::aes(x = RDA1 * 1.075,
                                                                                       y = RDA2 * 1.075,
                                                                                       label = Variable))} +
          ggplot2::theme_minimal()
        
      )
      
      mvaRDAPlot_plotly <- plotly::ggplotly(p = mvaRDAPlot_plot)
      
      return(mvaRDAPlot_plotly)
      
    })

    
  }) |>
      bindEvent(dcaVars(),
                selectSurveyMethod(),
                selectSurveyYears(),
                selectSurveyQuadrats(),
                selectSurveyGroups(),
                rda_scores_all_rval(),
                ignoreInit = TRUE,
                ignoreNULL = TRUE)
  
  
  outputOptions(output, "rdaAnovaTable", suspendWhenHidden = FALSE)
  
  # Return
  return()
  
}
