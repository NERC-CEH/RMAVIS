mvaNationalRef <- function(input, output, session, setupData, surveyData, vcAssignment, avgEIVs, sidebar_options) {
  
  ns <- session$ns
  
# Retrieve sidebar options ------------------------------------------------
  runAnalysis <- reactiveVal()
  dcaAxisSelection <- reactiveVal()
  dcaVars <- reactiveVal()
  # ccaVars <- reactiveVal()
  selectedReferenceSpaces <- reactiveVal()
  groupSurveyPlots <- reactiveVal()
  selectSurveyMethod <- reactiveVal()
  selectSurveyYears <- reactiveVal()
  selectSurveyQuadrats <- reactiveVal()
  selectSurveyGroups <- reactiveVal()
  aggTaxaOpts <- reactiveVal()
  
  observe({
    
    runAnalysis(sidebar_options()$runAnalysis)
    dcaAxisSelection(sidebar_options()$dcaAxisSelection)
    dcaVars(sidebar_options()$dcaVars)
    # ccaVars(sidebar_options()$ccaVars)
    groupSurveyPlots(sidebar_options()$groupSurveyPlots)
    selectedReferenceSpaces(sidebar_options()$selectedReferenceSpaces)
    selectSurveyMethod(sidebar_options()$selectSurveyMethod)
    selectSurveyYears(sidebar_options()$selectSurveyYears)
    selectSurveyQuadrats(sidebar_options()$selectSurveyQuadrats)
    selectSurveyGroups(sidebar_options()$selectSurveyGroups)
    aggTaxaOpts(sidebar_options()$aggTaxaOpts)
    
  }) |>
    bindEvent(sidebar_options(), 
              ignoreInit = TRUE)
  
# Retrieve Setup Data -----------------------------------------------------

## Retrieve options -------------------------------------------------------
  regional_availability <- reactiveVal()
  use_eivs <- reactiveVal()
  
  observe({
    
    regional_availability(setupData()$regional_availability)
    use_eivs(setupData()$regional_availability$avgEIVs)
    
  }) |>
    shiny::bindEvent(setupData(),
                     ignoreInit = FALSE,
                     ignoreNULL = TRUE)
   
## Retrieve psquads -------------------------------------------------------
  vc_pquads_wide <- reactiveVal()
  
  observe({
    
    shiny::isolate({
      pquads <- setupData()$pquads
      psq_taxon_name_col <- setupData()$psq_taxon_name_col
    })
    
    vc_pquads_wide_prepped <- pquads |>
      dplyr::select(psq_id, psq_taxon_name_col) |>
      dplyr::mutate("present" = 1) |>
      dplyr::distinct(.data[["psq_id"]], .data[[psq_taxon_name_col]], .keep_all = TRUE) |>
      tidyr::pivot_wider(id_cols = psq_id,
                         names_from = psq_taxon_name_col,
                         values_fill = 0,
                         values_from = present) |>
      tibble::column_to_rownames(var = "psq_id") |>
      as.matrix()
    
    
    vc_pquads_wide(vc_pquads_wide_prepped)
    
  }) |>
    bindEvent(runAnalysis(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
  
## Retrieve psquad cm eivs  ----------------------------------------------
  vc_pquads_mean_unweighted_eivs <- reactiveVal()
  
  observe({
    
    shiny::req(isTRUE(use_eivs()))
    
    shiny::isolate({
      psquad_cm_he <- setupData()$psquad_cm_he
    })
    
    psquad_cm_he_prepped <- psquad_cm_he |>
      dplyr::select(psq_id, `F`, L, N, R, S)

    vc_pquads_mean_unweighted_eivs(psquad_cm_he_prepped)
    
  }) |>
    bindEvent(runAnalysis(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
  
  
# Run DCA and CCA ---------------------------------------------------------
  mvaResults_rval <- reactiveVal()
  
  observe({
    
    # Require selected objects are not NULL
    shiny::req(surveyData())
    shiny::req(runAnalysis() != 0)
    shiny::req(vcAssignment())
    shiny::req(vc_pquads_wide())
    shiny::req(!is.null(selectedReferenceSpaces()))
    
    # Start busy spinner
    shinybusy::show_modal_spinner(
      spin = "fading-circle",
      color = "#3F9280",
      text = "Performing National Reference MVA"
    )
    
    # Isolate processes to prevent recursion when handling reactive objects not included in bindEvent
    shiny::isolate({
      
      vcAssignment <- vcAssignment()
      selectedReferenceSpaces <- selectedReferenceSpaces()
      vc_pquads_wide <- vc_pquads_wide()
      vc_pquads_mean_unweighted_eivs <- vc_pquads_mean_unweighted_eivs()
      avgEIVs <- avgEIVs()
      # ccaVars <- ccaVars()
      topvcCommunities <- vcAssignment$topvcCommunities
      
      if(isTRUE(regional_availability()$aggTaxa) & "mva" %in% aggTaxaOpts()){
        
        surveyData_long <- surveyData()$surveyData_long_agg
        
      } else {
        
        surveyData_long <- surveyData()$surveyData_long
        
      }
      
    })
    
    # Perform a DCA on the combined pseudo-quadrat and survey data
    pquads_dca_results <- vegan::decorana(veg = vc_pquads_wide)
    
    # Extract the DCA results species axis scores
    pquads_dca_results_species <- vegan::scores(pquads_dca_results, tidy = TRUE) |>
      dplyr::filter(score == "species") |>
      dplyr::select(-score, -weight) |>
      dplyr::rename("Species" = label)
    
    # Extract the DCA results sample axis scores
    pquads_dca_results_quadrats <- vegan::scores(pquads_dca_results, tidy = TRUE) |>
      dplyr::filter(score == "sites") |>
      dplyr::select(-score, -weight) |>
      dplyr::rename("Quadrat" = label) |>
      dplyr::mutate("VC.Code" = stringr::str_extract(string = Quadrat, pattern = ".+?(?=\\_)"), .before  = "Quadrat")
    
    # Create convex hulls around the pseudo-quadrat DCA points.
    pquad_hulls_dca1dca2 <- pquads_dca_results_quadrats |>
      dplyr::group_by(VC.Code) |>
      dplyr::slice(grDevices::chull(DCA1, DCA2)) |>
      dplyr::ungroup() |>
      dplyr::mutate("dcaAxes" = "dca1dca2")
    
    pquad_hulls_dca1dca3 <- pquads_dca_results_quadrats |>
      dplyr::group_by(VC.Code) |>
      dplyr::slice(grDevices::chull(DCA1, DCA3)) |>
      dplyr::ungroup() |>
      dplyr::mutate("dcaAxes" = "dca1dca3")
    
    pquad_hulls_dca2dca3 <- pquads_dca_results_quadrats |>
      dplyr::group_by(VC.Code) |>
      dplyr::slice(grDevices::chull(DCA2, DCA3)) |>
      dplyr::ungroup() |>
      dplyr::mutate("dcaAxes" = "dca2dca3")
    
    pquad_hulls <- rbind(pquad_hulls_dca1dca2,
                         pquad_hulls_dca1dca3,
                         pquad_hulls_dca2dca3)
    
    # Calculate the surveyData DCA results using the pseudo-quadrat species scores
    surveyData_dca_results_site <- surveyData_long |>
      tibble::as_tibble() |>
      dplyr::select(-Cover) |>
      dplyr::left_join(pquads_dca_results_species, by = "Species") |>
      tidyr::unite(col = "ID", c(Year, Group, Quadrat), sep = " - ", remove = FALSE) |>
      dplyr::group_by(ID, Year, Group, Quadrat) |>
      dplyr::summarise("DCA1" = mean(DCA1, na.rm = TRUE),
                       "DCA2" = mean(DCA2, na.rm = TRUE),
                       "DCA3" = mean(DCA3, na.rm = TRUE),
                       "DCA4" = mean(DCA4, na.rm = TRUE),
                       .groups = "drop") |>
      dplyr::mutate("VC.Code" = "Sample", .before  = "Quadrat")

    # Prepare the data required to draw arrows between points, ordered by Year
    if(length(unique(surveyData_dca_results_site$Year)) > 1){
      
      arrow_plot_data <- surveyData_dca_results_site |>
        dplyr::arrange(Year) |>
        dplyr::select("Year" = Year, 
                      "Group" = Group,
                      "Quadrat" = Quadrat, 
                      "x" = DCA1, 
                      "y" = DCA2) |>
        dplyr::group_by(Quadrat) |>
        dplyr::mutate("endX" = dplyr::lead(x),
                      "endY" = dplyr::lead(y)) |>
        dplyr::filter(!is.na(endX)) |>
        dplyr::ungroup()
      
    } else {
      
      arrow_plot_data <- NULL
      
    }
    
    # Calculate pseudo-quadrat centroids
    pquad_centroids <- pquads_dca_results_quadrats |>
      dplyr::group_by(VC.Code) |>
      dplyr::summarise("DCA1" = mean(DCA1),
                       "DCA2" = mean(DCA2),
                       "DCA3" = mean(DCA3),
                       "DCA4" = mean(DCA4)) |>
      dplyr::ungroup()
    
    if(!is.null(vc_pquads_mean_unweighted_eivs)){
      
      # Retrieve the unweighted mean Hill-Ellenberg scores for the pseudo-quadrats
      vc_pquads_mean_unweighted_eivs_prepped <- vc_pquads_mean_unweighted_eivs |>
        tibble::column_to_rownames(var = "psq_id")
      
      # Perform a CCA on the selected pseudo-quadrats using selected Hill-Ellenberg scores
      selected_pquads_prepped_cca  <- vegan::cca(as.formula(paste0("vc_pquads_mean_unweighted_eivs_prepped ~ ", paste0(RMAVIS:::he_options, collapse = " + "))), # selected_pquads_prepped ~ `F` + `L` + `N`
                                                 data = vc_pquads_mean_unweighted_eivs_prepped,
                                                 na.action = na.exclude)
      
      # Extract CCA scores
      selected_pquads_prepped_cca_scores <- vegan::scores(selected_pquads_prepped_cca, 
                                                          choices = c(1, 2, 3),
                                                          display = "bp")
      
      # Extract CCA multiplier, not currently used.
      # selected_pquads_prepped_cca_multiplier <- vegan:::ordiArrowMul(selected_pquads_prepped_cca_scores)
      
      # Create CCA arrow data
      CCA_arrowData <- selected_pquads_prepped_cca_scores |>
        tibble::as_tibble(rownames = NA) |>
        tibble::rownames_to_column(var = "Hill-Ellenberg")
      
    } else {
      
      CCA_arrowData <- NULL
      
    }
    
    # Compose list of DCA results objects
    mvaResults_list <- list("dca_results_pquads_species" = pquads_dca_results_species,
                            "dca_results_pquads_site" = pquads_dca_results_quadrats,
                            "dca_results_sample_site" = surveyData_dca_results_site,
                            "pquad_hulls" = pquad_hulls,
                            "pquad_centroids" = pquad_centroids,
                            # "sample_centroids" = sample_centroids,
                            "arrow_plot_data" = arrow_plot_data,
                            "CCA_arrowData" = CCA_arrowData
                            )
    
    mvaResults_rval(mvaResults_list)
    
    shinybusy::remove_modal_spinner()
      
      
  }) |>
    bindEvent(selectedReferenceSpaces(), # Changes every time the analysis is re-run
              # ccaVars(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)

# Subset data and create plot ---------------------------------------------
    observe({
      
      shiny::req(mvaResults_rval())
      mvaResults <- mvaResults_rval()
      dcaAxisSelection <- dcaAxisSelection()
      groupSurveyPlots <- groupSurveyPlots()
      selectSurveyMethod <- selectSurveyMethod()
      selectSurveyYears <- selectSurveyYears()
      selectSurveyGroups <- selectSurveyGroups()
      selectSurveyQuadrats <- selectSurveyQuadrats()
      
      
      if(groupSurveyPlots == "no"){
        
        dca_results_sample_site_selected <- mvaResults$dca_results_sample_site
      
        
      } else if(groupSurveyPlots == "group") {
        
        dca_results_sample_site_selected <- mvaResults$dca_results_sample_site |>
          dplyr::group_by(Year, Group) |>
          dplyr::summarise("DCA1" = mean(DCA1),
                           "DCA2" = mean(DCA2),
                           "DCA3" = mean(DCA3)) |>
          dplyr::ungroup() |>
          dplyr::arrange(Group, Year)
      
        
      } else if(groupSurveyPlots == "year") {
        
        dca_results_sample_site_selected <- mvaResults$dca_results_sample_site |>
          dplyr::group_by(Year) |>
          dplyr::summarise("DCA1" = mean(DCA1),
                           "DCA2" = mean(DCA2),
                           "DCA3" = mean(DCA3)) |>
          dplyr::ungroup() |>
          dplyr::arrange(Year)
        
      }
      
      # Select the survey plots and path arrows to include
      if(groupSurveyPlots() == "no" && selectSurveyMethod == "all"){
        
        dca_results_sample_site_selected <- mvaResults$dca_results_sample_site
        
      } else if(groupSurveyPlots() == "no" && selectSurveyMethod == "selectYears"){
        
        dca_results_sample_site_selected <- mvaResults$dca_results_sample_site |>
          dplyr::filter(Year %in% selectSurveyYears)
        
      } else if(groupSurveyPlots() == "no" && selectSurveyMethod == "selectGroups"){
        
        dca_results_sample_site_selected <- mvaResults$dca_results_sample_site |>
          dplyr::filter(Group %in% selectSurveyGroups)
        
      } else if(groupSurveyPlots() == "no" && selectSurveyMethod == "selectQuadrats"){
        
        dca_results_sample_site_selected <- mvaResults$dca_results_sample_site |>
          dplyr::filter(Quadrat %in% selectSurveyQuadrats)
        
      }
      
      # Select centroids
      pquad_centroids_nrs <- mvaResults$pquad_centroids |>
        dplyr::filter(VC.Code %in% selectedReferenceSpaces())
      
      # Select hulls
      pquad_hulls_nrs <- mvaResults$pquad_hulls |>
        dplyr::filter(VC.Code %in% selectedReferenceSpaces())
      

      # Retrieve hulls and centroids for selected DCA axes
      if(dcaAxisSelection == "dca1dca2"){
        
        x_axis <- "DCA1"
        y_axis <- "DCA2"
        x_axis_cca <- "CCA1"
        y_axis_cca <- "CCA2"
        
        pquad_hulls_selected <- pquad_hulls_nrs |>
          dplyr::filter(dcaAxes == "dca1dca2") |>
          dplyr::select(-dcaAxes)
        
        pquad_centroids_selected <- pquad_centroids_nrs |>
          dplyr::select(VC.Code, DCA1, DCA2)
        
      } else if(dcaAxisSelection == "dca1dca3"){
        
        x_axis <- "DCA1"
        y_axis <- "DCA3"
        x_axis_cca <- "CCA1"
        y_axis_cca <- "CCA3"
        
        pquad_hulls_selected <- pquad_hulls_nrs |>
          dplyr::filter(dcaAxes == "dca1dca3") |>
          dplyr::select(-dcaAxes)
        
        pquad_centroids_selected <- pquad_centroids_nrs |>
          dplyr::select(VC.Code, DCA1, DCA3)
        
      } else if(dcaAxisSelection == "dca2dca3"){
        
        x_axis <- "DCA2"
        y_axis <- "DCA3"
        x_axis_cca <- "CCA2"
        y_axis_cca <- "CCA3"
        
        pquad_hulls_selected <- pquad_hulls_nrs |>
          dplyr::filter(dcaAxes == "dca2dca3") |>
          dplyr::select(-dcaAxes)
        
        pquad_centroids_selected <- pquad_centroids_nrs |>
          dplyr::select(VC.Code, DCA2, DCA3)
        
      }

      # Prepare arrow data
      if(length(unique(dca_results_sample_site_selected$Year)) > 1){
        
        arrow_plot_data <- dca_results_sample_site_selected |>
          # dplyr::group_by(dplyr::across(c(-Year, -DCA1, -DCA2, -DCA3, -DCA4))) |>
          dplyr::group_by(dplyr::across(-dplyr::any_of(c("Year", "DCA1", "DCA2", "DCA3", "DCA4")))) |>
          dplyr::mutate("x" = get(x_axis), "y" = get(y_axis)) |>
          dplyr::mutate("endX" = dplyr::lead(x), "endY" = dplyr::lead(y)) |>
          dplyr::filter(!is.na(endX)) |>
          dplyr::ungroup()
        
      } else {
        
        arrow_plot_data <- NULL
        
      }
      
      # Create an interactive plot of the DCA results
      output$mvaNationalRefPlot <- plotly::renderPlotly({
        
        suppressWarnings(
          
          # Create ggplot2 plot
          mvaNationalRefPlot_plot <- ggplot2::ggplot() +
            {if("referenceSpace" %in% dcaVars())ggplot2::geom_polygon(data = pquad_hulls_selected, alpha = 0.2, 
                                                                      mapping = ggplot2::aes(x = .data[[x_axis]], 
                                                                                             y = .data[[y_axis]],
                                                                                             fill = VC.Code))} +
            {if("referenceCentroids" %in% dcaVars())ggplot2::geom_point(data = pquad_centroids_selected,
                                                                        mapping = ggplot2::aes(x = .data[[x_axis]], 
                                                                                               y = .data[[y_axis]],
                                                                                               fill = VC.Code,
                                                                                               color = VC.Code),
                                                                                               size = 3)} +
            {if("species" %in% dcaVars())ggplot2::geom_point(data = mvaResults$dca_results_pquads_species,
                                                             color = '#32a87d',
                                                             shape = 18,
                                                             mapping = ggplot2::aes(x = .data[[x_axis]], 
                                                                                    y = .data[[y_axis]],
                                                                                    Species = Species))} +
            {if("surveyQuadrats" %in% dcaVars() && groupSurveyPlots() == "year")
              ggplot2::geom_point(data = dca_results_sample_site_selected,
                                  color = 'black',
                                  mapping = ggplot2::aes(Year = Year,
                                                         x = .data[[x_axis]],
                                                         y = .data[[y_axis]]))} +
            {if("surveyQuadrats" %in% dcaVars() && groupSurveyPlots() == "group")
              ggplot2::geom_point(data = dca_results_sample_site_selected,
                                  color = 'black',
                                  mapping = ggplot2::aes(Year = Year,
                                                         Group = Group,
                                                         x = .data[[x_axis]],
                                                         y = .data[[y_axis]]))} +
            {if("surveyQuadrats" %in% dcaVars() && groupSurveyPlots() == "no")
              ggplot2::geom_point(data = dca_results_sample_site_selected,
                                  color = 'black',
                                  mapping = ggplot2::aes(Year = Year,
                                                         Group = Group,
                                                         Quadrat = Quadrat,
                                                         x = .data[[x_axis]],
                                                         y = .data[[y_axis]]))} +
            {if("hillEllenberg" %in% dcaVars() & !is.null(mvaResults$CCA_arrowData)) ggplot2::geom_segment(data = mvaResults$CCA_arrowData,
                                                                                                           color = 'black',
                                                                                                           arrow = grid::arrow(),
                                                                                                           mapping = ggplot2::aes(x = 0,
                                                                                                                                  y = 0,
                                                                                                                                  xend = .data[[x_axis_cca]],
                                                                                                                                  yend = .data[[y_axis_cca]],
                                                                                                                                  label = `Hill-Ellenberg`))} +
            {if("hillEllenberg" %in% dcaVars() & !is.null(mvaResults$CCA_arrowData))ggplot2::geom_text(data = mvaResults$CCA_arrowData,
                                                                                                       color = 'black',
                                                                                                       # position = ggplot2::position_dodge(width = 0.9),
                                                                                                       size = 5,
                                                                                                       mapping = ggplot2::aes(x = .data[[x_axis_cca]] * 1.075,
                                                                                                                              y = .data[[y_axis_cca]] * 1.075,
                                                                                                                              label = `Hill-Ellenberg`))} +
            ggplot2::theme_minimal()
          
        )
        
        if("trajectory" %in% dcaVars() & !is.null(arrow_plot_data)){
          
          if(nrow(arrow_plot_data) > 0){
            
            mvaNationalRefPlot_plotly <- plotly::ggplotly(p = mvaNationalRefPlot_plot) |>
              plotly::add_annotations(data = arrow_plot_data,
                                      showarrow = TRUE,
                                      text = "",
                                      xref = "x", axref = "x",
                                      yref = "y", ayref = "y",
                                      x = ~endX,
                                      ax = ~x,
                                      y = ~endY,
                                      ay = ~y) |>
              plotly::hide_legend()
            
          }
          
        } else {
          
          mvaNationalRefPlot_plotly <- plotly::ggplotly(p = mvaNationalRefPlot_plot)
          
        }
        
        mvaNationalRefPlot_plotly <- plotly::hide_legend(mvaNationalRefPlot_plotly)
        
        return(mvaNationalRefPlot_plotly)
      
      })
      
    }) |>
    bindEvent(mvaResults_rval(),
              dcaAxisSelection(),
              dcaVars(),
              groupSurveyPlots(),
              selectedReferenceSpaces(),
              selectSurveyMethod(),
              selectSurveyYears(),
              selectSurveyGroups(),
              selectSurveyQuadrats(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
    
  
  # Return list of DCA results objects
  return(mvaResults_rval)
  
}
