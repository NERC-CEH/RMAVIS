mvaLocalRefRestricted <- function(input, output, session, setupData, surveyData, nvcAssignment, sidebar_options) {
  
  ns <- session$ns

# Retrieve sidebar options ------------------------------------------------
  runAnalysis <- reactiveVal()
  dcaAxisSelection <- reactiveVal()
  dcaVars <- reactiveVal()
  ccaVars <- reactiveVal()
  groupSurveyPlots <- reactiveVal()
  selectedReferenceSpaces <- reactiveVal()
  selectSurveyMethod <- reactiveVal()
  selectSurveyYears <- reactiveVal()
  selectSurveyQuadrats <- reactiveVal()
  selectSurveyGroups <- reactiveVal()
  
  observe({
    
    runAnalysis(sidebar_options()$runAnalysis)
    dcaAxisSelection(sidebar_options()$dcaAxisSelection)
    dcaVars(sidebar_options()$dcaVars)
    ccaVars(sidebar_options()$ccaVars)
    groupSurveyPlots(sidebar_options()$groupSurveyPlots)
    selectedReferenceSpaces(sidebar_options()$selectedReferenceSpaces)
    selectSurveyMethod(sidebar_options()$selectSurveyMethod)
    selectSurveyYears(sidebar_options()$selectSurveyYears)
    selectSurveyQuadrats(sidebar_options()$selectSurveyQuadrats)
    selectSurveyGroups(sidebar_options()$selectSurveyGroups)
    
  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)

  # Retrieve Setup Data -----------------------------------------------------
  nvc_pquads_wide <- reactiveVal()
  nvc_pquads_mean_unweighted_eivs <- reactiveVal()
  
  observe({
    
    shiny::isolate({
      setupData <- setupData()
    })
    
    # assign(x = "setupData", value = setupData, envir = .GlobalEnv)
    
    nvc_pquads_wide_prepped <- setupData$pquads |>
      dplyr::select(psq_id, nvc_taxon_name) |>
      dplyr::mutate("present" = 1) |>
      dplyr::distinct(psq_id, nvc_taxon_name, .keep_all = TRUE) |>
      tidyr::pivot_wider(id_cols = psq_id,
                         names_from = nvc_taxon_name,
                         values_fill = 0,
                         values_from = present) |>
      tibble::column_to_rownames(var = "psq_id") |>
      as.matrix()
    
    psquad_cm_he_prepped <- setupData$psquad_cm_he |>
      dplyr::select(psq_id, `F`, L, N, R, S)
    
    
    nvc_pquads_wide(nvc_pquads_wide_prepped)
    nvc_pquads_mean_unweighted_eivs(psquad_cm_he_prepped)
    
  }) |>
    bindEvent(runAnalysis(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
  
# Run DCA and CCA ---------------------------------------------------------
  mvaResults <- reactiveVal()
  observe({
    
    # Require selected objects are not NULL
    shiny::req(surveyData())
    shiny::req(nvcAssignment())
    shiny::req(nvc_pquads_wide())
    shiny::req(nvc_pquads_mean_unweighted_eivs())
    
    # Start busy spinner
    shinybusy::show_modal_spinner(
      spin = "fading-circle",
      color = "#3F9280",
      text = "Performing Local Reference, Restricted MVA"
    )
    
    # Isolate processes to prevent recursion when handling reactive objects not included in bindEvent
    shiny::isolate({
      
      nvcAssignment <- nvcAssignment()
      selectedReferenceSpaces <- selectedReferenceSpaces()
      nvc_pquads_wide <- nvc_pquads_wide()
      nvc_pquads_mean_unweighted_eivs <- nvc_pquads_mean_unweighted_eivs()
      surveyData <- surveyData()
      surveyData_long <- surveyData$surveyData_long
      ccaVars <- ccaVars()
      
    })
      
    # Create pattern to subset matrix rows
    codes_regex <- paste0("^(", stringr::str_c(selectedReferenceSpaces, collapse = "|"), ")(?<=)\\_")
    
    # Subset pseudo-quadrats for selected communities
    selected_pquads <- nvc_pquads_wide[stringr::str_detect(string = row.names(nvc_pquads_wide), pattern = codes_regex), ]

    # Remove columns (species) that are absent in all selected communities
    selected_pquads_prepped <- selected_pquads[, colSums(abs(selected_pquads)) != 0] |>
      tibble::as_tibble(rownames = NA)
    
    # Retrieve the unweighted mean Hill-Ellenberg scores for the pseudo-quadrats
    nvc_pquads_mean_unweighted_eivs_prepped <- nvc_pquads_mean_unweighted_eivs |>
      dplyr::filter(psq_id %in% rownames(selected_pquads_prepped)) |>
      tibble::column_to_rownames(var = "psq_id")

    # Perform a CCA on the selected pseudo-quadrats using selected Hill-Ellenberg scores
    selected_pquads_prepped_cca  <- vegan::cca(as.formula(paste0("nvc_pquads_mean_unweighted_eivs_prepped ~ ", paste0(c(RMAVIS:::ccaVars_vals[[ccaVars]]), collapse = " + "))), # selected_pquads_prepped ~ `F` + `L` + `N`
                                               data = nvc_pquads_mean_unweighted_eivs_prepped,
                                               na.action = na.exclude)
    
    # Extract CCA scores
    selected_pquads_prepped_cca_scores <- vegan::scores(selected_pquads_prepped_cca, display = "bp")
    
    # Extract CCA multiplier
    selected_pquads_prepped_cca_multiplier <- vegan:::ordiArrowMul(selected_pquads_prepped_cca_scores)
    
    # Create CCA arrow data
    CCA_arrowData <- selected_pquads_prepped_cca_scores #* selected_pquads_prepped_cca_multiplier
    CCA_arrowData <- CCA_arrowData |>
      tibble::as_tibble(rownames = NA) |>
      tibble::rownames_to_column(var = "Hill-Ellenberg")
    
    # Perform a DCA on the selected pseudo-quadrats
    selected_pquads_dca_results <- vegan::decorana(veg = selected_pquads_prepped)
    
    # Extract the DCA results species axis scores
    dca_results_pquads_species <- vegan::scores(selected_pquads_dca_results, tidy = TRUE) |>
      dplyr::filter(score == "species") |>
      dplyr::select(-score, -weight) |>
      dplyr::rename("Species" = label)
    
    # Extract the DCA results quadrat axis scores
    dca_results_pquads_site <- vegan::scores(selected_pquads_dca_results, tidy = TRUE) |>
      dplyr::filter(score == "sites") |>
      dplyr::select(-score, -weight) |>
      dplyr::rename("Quadrat" = label)
    
    # Prepare the pseudo-quadrat DCA results quadrat axis scores
    dca_results_pquads_site <- dca_results_pquads_site  |>
      dplyr::mutate("Year" = "Reference", .before  = "Quadrat") |>
      dplyr::mutate("Group" = "Reference", .before  = "Quadrat") |>
      dplyr::mutate("NVC.Comm" = stringr::str_extract(string = Quadrat, pattern = ".+?(?=\\_)"), .before  = "Quadrat")
    
  
    # Calculate the surveyData DCA results using the pseudo-quadrat species scores
    dca_results_sample_site <- surveyData_long |>
      tibble::as_tibble() |>
      dplyr::select(-Cover) |>
      dplyr::left_join(dca_results_pquads_species, by = "Species") |>
      tidyr::unite(col = "ID", c(Year, Group, Quadrat), sep = " - ", remove = FALSE) |>
      dplyr::group_by(ID, Year, Group, Quadrat) |>
      dplyr::summarise("DCA1" = mean(DCA1, na.rm = TRUE),
                       "DCA2" = mean(DCA2, na.rm = TRUE),
                       "DCA3" = mean(DCA3, na.rm = TRUE),
                       "DCA4" = mean(DCA4, na.rm = TRUE),
                       .groups = "drop") |>
      dplyr::mutate("NVC.Comm" = "Sample", .before  = "Quadrat")
    
    # Create convex hulls around the pseudo-quadrat DCA points.
    pquad_hulls_dca1dca2 <- dca_results_pquads_site |>
      dplyr::group_by(NVC.Comm) |>
      dplyr::slice(grDevices::chull(DCA1, DCA2)) |>
      dplyr::ungroup() |>
      dplyr::mutate("dcaAxes" = "dca1dca2")
    
    pquad_hulls_dca1dca3 <- dca_results_pquads_site |>
      dplyr::group_by(NVC.Comm) |>
      dplyr::slice(grDevices::chull(DCA1, DCA3)) |>
      dplyr::ungroup() |>
      dplyr::mutate("dcaAxes" = "dca1dca3")
    
    pquad_hulls_dca2dca3 <- dca_results_pquads_site |>
      dplyr::group_by(NVC.Comm) |>
      dplyr::slice(grDevices::chull(DCA2, DCA3)) |>
      dplyr::ungroup() |>
      dplyr::mutate("dcaAxes" = "dca2dca3")
    
    pquad_hulls <- rbind(pquad_hulls_dca1dca2,
                         pquad_hulls_dca1dca3,
                         pquad_hulls_dca2dca3)
    
    # Prepare the data required to draw arrows between points, ordered by Year
    if(length(unique(dca_results_sample_site$Year)) > 1){
      
      arrow_plot_data <- dca_results_sample_site |>
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
    pquad_centroids <- dca_results_pquads_site |>
      dplyr::group_by(NVC.Comm) |>
      dplyr::summarise("DCA1" = mean(DCA1),
                       "DCA2" = mean(DCA2),
                       "DCA3" = mean(DCA3),
                       "DCA4" = mean(DCA4)) |>
      dplyr::ungroup()
    
    # Calculate sample centroids
    sample_centroids <- dca_results_sample_site |>
      dplyr::group_by(NVC.Comm) |>
      dplyr::summarise("DCA1" = mean(DCA1),
                       "DCA2" = mean(DCA2),
                       "DCA3" = mean(DCA3),
                       "DCA4" = mean(DCA4)) |>
      dplyr::ungroup()
    
    # Compose list of DCA results objects
    mvaResults_list <- list("dca_results_pquads_species" = dca_results_pquads_species,
                            "dca_results_pquads_site" = dca_results_pquads_site,
                            "dca_results_sample_site" = dca_results_sample_site,
                            "pquad_hulls" = pquad_hulls,
                            "pquad_centroids" = pquad_centroids,
                            "sample_centroids" = sample_centroids,
                            "arrow_plot_data" = arrow_plot_data,
                            "CCA_arrowData" = CCA_arrowData)
    
    mvaResults(mvaResults_list)
    
    shinybusy::remove_modal_spinner()
      
      
  }) |>
    bindEvent(selectedReferenceSpaces(),
              ccaVars(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  
# Subset data and create plot ---------------------------------------------
    observe({
      
      shiny::req(mvaResults())
      mvaResults <- mvaResults()
      dcaAxisSelection <- dcaAxisSelection()
      groupSurveyPlots <- groupSurveyPlots()
      
      pquad_hulls_selected <- mvaResults$pquad_hulls |>
        dplyr::filter(dcaAxes == dcaAxisSelection) |>
        dplyr::select(-dcaAxes)
      
      if(dcaAxisSelection == "dca1dca2"){
        
        x_axis <- "DCA1"
        y_axis <- "DCA2"
        
      } else if(dcaAxisSelection == "dca1dca3"){
        
        x_axis <- "DCA1"
        y_axis <- "DCA3"
        
      } else if(dcaAxisSelection == "dca2dca3"){
        
        x_axis <- "DCA2"
        y_axis <- "DCA3"
        
      }
      
      if(groupSurveyPlots == "no"){
        
        dca_results_sample_site_selected <- mvaResults$dca_results_sample_site
        
        
      } else if(groupSurveyPlots == "group") {
        
        dca_results_sample_site_selected <- mvaResults$dca_results_sample_site |>
          dplyr::group_by(Year, Group) |>
          dplyr::summarise("DCA1" = mean(DCA1),
                           "DCA2" = mean(DCA2),
                           "DCA3" = mean(DCA3),
                           "DCA4" = mean(DCA4)) |>
          dplyr::ungroup()
        
        
      } else if(groupSurveyPlots == "year") {
        
        dca_results_sample_site_selected <- mvaResults$dca_results_sample_site |>
          dplyr::group_by(Year) |>
          dplyr::summarise("DCA1" = mean(DCA1),
                           "DCA2" = mean(DCA2),
                           "DCA3" = mean(DCA3),
                           "DCA4" = mean(DCA4)) |>
          dplyr::ungroup()
        
      }
      
      if(groupSurveyPlots == "no" && selectSurveyMethod() == "all"){
        
        dca_results_sample_site_selected <- mvaResults$dca_results_sample_site
        
      } else if(groupSurveyPlots == "no" && selectSurveyMethod() == "selectYears"){
        
        dca_results_sample_site_selected <- mvaResults$dca_results_sample_site |>
          dplyr::filter(Year %in% selectSurveyYears())
        
      } else if(groupSurveyPlots == "no" && selectSurveyMethod() == "selectGroups"){
        
        dca_results_sample_site_selected <- mvaResults$dca_results_sample_site |>
          dplyr::filter(Group %in% selectSurveyGroups())
        
      } else if(groupSurveyPlots == "no" && selectSurveyMethod() == "selectQuadrats"){
        
        dca_results_sample_site_selected <- mvaResults$dca_results_sample_site |>
          dplyr::filter(Quadrat %in% selectSurveyQuadrats())
        
      }
      
      # Prepare arrow data
      if(length(unique(dca_results_sample_site_selected$Year)) > 1){
        
        arrow_plot_data <- dca_results_sample_site_selected |>
          dplyr::arrange(Year) |>
          dplyr::group_by(dplyr::across(c(-Year, -DCA1, -DCA2, -DCA3, -DCA4))) |>
          dplyr::mutate("x" = get(x_axis), "y" = get(y_axis)) |>
          dplyr::ungroup() |>
          dplyr::mutate("endX" = dplyr::lead(x), "endY" = dplyr::lead(y)) |>
          dplyr::filter(!is.na(endX))
        
      } else {
        
        arrow_plot_data <- NULL
        
      }
      
      # Create an interactive plot of the DCA results
      output$mvaLocalRefRestrictedPlot <- plotly::renderPlotly({
        
        suppressWarnings(
          
          # Create ggplot2 plot
          mvaLocalRefRestrictedPlot_plot <- ggplot2::ggplot() +
            {if("referenceSpace" %in% dcaVars())ggplot2::geom_polygon(data = pquad_hulls_selected, 
                                                                      alpha = 0.2, 
                                                                      mapping = ggplot2::aes(x = .data[[x_axis]], 
                                                                                             y = .data[[y_axis]],
                                                                                             fill = NVC.Comm))} +
            {if("referenceCentroids" %in% dcaVars())ggplot2::geom_point(data = mvaResults$pquad_centroids,
                                                                        mapping = ggplot2::aes(x = .data[[x_axis]], 
                                                                                               y = .data[[y_axis]],
                                                                                               fill = NVC.Comm,
                                                                                               color = NVC.Comm),
                                                                        size = 3)} +
            {if("species" %in% dcaVars())ggplot2::geom_point(data = mvaResults$dca_results_pquads_species,
                                                             color = '#32a87d',
                                                             shape = 18,
                                                             mapping = ggplot2::aes(x = .data[[x_axis]], 
                                                                                    y = .data[[y_axis]],
                                                                                    Species = Species))} +
            {if("pseudoQuadrats" %in% dcaVars())ggplot2::geom_point(data = mvaResults$dca_results_pquads_site,
                                                                    mapping = ggplot2::aes(color = NVC.Comm,
                                                                                           Quadrat = Quadrat,
                                                                                           x = .data[[x_axis]], 
                                                                                           y = .data[[y_axis]]))} +
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
            {if("hillEllenberg" %in% dcaVars())ggplot2::geom_segment(data = mvaResults$CCA_arrowData,
                                                                     color = 'black',
                                                                     arrow = grid::arrow(),
                                                                     mapping = ggplot2::aes(x = 0,
                                                                                            y = 0,
                                                                                            xend = CCA1,
                                                                                            yend = CCA2,
                                                                                            label = `Hill-Ellenberg`))} +
            {if("hillEllenberg" %in% dcaVars())ggplot2::geom_text(data = mvaResults$CCA_arrowData,
                                                                  color = 'black',
                                                                  # position = ggplot2::position_dodge(width = 0.9),
                                                                  size = 5,
                                                                  mapping = ggplot2::aes(x = CCA1 * 1.075,
                                                                                         y = CCA2 * 1.075,
                                                                                         label = `Hill-Ellenberg`))} +
            ggplot2::theme_minimal()
          
        )
        
        if("trajectory" %in% dcaVars() & !is.null(arrow_plot_data) & !is.null(dca_results_sample_site_selected)){
          
          if(nrow(arrow_plot_data) > 0){
            
            mvaLocalRefRestrictedPlot_plotly <- plotly::ggplotly(p = mvaLocalRefRestrictedPlot_plot) |>
              plotly::add_annotations(data = arrow_plot_data,
                                      showarrow = TRUE,
                                      text = "",
                                      xref = "x", axref = "x",
                                      yref = "y", ayref = "y",
                                      x = ~endX,
                                      ax = ~x,
                                      y = ~endY,
                                      ay = ~y)
            
          }
          
        } else {
          
          mvaLocalRefRestrictedPlot_plotly <- plotly::ggplotly(p = mvaLocalRefRestrictedPlot_plot)
          
        }
        
        
        return(mvaLocalRefRestrictedPlot_plotly)  
      
      
      })
      
    }) |>
    bindEvent(mvaResults(),
              dcaAxisSelection(),
              dcaVars(),
              groupSurveyPlots(),
              selectSurveyMethod(),
              selectSurveyYears(),
              selectSurveyGroups(),
              selectSurveyQuadrats(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
    
  
  # Return list of DCA results objects
  return(mvaResults)
  
}
