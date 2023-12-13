mvaNationalRef <- function(input, output, session, surveyTable, nvcAssignment, sidebar_options) {
  
  ns <- session$ns
  
  # Retrieve sidebar options ------------------------------------------------
  runAnalysis <- reactiveVal()
  dcaAxisSelection <- reactiveVal()
  dcaVars <- reactiveVal()
  ccaVars <- reactiveVal()
  globalReferenceSpaces <- reactiveVal()
  selectSurveyMethod <- reactiveVal()
  selectSurveyYears <- reactiveVal()
  selectSurveyQuadrats <- reactiveVal()
  selectSurveyGroups <- reactiveVal()
  
  observe({
    
    runAnalysis(sidebar_options()$runAnalysis)
    dcaAxisSelection(sidebar_options()$dcaAxisSelection)
    dcaVars(sidebar_options()$dcaVars)
    ccaVars(sidebar_options()$ccaVars)
    globalReferenceSpaces(sidebar_options()$globalReferenceSpaces)
    selectSurveyMethod(sidebar_options()$selectSurveyMethod)
    selectSurveyYears(sidebar_options()$selectSurveyYears)
    selectSurveyQuadrats(sidebar_options()$selectSurveyQuadrats)
    selectSurveyGroups(sidebar_options()$selectSurveyGroups)
    
  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)
  
  
  mvaNationalRefResults <- reactiveVal()
  
# Run DCA and CCA ---------------------------------------------------------
  observe({
    
    # Require selected objects are not NULL
    shiny::req(surveyTable())
    shiny::req(nvcAssignment())
    
    # Start busy spinner
    shinybusy::show_modal_spinner(
      spin = "fading-circle",
      color = "#3F9280",
      text = "Calculating National Reference DCA"
    )
    
    # Peform analysis in a reactive context without creating a reactive relationship
    shiny::isolate({
      
       # Remove columns (species) that are absent in all selected communities
      selected_pquads_prepped <- nvc_pquads_final_wide
      
      # Retieve the unweighted mean Hill-Ellenberg scores for the pseudo-quadrats
      nvc_pquads_mean_unweighted_eivs_prepped <- nvc_pquads_mean_unweighted_eivs |>
        dplyr::filter(Pid3 %in% rownames(selected_pquads_prepped)) |>
        tibble::column_to_rownames(var = "Pid3")
      
      # # Perform a CCA on the selected pseudo-quadrats 
      # selected_pquads_prepped_cca  <- vegan::cca(as.formula(paste0("selected_pquads_prepped ~ ", paste0(c(ccaVars()), collapse = " + "))),
      #                                            data = nvc_pquads_mean_unweighted_eivs_prepped,
      #                                            na.action = na.exclude)
      # 
      # # Extract CCA scores
      # selected_pquads_prepped_cca_scores <- vegan::scores(selected_pquads_prepped_cca, display = "bp")
      # 
      # # Extract CCA multiplier
      # selected_pquads_prepped_cca_multiplier <- vegan:::ordiArrowMul(selected_pquads_prepped_cca_scores)
      # 
      # 
      # # Create CCA arrow data
      # CCA_arrowData <- selected_pquads_prepped_cca_scores #* selected_pquads_prepped_cca_multiplier
      # CCA_arrowData <- CCA_arrowData |>
      #   tibble::as_tibble(rownames = NA) |>
      #   tibble::rownames_to_column(var = "Hill-Ellenberg")
      
      # Retrieve pre-calc
      selected_pquads_dca_results <- nvc_pquad_dca_all
      
      # assign(x = "selected_pquads_dca_results", value = selected_pquads_dca_results, envir = .GlobalEnv)
      
      # Extract the DCA results species axis scores
      selected_pquads_dca_results_species <- vegan::scores(selected_pquads_dca_results, tidy = TRUE) |>
        dplyr::filter(score == "species") |>
        dplyr::select(-score, -weight) |>
        dplyr::rename("Species" = label)
      
      # assign(x = "selected_pquads_dca_results_species", value = selected_pquads_dca_results_species, envir = .GlobalEnv)
      
      # Extract the DCA results quadrat axis scores
      selected_pquads_dca_results_quadrats <- vegan::scores(selected_pquads_dca_results, tidy = TRUE) |>
        dplyr::filter(score == "sites") |>
        dplyr::select(-score, -weight) |>
        dplyr::rename("Quadrat" = label)
      
      # assign(x = "selected_pquads_dca_results_quadrats", value = selected_pquads_dca_results_quadrats, envir = .GlobalEnv)
      
      # Prepare the pseudo-quadrat DCA results quadrat axis scores
      selected_pquads_dca_results_quadrats_final <- selected_pquads_dca_results_quadrats  |>
        dplyr::mutate("Year" = "Reference", .before  = "Quadrat") |>
        dplyr::mutate("Group" = "Reference", .before  = "Quadrat") |>
        dplyr::mutate("NVC.Comm" = stringr::str_extract(string = Quadrat, pattern = ".+?(?=P)"), .before  = "Quadrat")
      
    
      # assign(x = "selected_pquads_dca_results_quadrats_final", value = selected_pquads_dca_results_quadrats_final, envir = .GlobalEnv)
      
      # Calculate the surveyTable DCA results using the pseudo-quadrat species scores
      surveyTable_dca_results_quadrats <- surveyTable() |> #()
        tibble::as_tibble() |>
        dplyr::select(-Cover) |>
        dplyr::left_join(selected_pquads_dca_results_species, by = "Species") |>
        tidyr::unite(col = "ID", c(Year, Group, Quadrat), sep = " - ", remove = FALSE) |>
        dplyr::group_by(ID, Year, Group, Quadrat) |>
        dplyr::summarise("DCA1" = mean(DCA1, na.rm = TRUE),
                         "DCA2" = mean(DCA2, na.rm = TRUE),
                         "DCA3" = mean(DCA3, na.rm = TRUE),
                         "DCA4" = mean(DCA4, na.rm = TRUE),
                         .groups = "drop") |>
        dplyr::mutate("NVC.Comm" = "Sample", .before  = "Quadrat")
      
      # assign(x = "surveyTable", value = surveyTable(), envir = .GlobalEnv)
      
      # assign(x = "surveyTable_dca_results_quadrats", value = surveyTable_dca_results_quadrats, envir = .GlobalEnv)
      
      # Create convex hulls around the pseudo-quadrat DCA points.
      # selected_pquads_dca_results_quadrats_final_hull <- surveyTable_dca_results_quadrats |>
      #   dplyr::group_by(NVC.Comm) |>
      #   dplyr::slice(grDevices::chull(DCA1, DCA2))
      
      # Prepare the data required to draw arrows between points, ordered by Year
      if(length(unique(surveyTable_dca_results_quadrats$Year)) > 1){
        
        arrow_plot_data <- surveyTable_dca_results_quadrats |>
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
      
    }) # close isolate
    
    # Compose list of DCA results objects
    mvaNationalRefResults_list <- list("selected_pquads_dca_results_species_final" = selected_pquads_dca_results_species,
                                       "selected_pquads_dca_results_quadrats_final" = selected_pquads_dca_results_quadrats_final,
                                       "surveyTable_dca_results_quadrats" = surveyTable_dca_results_quadrats,
                                       # "selected_pquads_dca_results_quadrats_final_hull" = selected_pquads_dca_results_quadrats_final_hull,
                                       "arrow_plot_data" = arrow_plot_data#,
                                       #"CCA_arrowData" = CCA_arrowData
                                       )
    
    mvaNationalRefResults(mvaNationalRefResults_list)
    
    shinybusy::remove_modal_spinner()
      
      
  }) |>
    bindEvent(runAnalysis(),
              # dcaVars(),
              ccaVars(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  
  

# Subset data and create plot ---------------------------------------------
    observe({
      
      mvaNationalRefResults <- mvaNationalRefResults()
      
      if(selectSurveyMethod() == "all"){
        
        surveyTable_dca_results_quadrats_selected <- mvaNationalRefResults$surveyTable_dca_results_quadrats
        
        # print(surveyTable_dca_results_quadrats_selected)
        
        arrow_plot_data_selected <- mvaNationalRefResults$arrow_plot_data
        
      } else if(selectSurveyMethod() == "selectYears"){
        
        surveyTable_dca_results_quadrats_selected <- mvaNationalRefResults$surveyTable_dca_results_quadrats |>
          dplyr::filter(Year %in% selectSurveyYears())
        
        arrow_plot_data_selected <- mvaNationalRefResults$arrow_plot_data |>
          dplyr::filter(Year %in% selectSurveyYears())
        
      } else if(selectSurveyMethod() == "selectGroups"){
        
        surveyTable_dca_results_quadrats_selected <- mvaNationalRefResults$surveyTable_dca_results_quadrats |>
          dplyr::filter(Group %in% selectSurveyGroups())
        
        arrow_plot_data_selected <- mvaNationalRefResults$arrow_plot_data |>
          dplyr::filter(Group %in% selectSurveyGroups())
        
      } else if(selectSurveyMethod() == "selectQuadrats"){
        
        surveyTable_dca_results_quadrats_selected <- mvaNationalRefResults$surveyTable_dca_results_quadrats |>
          dplyr::filter(Quadrat %in% selectSurveyQuadrats())
        
        arrow_plot_data_selected <- mvaNationalRefResults$arrow_plot_data |>
          dplyr::filter(Quadrat %in% selectSurveyQuadrats())
        
      }
      
      dcaAxisSelection <- dcaAxisSelection()
      
      # Create an interactive plot of the DCA results
      output$mvaNationalRefPlot <- plotly::renderPlotly({
        
        if(dcaAxisSelection == "dca1dca2"){
          
          x_axis <- "DCA1"
          y_axis <- "DCA2"
          
          nvc_pquad_dca_all_hulls_selected <- nvc_pquad_dca_all_hulls |>
            dplyr::filter(NVC %in% globalReferenceSpaces(),
                          dcaAxes == "dca1dca2") |>
            dplyr::select(-dcaAxes)
          
        } else if(dcaAxisSelection == "dca1dca3"){
          
          x_axis <- "DCA1"
          y_axis <- "DCA3"
          
          nvc_pquad_dca_all_hulls_selected <- nvc_pquad_dca_all_hulls |>
            dplyr::filter(NVC %in% globalReferenceSpaces(),
                          dcaAxes == "dca1dca3") |>
            dplyr::select(-dcaAxes)
          
        } else if(dcaAxisSelection == "dca2dca3"){
          
          x_axis <- "DCA2"
          y_axis <- "DCA3"
          
          nvc_pquad_dca_all_hulls_selected <- nvc_pquad_dca_all_hulls |>
            dplyr::filter(NVC %in% globalReferenceSpaces(),
                          dcaAxes == "dca2dca3") |>
            dplyr::select(-dcaAxes)
          
          print(nvc_pquad_dca_all_hulls_selected)
          
        }
        
        suppressWarnings(
          
          # Create ggplot2 plot
          mvaNationalRefPlot_plot <- ggplot2::ggplot() +
            {if("referenceSpace" %in% dcaVars())ggplot2::geom_polygon(data = nvc_pquad_dca_all_hulls_selected, alpha = 0.2, 
                                                                      mapping = ggplot2::aes(x = DCA1, y = DCA2, fill = NVC))} +
            {if("species" %in% dcaVars())ggplot2::geom_point(data = mvaNationalRefResults$selected_pquads_dca_results_species,
                                                             color = '#32a87d',
                                                             shape = 18,
                                                             mapping = ggplot2::aes(x = .data[[x_axis]], 
                                                                                    y = .data[[y_axis]],
                                                                                    Species = Species))} +
            {if("surveyQuadrats" %in% dcaVars())ggplot2::geom_point(data = surveyTable_dca_results_quadrats_selected,
                                                                    color = 'black',
                                                                    mapping = ggplot2::aes(Year = Year,
                                                                                           Group = Group,
                                                                                           Quadrat = Quadrat,
                                                                                           x = .data[[x_axis]],
                                                                                           y = .data[[y_axis]]))} +
            # {if("hillEllenberg" %in% dcaVars())ggplot2::geom_segment(data = mvaNationalRefResults$CCA_arrowData,
            #                                                          color = 'black',
            #                                                          arrow = grid::arrow(),
            #                                                          mapping = ggplot2::aes(x = 0,
            #                                                                                 y = 0,
            #                                                                                 xend = CCA1,
            #                                                                                 yend = CCA2,
            #                                                                                 label = `Hill-Ellenberg`))} +
            # {if("hillEllenberg" %in% dcaVars())ggplot2::geom_text(data = mvaNationalRefResults$CCA_arrowData,
            #                                                       color = 'black',
            #                                                       # position = ggplot2::position_dodge(width = 0.9),
            #                                                       size = 5,
            #                                                       mapping = ggplot2::aes(x = CCA1 * 1.075,
            #                                                                              y = CCA2 * 1.075,
            #                                                                              label = `Hill-Ellenberg`))} +
            ggplot2::theme_minimal()
          
        )
        
        if("surveyQuadratChange" %in% dcaVars() & !is.null(arrow_plot_data_selected)){
          
          if(nrow(arrow_plot_data_selected) > 0){
            
            mvaNationalRefPlot_plotly <- plotly::ggplotly(p = mvaNationalRefPlot_plot) |>
              plotly::add_annotations(data = arrow_plot_data_selected,
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
          
          mvaNationalRefPlot_plotly <- plotly::ggplotly(p = mvaNationalRefPlot_plot)
          
        }
        
        
        return(mvaNationalRefPlot_plotly)  
      
      
      })
      
    }) |>
    bindEvent(mvaNationalRefResults(),
              dcaAxisSelection(),
              dcaVars(),
              globalReferenceSpaces(),
              selectSurveyMethod(),
              selectSurveyYears(),
              selectSurveyGroups(),
              selectSurveyQuadrats(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
    
  
  # Return list of DCA results objects
  return(mvaNationalRefResults)
  
}
