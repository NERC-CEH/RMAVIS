dcaSubsetNVC <- function(input, output, session, surveyTable, nvcAverageSim, sidebar_options) {
  
  ns <- session$ns
  
  # Retrieve sidebar options ------------------------------------------------
  runAnalysis <- reactiveVal()
  dcaVars <- reactiveVal()
  ccaVars <- reactiveVal()
  selectSurveyMethod <- reactiveVal()
  selectSurveyYears <- reactiveVal()
  selectSurveyQuadrats <- reactiveVal()
  selectSurveyGroups <- reactiveVal()
  
  observe({
    
    runAnalysis(sidebar_options()$runAnalysis)
    dcaVars(sidebar_options()$dcaVars)
    ccaVars(sidebar_options()$ccaVars)
    selectSurveyMethod(sidebar_options()$selectSurveyMethod)
    selectSurveyYears(sidebar_options()$selectSurveyYears)
    selectSurveyQuadrats(sidebar_options()$selectSurveyQuadrats)
    selectSurveyGroups(sidebar_options()$selectSurveyGroups)
    
  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)
  
  
  dcaSubsetNVCResults <- reactiveVal()
  
# Run DCA and CCA ---------------------------------------------------------
  observe({
    
    # Require selected objects are not NULL
    shiny::req(surveyTable())
    shiny::req(nvcAverageSim())
    
    # Start busy spinner
    shinybusy::show_modal_spinner(
      spin = "fading-circle",
      color = "#3F9280",
      text = "Performing DCA Analysis"
    )
    
    # Peform analysis in a reactive context without creating a reactive relationship
    shiny::isolate({
      
      # Get all NVC communities and sub-communities from nvc assignment results
      NVC_communities_all <- nvcAverageSim() |>
        dplyr::pull(NVC.Code)
      
      # Get all NVC communities from community and sub-community codes
      NVC_communities_fromSubCom <- stringr::str_replace(string = NVC_communities_all, 
                                                         pattern = "(\\d)[^0-9]+$", 
                                                         replace = "\\1") |>
        unique()
      
      NVC_communities_final <- unique(c(NVC_communities_all, NVC_communities_fromSubCom))
      
      
      # Create pattern to subset matrix rows
      codes_regex <- c()
      
      for(code in NVC_communities_final){
        
        regex <- paste0("^(", code, ")(?<=)P")
        
        codes_regex <- c(codes_regex, regex)
        
        codes_regex <- stringr::str_c(codes_regex, collapse = "|")
        
      }
      
      # assign(x = "codes_regex", value = codes_regex, envir = .GlobalEnv)
      
      # Subset pseudo-quadrats for selected communities
      selected_pquads <- nvc_pquads_final_wide[stringr::str_detect(string = row.names(nvc_pquads_final_wide), pattern = codes_regex), ]
      
      
      # assign(x = "selected_pquads", value = selected_pquads, envir = .GlobalEnv)
      
      
      # Remove columns (species) that are absent in all selected communities
      selected_pquads_prepped <- selected_pquads[, colSums(abs(selected_pquads)) != 0] |>
        tibble::as_tibble(rownames = NA)
      
      # assign(x = "selected_pquads_prepped", value = selected_pquads_prepped, envir = .GlobalEnv)
      
      # foo <- selected_pquads_prepped["SM8P8",]
      # foo <- foo[, colSums(abs(foo)) != 0]
      
      # Retieve the unweighted mean Hill-Ellenberg scores for the pseudo-quadrats
      nvc_pquads_mean_unweighted_eivs_prepped <- nvc_pquads_mean_unweighted_eivs |>
        dplyr::filter(Pid3 %in% rownames(selected_pquads_prepped)) |>
        tibble::column_to_rownames(var = "Pid3")
      
      # assign(x = "nvc_pquads_mean_unweighted_eivs_prepped", value = nvc_pquads_mean_unweighted_eivs_prepped, envir = .GlobalEnv)
  
      # print(ccaVars())
      # Perform a CCA on the selected pseudo-quadrats using F, L, and N scores
      selected_pquads_prepped_cca  <- vegan::cca(as.formula(paste0("selected_pquads_prepped ~ ", paste0(c(ccaVars()), collapse = " + "))), # selected_pquads_prepped ~ `F` + `L` + `N`
                                                 data = nvc_pquads_mean_unweighted_eivs_prepped,
                                                 na.action = na.exclude)
      
      # Extract CCA scores
      selected_pquads_prepped_cca_scores <- vegan::scores(selected_pquads_prepped_cca, display = "bp")
      
      # Extract CCA multiplier
      selected_pquads_prepped_cca_multiplier <- vegan:::ordiArrowMul(selected_pquads_prepped_cca_scores)
      
      # print(selected_pquads_prepped_cca_multiplier)
      
      # Create CCA arrow data
      CCA_arrowData <- selected_pquads_prepped_cca_scores #* selected_pquads_prepped_cca_multiplier
      CCA_arrowData <- CCA_arrowData |>
        tibble::as_tibble(rownames = NA) |>
        tibble::rownames_to_column(var = "Hill-Ellenberg")
      
      # assign(x = "CCA_arrowData", value = CCA_arrowData, envir = .GlobalEnv)
      
      # Perform a DCA on the selected pseudo-quadrats
      selected_pquads_dca_results <- vegan::decorana(veg = selected_pquads_prepped)
      
      # assign(x = "selected_pquads_dca_results", value = selected_pquads_dca_results, envir = .GlobalEnv)
      
      # Extract the DCA results species axis scores
      selected_pquads_dca_results_species <- vegan::scores(selected_pquads_dca_results, tidy = TRUE) |>
        dplyr::filter(score == "species") |>
        dplyr::select(-score, -weight) |>
        dplyr::rename("Species" = label)
      
      # Extract the DCA results quadrat axis scores
      selected_pquads_dca_results_quadrats <- vegan::scores(selected_pquads_dca_results, tidy = TRUE) |>
        dplyr::filter(score == "sites") |>
        dplyr::select(-score, -weight) |>
        dplyr::rename("Quadrat" = label)
      
      # print(vegan::scores(selected_pquads_dca_results))
      # 
      # print(selected_pquads_dca_results_quadrats)
      
      # assign(x = "foo", value = vegan::scores(selected_pquads_dca_results), envir = .GlobalEnv)
      
      # Prepare the pseudo-quadrat DCA results quadrat axis scores
      selected_pquads_dca_results_quadrats_final <- selected_pquads_dca_results_quadrats  |>
        dplyr::mutate("Year" = "Reference", .before  = "Quadrat") |>
        dplyr::mutate("Group" = "Reference", .before  = "Quadrat") |>
        dplyr::mutate("NVC.Comm" = stringr::str_extract(string = Quadrat, pattern = ".+?(?=P)"), .before  = "Quadrat")
      
    
      # Calculate the surveyTable DCA results using the pseudo-quadrat species scores
      surveyTable_dca_results_quadrats <- surveyTable() |>
        tibble::as_tibble() |>
        dplyr::filter(!is.na(Cover)) |>
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
      
      # assign(x = "surveyTable_dca_results_quadrats", value = surveyTable_dca_results_quadrats, envir = .GlobalEnv)
      
      # Create convex hulls around the pseudo-quadrat DCA points.
      selected_pquads_dca_results_quadrats_final_hull <- selected_pquads_dca_results_quadrats_final |>
        dplyr::group_by(NVC.Comm) |>
        dplyr::slice(grDevices::chull(DCA1, DCA2))
      
      # assign(x = "selected_pquads_dca_results_quadrats_final_hull", value = selected_pquads_dca_results_quadrats_final_hull, envir = .GlobalEnv)
      
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
    dcaSubsetNVCResults_list <- list("selected_pquads_dca_results_species_final" = selected_pquads_dca_results_species,
                                      "selected_pquads_dca_results_quadrats_final" = selected_pquads_dca_results_quadrats_final,
                                      "surveyTable_dca_results_quadrats" = surveyTable_dca_results_quadrats,
                                      "selected_pquads_dca_results_quadrats_final_hull" = selected_pquads_dca_results_quadrats_final_hull,
                                      "arrow_plot_data" = arrow_plot_data,
                                      "CCA_arrowData" = CCA_arrowData)
    
    dcaSubsetNVCResults(dcaSubsetNVCResults_list)
    
    shinybusy::remove_modal_spinner()
      
      
  }) |>
    bindEvent(runAnalysis(),
              # dcaVars(),
              ccaVars(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  
  

# Subset data and create plot ---------------------------------------------
    observe({
      
      dcaSubsetNVCResults <- dcaSubsetNVCResults()
      
      if(selectSurveyMethod() == "all"){
        
        surveyTable_dca_results_quadrats_selected <- dcaSubsetNVCResults$surveyTable_dca_results_quadrats
        
        arrow_plot_data_selected <- dcaSubsetNVCResults$arrow_plot_data
        
      } else if(selectSurveyMethod() == "selectYears"){
        
        surveyTable_dca_results_quadrats_selected <- dcaSubsetNVCResults$surveyTable_dca_results_quadrats |>
          dplyr::filter(Year %in% selectSurveyYears())
        
        arrow_plot_data_selected <- dcaSubsetNVCResults$arrow_plot_data |>
          dplyr::filter(Year %in% selectSurveyYears())
        
      } else if(selectSurveyMethod() == "selectGroups"){
        
        surveyTable_dca_results_quadrats_selected <- dcaSubsetNVCResults$surveyTable_dca_results_quadrats |>
          dplyr::filter(Group %in% selectSurveyGroups())
        
        arrow_plot_data_selected <- dcaSubsetNVCResults$arrow_plot_data |>
          dplyr::filter(Group %in% selectSurveyGroups())
        
      } else if(selectSurveyMethod() == "selectQuadrats"){
        
        surveyTable_dca_results_quadrats_selected <- dcaSubsetNVCResults$surveyTable_dca_results_quadrats |>
          dplyr::filter(Quadrat %in% selectSurveyQuadrats())
        
        arrow_plot_data_selected <- dcaSubsetNVCResults$arrow_plot_data |>
          dplyr::filter(Quadrat %in% selectSurveyQuadrats())
        
      }
      
      # Create an interactive plot of the DCA results
      output$dcaSubsetNVCPlot <- plotly::renderPlotly({
        
        suppressWarnings(
          
          # Create ggplot2 plot
          dcaSubsetNVCPlot_plot <- ggplot2::ggplot() +
            {if("referenceSpace" %in% dcaVars())ggplot2::geom_polygon(data = dcaSubsetNVCResults$selected_pquads_dca_results_quadrats_final_hull, alpha = 0.2, 
                                                                      mapping = ggplot2::aes(x = DCA1, y = DCA2, fill = NVC.Comm))} +
            {if("species" %in% dcaVars())ggplot2::geom_point(data = selected_pquads_dca_results_species,
                                                             color = '#32a87d',
                                                             shape = 18,
                                                             mapping = ggplot2::aes(x = DCA1, 
                                                                                    y = DCA2,
                                                                                    Species = Species))} +
            {if("pseudoQuadrats" %in% dcaVars())ggplot2::geom_point(data = dcaSubsetNVCResults$selected_pquads_dca_results_quadrats_final,
                                                                    mapping = ggplot2::aes(color = NVC.Comm,
                                                                                           Quadrat = Quadrat,
                                                                                           x = DCA1,
                                                                                           y = DCA2))} +
            {if("surveyQuadrats" %in% dcaVars())ggplot2::geom_point(data = surveyTable_dca_results_quadrats_selected,
                                                                    color = 'black',
                                                                    mapping = ggplot2::aes(Year = Year,
                                                                                           Group = Group,
                                                                                           Quadrat = Quadrat,
                                                                                           x = DCA1,
                                                                                           y = DCA2))} +
            {if("hillEllenberg" %in% dcaVars())ggplot2::geom_segment(data = dcaSubsetNVCResults$CCA_arrowData,
                                                                     color = 'black',
                                                                     arrow = grid::arrow(),
                                                                     mapping = ggplot2::aes(x = 0,
                                                                                            y = 0,
                                                                                            xend = CCA1,
                                                                                            yend = CCA2,
                                                                                            label = `Hill-Ellenberg`))} +
            {if("hillEllenberg" %in% dcaVars())ggplot2::geom_text(data = dcaSubsetNVCResults$CCA_arrowData,
                                                                  color = 'black',
                                                                  # position = ggplot2::position_dodge(width = 0.9),
                                                                  size = 5,
                                                                  mapping = ggplot2::aes(x = CCA1 * 1.075,
                                                                                         y = CCA2 * 1.075,
                                                                                         label = `Hill-Ellenberg`))} +
            ggplot2::theme_minimal()
          
        )
        
        if("surveyQuadratChange" %in% dcaVars() & !is.null(arrow_plot_data_selected)){
          
          if(nrow(arrow_plot_data_selected) > 0){
            
            dcaSubsetNVCPlot_plotly <- plotly::ggplotly(p = dcaSubsetNVCPlot_plot) |>
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
          
          dcaSubsetNVCPlot_plotly <- plotly::ggplotly(p = dcaSubsetNVCPlot_plot)
          
        }
        
        
        return(dcaSubsetNVCPlot_plotly)  
      
      
      })
      
    }) |>
    bindEvent(dcaSubsetNVCResults(),
              dcaVars(),
              selectSurveyMethod(),
              selectSurveyYears(),
              selectSurveyGroups(),
              selectSurveyQuadrats(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
    
  
  # Return list of DCA results objects
  return(dcaSubsetNVCResults)
  
}
