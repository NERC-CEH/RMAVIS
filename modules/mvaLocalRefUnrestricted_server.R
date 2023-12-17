mvaLocalRefUnrestricted <- function(input, output, session, surveyTableWide, nvcAssignment, sidebar_options) {
  
  ns <- session$ns
  
  # Retrieve sidebar options ------------------------------------------------
  runAnalysis <- reactiveVal()
  dcaAxisSelection <- reactiveVal()
  dcaVars <- reactiveVal()
  selectSurveyMethod <- reactiveVal()
  selectSurveyYears <- reactiveVal()
  selectSurveyGroups <- reactiveVal()
  selectSurveyQuadrats <- reactiveVal()
  
  observe({
    
    runAnalysis(sidebar_options()$runAnalysis)
    dcaAxisSelection(sidebar_options()$dcaAxisSelection)
    dcaVars(sidebar_options()$dcaVars)
    selectSurveyMethod(sidebar_options()$selectSurveyMethod)
    selectSurveyYears(sidebar_options()$selectSurveyYears)
    selectSurveyGroups(sidebar_options()$selectSurveyGroups)
    selectSurveyQuadrats(sidebar_options()$selectSurveyQuadrats)
    
  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)
  
  
  mvaLocalRefUnrestrictedResults <- reactiveVal()
  
  observe({
    
    # shiny::req(surveyTable())
    shiny::req(surveyTableWide())
    shiny::req(nvcAssignment())
    
    shinybusy::show_modal_spinner(
      spin = "fading-circle",
      color = "#3F9280",
      text = "Performing DCA Analysis"
    )
    
    surveyTableWide <- surveyTableWide()
    
    # # Retrieve the table, optionally modify the table without triggering recursion.
    shiny::isolate({
      
      # Get all NVC communities and sub-communities from nvc assignment results
      NVC_communities_all <- nvcAssignment() |>
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
      
      # Subset pseudo-quadrats for selected communities
      nvc_pquads_final_wide_trimmed <- nvc_pquads_final_wide[stringr::str_detect(string = row.names(nvc_pquads_final_wide), pattern = codes_regex), ]
      
      # Remove columns (species) that are absent in all selected communities
      nvc_pquads_final_wide_prepped <- nvc_pquads_final_wide_trimmed[, colSums(abs(nvc_pquads_final_wide_trimmed)) != 0] |>
        as.data.frame()
      
      # print(surveyTableWide)
      
      # assign(x = "nvc_pquads_final_wide_prepped", value = nvc_pquads_final_wide_prepped, envir = .GlobalEnv)
      # 
      # assign(x = "surveyTableWide", value = surveyTableWide, envir = .GlobalEnv)
      
      surveyTableWide_prepped <- surveyTableWide |>
        as.data.frame() |>
        dplyr::mutate_if(is.numeric, ~1 * (. != 0))
        
      # Combine the pseudo-quadrats and survey data into a single matrix
      nvc_pquads_final_wide_prepped_wSurveyTableWide <- nvc_pquads_final_wide_prepped |>
        dplyr::bind_rows(surveyTableWide_prepped) |>
        dplyr::mutate_all(~replace(., is.na(.), 0)) |>
        as.matrix()
      
      # assign(x = "nvc_pquads_final_wide_prepped_wSurveyTableWide", value = nvc_pquads_final_wide_prepped_wSurveyTableWide, envir = .GlobalEnv)
      
      # Perform a DCA on the combined pseudo-quadrat and survey data
      pquads_surveyTable_dca_results <- vegan::decorana(veg = nvc_pquads_final_wide_prepped_wSurveyTableWide)
      
      # Extract the DCA results species axis scores
      pquads_surveyTable_dca_results_species <- vegan::scores(pquads_surveyTable_dca_results, tidy = TRUE) |>
        dplyr::filter(score == "species") |>
        dplyr::select(-score, -weight) |>
        dplyr::rename("Species" = label)
      
      pquads_surveyTable_dca_results_species_unique <- pquads_surveyTable_dca_results_species |>
        dplyr::filter(Species %in% setdiff(colnames(surveyTableWide), colnames(nvc_pquads_final_wide_prepped)))
      
      # Extract the DCA results quadrat axis scores
      pquads_surveyTable_dca_results_quadrats <- vegan::scores(pquads_surveyTable_dca_results, tidy = TRUE) |>
        dplyr::filter(score == "sites") |>
        dplyr::select(-score, -weight) |>
        dplyr::rename("Quadrat" = label)
      
      pquads_surveyTable_dca_results_quadrats <- pquads_surveyTable_dca_results_quadrats |>
        dplyr::mutate(
          "Year" =
            dplyr::case_when(
              stringr::str_detect(string = Quadrat, pattern = stringr::str_c(NVC_communities_final, collapse = "|")) ~ "Reference",
              TRUE ~ stringr::str_extract(string = Quadrat, pattern = "(\\d{4})")
            ),
          .before  = "Quadrat"
        ) |>
        dplyr::mutate(
          "NVC.Comm" =
            dplyr::case_when(
              stringr::str_detect(string = Quadrat, pattern = stringr::str_c(NVC_communities_final, collapse = "|")) ~ stringr::str_extract(string = Quadrat, pattern = ".+?(?=P)"),
              TRUE ~ as.character("Sample")
            ),
          .before  = "Quadrat"
        ) |>
        dplyr::mutate(
          "Group" =
            dplyr::case_when(
              stringr::str_detect(string = Quadrat, pattern = stringr::str_c(NVC_communities_final, collapse = "|")) == TRUE ~ "Reference",
              stringr::str_detect(string = Quadrat, pattern = stringr::str_c(NVC_communities_final, collapse = "|")) == FALSE ~ stringr::str_extract(string = Quadrat, pattern = "\\d{4}\\s-\\s(.*)\\s-\\s.*", group = 1),
              TRUE ~ as.character("")
            ),
          .before  = "Quadrat"
        )
      
      
      pquads_surveyTable_dca_results_quadrats_pquads <- pquads_surveyTable_dca_results_quadrats |>
        dplyr::filter(NVC.Comm != "Sample")
        
      pquads_surveyTable_dca_results_quadrats_sample <- pquads_surveyTable_dca_results_quadrats |>
        dplyr::filter(NVC.Comm == "Sample") |>
        dplyr::mutate("ID" = Quadrat, .before = "Year") |>
        dplyr::mutate("Quadrat" = stringr::str_extract(string = Quadrat, pattern = "\\d{4}\\s-\\s.*\\s-\\s(.*)", group = 1))
        
        
      dcaAxisSelection <- dcaAxisSelection()
      
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
      
      # Create convex hulls around the pseudo-quadrat DCA points.
      pquads_surveyTable_dca_results_quadrats_hull <- pquads_surveyTable_dca_results_quadrats_pquads |>
        dplyr::group_by(NVC.Comm) |>
        dplyr::slice(grDevices::chull(get(x_axis), get(y_axis))) |>
        dplyr::ungroup()
      
      # Prepare the data required to draw arrows between points, ordered by Year
      if(length(unique(pquads_surveyTable_dca_results_quadrats_sample$Year)) > 1){
        
        arrow_plot_data <- pquads_surveyTable_dca_results_quadrats_sample |>
          dplyr::arrange(Year) |>
          dplyr::select("Year" = Year, 
                        "Group" = Group,
                        "Quadrat" = Quadrat,
                        "x" = tidyselect::all_of(x_axis), 
                        "y" = tidyselect::all_of(y_axis)) |>
          dplyr::group_by(Quadrat) |>
          dplyr::mutate("endX" = dplyr::lead(x),
                        "endY" = dplyr::lead(y)) |>
          dplyr::filter(!is.na(endX)) |>
          dplyr::ungroup()
        
      } else {
        
        arrow_plot_data <- NULL
        
      }
      
    }) # Close isolate
    
    shinybusy::remove_modal_spinner()
    
    # Compose list of DCA results objects
    mvaLocalRefUnrestrictedResults_list <- list("pquads_surveyTable_dca_results_species" = pquads_surveyTable_dca_results_species,
                                                "pquads_surveyTable_dca_results_quadrats_sample" = pquads_surveyTable_dca_results_quadrats_sample,
                                                "pquads_surveyTable_dca_results_quadrats_pquads" = pquads_surveyTable_dca_results_quadrats_pquads,
                                                "pquads_surveyTable_dca_results_quadrats_hull" = pquads_surveyTable_dca_results_quadrats_hull,
                                                "pquads_surveyTable_dca_results_species_unique" = pquads_surveyTable_dca_results_species_unique,
                                                "arrow_plot_data" = arrow_plot_data)
    
    mvaLocalRefUnrestrictedResults(mvaLocalRefUnrestrictedResults_list)
    
  }) |>
    bindEvent(runAnalysis(),
              dcaAxisSelection(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  
  observe({
    
    mvaLocalRefUnrestrictedResults <- mvaLocalRefUnrestrictedResults()
    
    if(selectSurveyMethod() == "all"){
      
      pquads_surveyTable_dca_results_quadrats_sample_selected <- mvaLocalRefUnrestrictedResults$pquads_surveyTable_dca_results_quadrats_sample
      
      arrow_plot_data_selected <- mvaLocalRefUnrestrictedResults$arrow_plot_data
      
    } else if(selectSurveyMethod() == "selectYears"){
      
      pquads_surveyTable_dca_results_quadrats_sample_selected <- mvaLocalRefUnrestrictedResults$pquads_surveyTable_dca_results_quadrats_sample |>
        dplyr::filter(Year %in% selectSurveyYears())
      
      arrow_plot_data_selected <- mvaLocalRefUnrestrictedResults$arrow_plot_data |>
        dplyr::filter(Year %in% selectSurveyYears())
      
    } else if(selectSurveyMethod() == "selectGroups"){
      
      pquads_surveyTable_dca_results_quadrats_sample_selected <- mvaLocalRefUnrestrictedResults$pquads_surveyTable_dca_results_quadrats_sample |>
        dplyr::filter(Group %in% selectSurveyGroups())
      
      arrow_plot_data_selected <- mvaLocalRefUnrestrictedResults$arrow_plot_data |>
        dplyr::filter(Group %in% selectSurveyGroups())
      
    } else if(selectSurveyMethod() == "selectQuadrats"){
      
      pquads_surveyTable_dca_results_quadrats_sample_selected <- mvaLocalRefUnrestrictedResults$pquads_surveyTable_dca_results_quadrats_sample |>
        dplyr::filter(Quadrat %in% selectSurveyQuadrats())
      
      arrow_plot_data_selected <- mvaLocalRefUnrestrictedResults$arrow_plot_data |>
        dplyr::filter(Quadrat %in% selectSurveyQuadrats())
      
    }
    
    dcaAxisSelection <- dcaAxisSelection()
    
    output$mvaLocalRefUnrestrictedPlot <- plotly::renderPlotly({
      
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
      
      suppressWarnings(
        
        mvaLocalRefUnrestrictedPlot_plot <- ggplot2::ggplot() +
          {if("referenceSpace" %in% dcaVars())ggplot2::geom_polygon(data = mvaLocalRefUnrestrictedResults$pquads_surveyTable_dca_results_quadrats_hull, alpha = 0.2, 
                                                                    mapping = ggplot2::aes(x = .data[[x_axis]], 
                                                                                           y = .data[[y_axis]],
                                                                                           fill = NVC.Comm))} +
          {if("species" %in% dcaVars())ggplot2::geom_point(data = mvaLocalRefUnrestrictedResults$pquads_surveyTable_dca_results_species,
                                                           color = '#32a87d',
                                                           shape = 18,
                                                           mapping = ggplot2::aes(x = .data[[x_axis]], 
                                                                                  y = .data[[y_axis]],
                                                                                  Species = Species))} +
          {if("pseudoQuadrats" %in% dcaVars())ggplot2::geom_point(data = mvaLocalRefUnrestrictedResults$pquads_surveyTable_dca_results_quadrats_pquads,
                                                                  mapping = ggplot2::aes(color = NVC.Comm,
                                                                                         Quadrat = Quadrat,
                                                                                         x = .data[[x_axis]], 
                                                                                         y = .data[[y_axis]]))} +
          {if("surveyQuadrats" %in% dcaVars())ggplot2::geom_point(data = pquads_surveyTable_dca_results_quadrats_sample_selected,
                                                                  color = 'black',
                                                                  mapping = ggplot2::aes(Year = Year,
                                                                                         # Group = Group,
                                                                                         Quadrat = Quadrat,
                                                                                         x = .data[[x_axis]], 
                                                                                         y = .data[[y_axis]]))} +
          {if("uniqSurveySpecies" %in% dcaVars())ggplot2::geom_point(data = mvaLocalRefUnrestrictedResults$pquads_surveyTable_dca_results_species_unique,
                                                                     color = '#32a87d',
                                                                     shape = 18,
                                                                     mapping = ggplot2::aes(x = .data[[x_axis]], 
                                                                                            y = .data[[y_axis]],
                                                                                            Species = Species))} +
          ggplot2::theme_minimal()
        
      )
      
      if("surveyQuadratChange" %in% dcaVars() & !is.null(arrow_plot_data_selected)){
        
        if(nrow(arrow_plot_data_selected) > 0){
          
          mvaLocalRefUnrestrictedPlot_plotly <- plotly::ggplotly(p = mvaLocalRefUnrestrictedPlot_plot) |>
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
        
        mvaLocalRefUnrestrictedPlot_plotly <- plotly::ggplotly(p = mvaLocalRefUnrestrictedPlot_plot)
        
      }
      
      
      return(mvaLocalRefUnrestrictedPlot_plotly)  
      
    })
    
  }) |>
    bindEvent(mvaLocalRefUnrestrictedResults(),
              dcaAxisSelection(),
              dcaVars(),
              selectSurveyMethod(),
              selectSurveyYears(),
              selectSurveyGroups(),
              selectSurveyQuadrats(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  return(mvaLocalRefUnrestrictedResults)
  
}
