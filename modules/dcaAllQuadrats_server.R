dcaFixedSpace <- function(input, output, session, surveyTable, nvcAverageSim, sidebar_options) {
  
  ns <- session$ns
  
  # Retrieve sidebar options ------------------------------------------------
  runAnalysis <- reactiveVal()
  dcaVars <- reactiveVal()
  
  observe({
    
    runAnalysis(sidebar_options()$runAnalysis)
    dcaVars(sidebar_options()$dcaVars)
    
  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)
  
  
  observe({
    
    shiny::req(surveyTable())
    shiny::req(nvcAverageSim())
    
    shinybusy::show_modal_spinner(
      spin = "fading-circle",
      color = "#3F9280",
      text = "Performing DCA Analysis"
    )
    
    # # Retrieve the table, optionally modify the table without triggering recursion.
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
        
        regex <- paste0("(", code, ")(?<=)P")
        
        codes_regex <- c(codes_regex, regex)
        
        codes_regex <- stringr::str_c(codes_regex, collapse = "|")
        
      }
      
      nvc_pquads_final_wide_trimmed <- nvc_pquads_final_wide[stringr::str_detect(string = row.names(nvc_pquads_final_wide), pattern = codes_regex), ]
      
      # Remove columns (species) that are absent in all selected communities
      nvc_pquads_final_wide_prepped <- nvc_pquads_final_wide_trimmed[, colSums(abs(nvc_pquads_final_wide_trimmed)) != 0] |>
        as.data.frame()
      
      # Prepare survey data
      # surveyTable <- read.csv(file = "./data/bundled_data/example_data_long.csv")
      surveyTable <- surveyTable()
      
      surveyTable_prepped <- surveyTable |>
        # Convert cover estimates to presence/absence binary values
        dplyr::mutate(
          "Cover" = 
            dplyr::case_when(
              Cover > 0 ~ 1,
              is.na(Cover) ~ 0,
              TRUE ~ as.numeric(0)
            )
          ) |>
        tidyr::unite(col = "ID", c(Year, Site, Quadrat.Group, Quadrat), sep = " - ", remove = TRUE) |>
        dplyr::select(ID, Species, Cover)  |>
        dplyr::filter(!is.na(Cover)) |>
        tidyr::pivot_wider(id_cols = ID,
                           names_from = Species,
                           values_from = Cover) |>
        tibble::column_to_rownames(var = "ID")
      
      # Method 1
      dca_results_m1 <- vegan::decorana(veg = nvc_pquads_final_wide_prepped)
      
      # print("dca_results_m1")
      # print(dca_results_m1)
      
      dca_results_m1_species <- dca_results_m1$cproj |>
        tibble::as_tibble(rownames = "Species")
      
      # print("dca_results_m1_species")
      # print(dca_results_m1_species)
      
      dca_results_m1_quadrats <- dca_results_m1$rproj|>
        tibble::as_tibble(rownames = "Quadrat")
      
      # print("dca_results_m1_quadrats")
      # print(dca_results_m1_quadrats)
      
      method1_sampleDCA <- surveyTable |>
        tibble::as_tibble() |>
        dplyr::filter(!is.na(Cover)) |>
        dplyr::select(-Cover) |>
        dplyr::left_join(dca_results_m1_species, by = "Species") |>
        tidyr::unite(col = "ID", c(Year, Site, Quadrat.Group, Quadrat), sep = " - ", remove = TRUE) |>
        dplyr::group_by(ID) |>
        dplyr::summarise("DCA1" = mean(DCA1, na.rm = TRUE),
                         "DCA2" = mean(DCA2, na.rm = TRUE),
                         "DCA3" = mean(DCA3, na.rm = TRUE),
                         "DCA4" = mean(DCA4, na.rm = TRUE),
                         .groups = "drop") |>
        dplyr::rename("Quadrat" = "ID")
      

      
      method1_results1 <- method1_sampleDCA |>
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
          "Quadrat.Group" =
            dplyr::case_when(
              stringr::str_detect(string = Quadrat, pattern = stringr::str_c(NVC_communities_final, collapse = "|")) ~ stringr::str_extract(string = Quadrat, pattern = "^([A-Z]*)"),
              TRUE ~ as.character("Sample")
            ),
          .before  = "Quadrat"
        )
      

      
      method1_results2 <- dca_results_m1_quadrats  |>
        dplyr::mutate(
          "NVC.Comm" =
            dplyr::case_when(
              stringr::str_detect(string = Quadrat, pattern = stringr::str_c(NVC_communities_final, collapse = "|")) ~ stringr::str_extract(string = Quadrat, pattern = ".+?(?=P)"),
              TRUE ~ as.character("Sample")
            ),
          .before  = "Quadrat"
        ) |>
        dplyr::mutate(
          "Quadrat.Group" =
            dplyr::case_when(
              stringr::str_detect(string = Quadrat, pattern = stringr::str_c(NVC_communities_final, collapse = "|")) ~ stringr::str_extract(string = Quadrat, pattern = "^([A-Z]*)"),
              TRUE ~ as.character("Sample")
            ),
          .before  = "Quadrat"
        ) |>
        dplyr::mutate(
          "Year" =
            dplyr::case_when(
              stringr::str_detect(string = Quadrat, pattern = stringr::str_c(NVC_communities_final, collapse = "|")) ~ "Reference",
              TRUE ~ stringr::str_extract(string = Quadrat, pattern = "(\\d{4})")
            ),
          .before  = "Quadrat"
        )
      
      # print(method1_results1)
      # print(method1_results2)
      
      method1_results_all <- dplyr::bind_rows(method1_results1, method1_results2)
      
      method1_results2_hull <- method1_results2 |>
        dplyr::group_by(NVC.Comm) |>
        dplyr::slice(grDevices::chull(DCA1, DCA2))
      
      
      # Method 2
      nvc_pquads_final_wide_prepped_wSurveyTable_prepped <- nvc_pquads_final_wide_prepped |>
        dplyr::bind_rows(surveyTable_prepped) |>
        dplyr::mutate_all(~replace(., is.na(.), 0)) |>
        as.matrix()
      
      dca_results <- vegan::decorana(veg = nvc_pquads_final_wide_prepped_wSurveyTable_prepped)
      
      dca_results_psquad_axisScores <- dca_results$rproj |>
        as.data.frame() |>
        tibble::rownames_to_column(var = "Quadrat")  |>
        dplyr::mutate(
          "NVC.Comm" =
            dplyr::case_when(
              stringr::str_detect(string = Quadrat, pattern = stringr::str_c(NVC_communities_final, collapse = "|")) ~ stringr::str_extract(string = Quadrat, pattern = ".+?(?=P)"),
              TRUE ~ as.character("Sample")
              ),
          .before  = "Quadrat"
          ) |>
        dplyr::mutate(
          "NVC.Broad" =
            dplyr::case_when(
              stringr::str_detect(string = Quadrat, pattern = stringr::str_c(NVC_communities_final, collapse = "|")) ~ stringr::str_extract(string = Quadrat, pattern = "^([A-Z]*)"),
              TRUE ~ as.character("Sample")
            ),
          .before  = "NVC.Comm"
        )
    })
    
    output$method1DCAPlot <- plotly::renderPlotly({
      
      # Create a plot containing the path along a set of sample quadrats by year.
      arrow_plot <- ggplot2::ggplot() +
        ggplot2::geom_path(data = method1_results1,
                           mapping = ggplot2::aes(x = DCA1,
                                                  y = DCA2,
                                                  # group = Quadrat,
                                                  group = c("Year", "Quadrat.Group")),
                           arrow = grid::arrow())
      
      arrow_plot_data <- method1_results1 |>
        dplyr::arrange(Quadrat) |>
        dplyr::select("Year" = Year, 
                      "Quadrat" = Quadrat, 
                      "x" = DCA1, 
                      "y" = DCA2) |>
        dplyr::mutate("endX" = dplyr::lead(x),
                      "endY" = dplyr::lead(y)) |>
        dplyr::filter(!is.na(endX))    
      
      # assign(x = "method1_results1", value = method1_results1, envir = .GlobalEnv)
      # assign(x = "arrow_plot_data", value = arrow_plot_data, envir = .GlobalEnv)
      # assign(x = "arrow_plot_data1", value = arrow_plot_data1, envir = .GlobalEnv)
      # assign(x = "arrow_plot", value = arrowTest_plot, envir = .GlobalEnv)
      
      method1DCAPlot_plot <- ggplot2::ggplot() +
        {if("referenceSpace" %in% dcaVars())ggplot2::geom_polygon(data = method1_results2_hull, alpha = 0.2, 
                                                                  mapping = ggplot2::aes(x = DCA1, y = DCA2, fill = NVC.Comm, colour = NVC.Comm))} +
        {if("pseudoQuadrats" %in% dcaVars())ggplot2::geom_point(data = method1_results2,
                                                                mapping = ggplot2::aes(color = NVC.Comm,
                                                                                       Quadrat = Quadrat,
                                                                                       x = DCA1,
                                                                                       y = DCA2))} +
        {if("surveyQuadrats" %in% dcaVars())ggplot2::geom_point(data = method1_results1,
                                                                mapping = ggplot2::aes(color = NVC.Comm,
                                                                                       Quadrat = Quadrat,
                                                                                       x = DCA1,
                                                                                       y = DCA2))} +
        {if("species" %in% dcaVars())ggplot2::geom_point(data = dca_results_m1_species,
                                                         mapping = ggplot2::aes(x = DCA1, 
                                                                                y = DCA2,
                                                                                Species = Species))} +
        # ggplot2::geom_path(data = method1_results1,
        #                    mapping = ggplot2::aes(x = DCA1,
        #                                           y = DCA2),
        #                    arrow = grid::arrow()) +
      ggplot2::theme_minimal()
      
      if("surveyQuadratChange" %in% dcaVars()){
        
        method1DCAPlot_plotly <- plotly::ggplotly(p = method1DCAPlot_plot) |>
          plotly::add_annotations(data = arrow_plot_data,
                                  showarrow = TRUE,
                                  text = "",
                                  xref = "x", axref = "x",
                                  yref = "y", ayref = "y",
                                  x = ~endX,
                                  ax = ~x,
                                  y = ~endY,
                                  ay = ~y)
        
      } else {
        
        method1DCAPlot_plotly <- plotly::ggplotly(p = method1DCAPlot_plot)
      
      }
      
      
      return(method1DCAPlot_plotly)  
      
    })
    
    output$method2DCAPlot <- plotly::renderPlotly({
      
      
      method2DCAPlot_plot <- ggplot2::ggplot(data = dca_results_psquad_axisScores) +
        ggplot2::geom_point(mapping = ggplot2::aes(color = NVC.Comm,
                                                   label2 = Quadrat,
                                                   x = DCA1, 
                                                   y = DCA2)) +
        ggplot2::theme_minimal()
      
      method2DCAPlot_plotly <- plotly::ggplotly(p = method2DCAPlot_plot)
      
      
      return(method2DCAPlot_plotly)  
      
    })
    
    shinybusy::remove_modal_spinner()
    
    dca_results <- list()
    
  }) |>
    bindEvent(runAnalysis(),
              # dcaVars(),
              # surveyTablePrepped(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  # return(dca_results)
  
}