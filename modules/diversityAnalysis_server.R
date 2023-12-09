diversityAnalysis <- function(input, output, session, surveyTable, surveyTableWide, sidebar_options) {
  
  ns <- session$ns
  
# Retrieve sidebar options ------------------------------------------------
  runAnalysis <- reactiveVal()
  resultsViewDiversity <- reactiveVal()
  
  observe({
    
    runAnalysis(sidebar_options()$runAnalysis)
    resultsViewDiversity(sidebar_options()$resultsViewDiversity)
    
  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)

  

# Show/Hide Results -------------------------------------------------------

  
  observe({
    
    shinyjs::show(id = "speciesRichnessSiteTable_div")
    shinyjs::show(id = "speciesRecordedOneYearOnlyTable_div")
    shinyjs::show(id = "speciesRichnessGroupTable_div")
    shinyjs::show(id = "speciesRichnessQuadratTable_div")
    
  }) |>
    bindEvent(resultsViewDiversity(),
              ignoreNULL = FALSE,
              ignoreInit = FALSE,
              once = TRUE)
  
  observe({
    
    # speciesRichnessSiteTable
    if("speciesRichnessSite" %in% resultsViewDiversity()){
      shinyjs::show(id = "speciesRichnessSiteTable_div")
    } else {
      shinyjs::hide(id = "speciesRichnessSiteTable_div")
    }
    
    # speciesRecordedOneYearOnlyTable
    if("speciesRecordedOneYearOnly" %in% resultsViewDiversity()){
      shinyjs::show(id = "speciesRecordedOneYearOnlyTable_div")
    } else {
      shinyjs::hide(id = "speciesRecordedOneYearOnlyTable_div")
    }
    
    # speciesRichnessGroupTable
    if("speciesRichnessGroup" %in% resultsViewDiversity()){
      shinyjs::show(id = "speciesRichnessGroupTable_div")
    } else {
      shinyjs::hide(id = "speciesRichnessGroupTable_div")
    }
    
    # speciesRichnessQuadratTable
    if("speciesRichnessQuadrat" %in% resultsViewDiversity()){
      shinyjs::show(id = "speciesRichnessQuadratTable_div")
    } else {
      shinyjs::hide(id = "speciesRichnessQuadratTable_div")
    }

    
  }) |>
    bindEvent(resultsViewDiversity(),
              ignoreInit = FALSE,
              ignoreNULL = FALSE)

  observe({
    
    shinyjs::show(id = "speciesRichnessSiteTable_div")
  
  }) |>
    bindEvent(resultsViewDiversity(),
              ignoreNULL = FALSE,
              ignoreInit = FALSE,
              once = TRUE)
  

# Initialise species unique to year table ---------------------------------
  speciesRecordedOneYearOnlyTable_init <- data.frame("Year" = integer(),
                                              "Species" = character())
  
  speciesRecordedOneYearOnlyTable_rval <- reactiveVal(speciesRecordedOneYearOnlyTable_init)
  
  output$speciesRecordedOneYearOnlyTable <- rhandsontable::renderRHandsontable({
    
    speciesRecordedOneYearOnlyTable <- rhandsontable::rhandsontable(data = speciesRecordedOneYearOnlyTable_init,
                                                                    rowHeaders = NULL,
                                                                    width = "100%"#,
                                                                    # overflow = "visible",
                                                                    # stretchH = "all"
                                                                    ) |>
      rhandsontable::hot_col(col = colnames(speciesRecordedOneYearOnlyTable_init), halign = "htCenter", readOnly = TRUE) |>
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
      htmlwidgets::onRender("
      function(el, x) {
        var hot = this.hot
        $('a[data-value=\"diversity_panel\"').on('click', function(){
          setTimeout(function() {hot.render();}, 0);
        })
      }")
    
    return(speciesRecordedOneYearOnlyTable)
    
  })

  
# Initialise Species Richness Quadrat Table -------------------------------
  speciesRichnessQuadratTable_init <- data.frame("Year" = integer(),
                                                 "Group" = character(),
                                                 "Quadrat" = character(),
                                                 "Richness" = double())
  
  speciesRichnessQuadratTable_rval <- reactiveVal(speciesRichnessQuadratTable_init)

  output$speciesRichnessQuadratTable <- rhandsontable::renderRHandsontable({

    speciesRichnessQuadratTable <- rhandsontable::rhandsontable(data = speciesRichnessQuadratTable_init,
                                                                rowHeaders = NULL,
                                                                width = "100%"#,
                                                                # overflow = "visible",
                                                                # stretchH = "all"
                                                                ) |>
      rhandsontable::hot_col(col = colnames(speciesRichnessQuadratTable_init), halign = "htCenter", readOnly = TRUE) |>
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
      htmlwidgets::onRender("
      function(el, x) {
        var hot = this.hot
        $('a[data-value=\"diversity_panel\"').on('click', function(){
          setTimeout(function() {hot.render();}, 0);
        })
      }")

    return(speciesRichnessQuadratTable)

  })
  

# Initialise Species Richness Group Table ---------------------------------
  speciesRichnessGroupTable_init <- data.frame("Year" = integer(),
                                               "Group" = character(),
                                               "Richness" = double())
  
  speciesRichnessGroupTable_rval <- reactiveVal(speciesRichnessGroupTable_init)
  
  output$speciesRichnessGroupTable <- rhandsontable::renderRHandsontable({
    
    speciesRichnessGroupTable <- rhandsontable::rhandsontable(data = speciesRichnessGroupTable_init,
                                                              rowHeaders = NULL,
                                                              width = "100%"#,
                                                              # overflow = "visible",
                                                              # stretchH = "all"
    ) |>
      rhandsontable::hot_col(col = colnames(speciesRichnessGroupTable_init), halign = "htCenter", readOnly = TRUE) |>
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
      htmlwidgets::onRender("
      function(el, x) {
        var hot = this.hot
        $('a[data-value=\"diversity_panel\"').on('click', function(){
          setTimeout(function() {hot.render();}, 0);
        })
      }")
    
    return(speciesRichnessGroupTable)
    
  })


# Initialise Species Richness Site Table ----------------------------------
  speciesRichnessSiteTable_init <- data.frame("Year" = integer(),
                                              "Richness" = double())
  
  speciesRichnessSiteTable_rval <- reactiveVal(speciesRichnessSiteTable_init)
  
  output$speciesRichnessSiteTable <- rhandsontable::renderRHandsontable({
    
    speciesRichnessSiteTable <- rhandsontable::rhandsontable(data = speciesRichnessSiteTable_init,
                                                             rowHeaders = NULL,
                                                             width = "100%"#,
                                                             # overflow = "visible",
                                                             # stretchH = "all"
                                                             ) |>
      rhandsontable::hot_col(col = colnames(speciesRichnessSiteTable_init), halign = "htCenter", readOnly = TRUE) |>
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
      htmlwidgets::onRender("
      function(el, x) {
        var hot = this.hot
        $('a[data-value=\"diversity_panel\"').on('click', function(){
          setTimeout(function() {hot.render();}, 0);
        })
      }")
    
    return(speciesRichnessSiteTable)
    
  })
  
  

# Calculate Diversity Metrics, Update Tables ------------------------------
  observe({
    
    shinybusy::show_modal_spinner(
      spin = "fading-circle",
      color = "#3F9280",
      text = "Calculating Diversity Metrics"
    )
    
    shiny::req(surveyTable())
    shiny::req(surveyTableWide())
    
    surveyTable <- surveyTable()
    surveyTableWide <- surveyTableWide()
    
    isolate({

# Species unique to year --------------------------------------------------
      surveyTable_species_all <- surveyTable |>
        dplyr::select(Year, Species) |>
        dplyr::distinct()
      
      species_uniq_to_year <- tibble::tibble()
      
      for (year in unique(surveyTable_species_all$Year)) {
        
        surveyTable_species_year <- surveyTable_species_all |>
          dplyr::filter(Year == year) |>
          dplyr::pull(Species)
        

        surveyTable_species_notYear <- surveyTable_species_all |>
          dplyr::filter(Year != year) |>
          dplyr::pull(Species)       
        
        surveyTable_species_diff <- setdiff(surveyTable_species_year, surveyTable_species_notYear)
        
        surveyTable_species_diff_df <- tibble::tibble("Year" = year,
                                                      "Species" = surveyTable_species_diff) |>
          dplyr::group_by(Year) |>
          dplyr::summarise(Species = toString(Species)) |>
          dplyr::ungroup()
        
        species_uniq_to_year <- species_uniq_to_year |>
          dplyr::bind_rows(surveyTable_species_diff_df)
        
      }
      
      output$speciesRecordedOneYearOnlyTable <- rhandsontable::renderRHandsontable({
        
        speciesRecordedOneYearOnlyTable <- rhandsontable::rhandsontable(data = species_uniq_to_year,
                                                                 rowHeaders = NULL#,
                                                                 # width = "100%"#,
                                                                 # overflow = "visible",
                                                                 # stretchH = "all"
                                                                 ) |>
          rhandsontable::hot_col(col = colnames(species_uniq_to_year), halign = "htCenter", readOnly = TRUE) |>
          rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
          rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
          htmlwidgets::onRender("
          function(el, x) {
            var hot = this.hot
            $('a[data-value=\"diversity_panel\"').on('click', function(){
              setTimeout(function() {hot.render();}, 0);
            })
          }")
        
        return(speciesRecordedOneYearOnlyTable)
        
      })
      
      speciesRecordedOneYearOnlyTable_rval <- reactiveVal(rhandsontable::hot_to_r(input$speciesRecordedOneYearOnlyTable))

# Species Richness --------------------------------------------------------
      
      assign(x = "surveyTable", value = surveyTable, envir = .GlobalEnv)
  
      # Species Richness - Quadrat
      speciesRichness_quadrat <- surveyTable |>
        dplyr::group_by(Year, Group, Quadrat) |>
        dplyr::summarise("Richness" = dplyr::n_distinct(Species)) |>
        dplyr::ungroup()
      
      speciesRichness_quadrat_wide <- speciesRichness_quadrat |>
        tidyr::pivot_wider(id_cols = c(Group, Quadrat),
                           names_from = Year,
                           values_from = Richness)
      
      speciesRichness_quadrat_long <- speciesRichness_quadrat |>
        tidyr::unite(col = "ID", c(Year, Group, Quadrat), sep = " - ", remove = TRUE)
      
      
      # Species Richness - Group
      speciesRichness_group <- surveyTable |>
        dplyr::group_by(Year, Group) |>
        dplyr::summarise("Richness" = dplyr::n_distinct(Species)) |>
        dplyr::ungroup()
      
      speciesRichness_group_wide <- speciesRichness_group |>
        tidyr::pivot_wider(id_cols = c(Group),
                           names_from = Year,
                           values_from = Richness)
      
      speciesRichness_quadrat_long <- speciesRichness_group |>
        tidyr::unite(col = "ID", c(Year, Group), sep = " - ", remove = TRUE)
      
      # Species Richness - Site
      speciesRichness_site <- surveyTable |>
        dplyr::group_by(Year) |>
        dplyr::summarise("Richness" = dplyr::n_distinct(Species)) |>
        dplyr::ungroup()
      
      speciesRichness_site_wide <- speciesRichness_site |>
        dplyr::mutate("Site" = "Site", .before = "Year") |>
        tidyr::pivot_wider(id_cols = Site,
                           names_from = Year,
                           values_from = Richness)
      
      speciesRichness_site_long <- speciesRichness_site
      
      
    }) # Close isolate
    
    # Update speciesRichnessSiteTable
    output$speciesRichnessSiteTable <- rhandsontable::renderRHandsontable({
      
      speciesRichnessSiteTable <- rhandsontable::rhandsontable(data = speciesRichness_site_wide,
                                                                  rowHeaders = NULL,
                                                                  width = "100%"#,
                                                                  # overflow = "visible",
                                                                  # stretchH = "all"
                                                                  ) |>
        rhandsontable::hot_col(col = colnames(speciesRichness_site_wide), halign = "htCenter", readOnly = TRUE) |>
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
        htmlwidgets::onRender("
        function(el, x) {
          var hot = this.hot
          $('a[data-value=\"diversity_panel\"').on('click', function(){
            setTimeout(function() {hot.render();}, 0);
          })
        }")
      
      return(speciesRichnessSiteTable)
      
    })
    
    speciesRichnessSiteTable_rval(rhandsontable::hot_to_r(input$speciesRichnessSiteTable))
    
    
    
    # Update speciesRichnessGroupTable
    output$speciesRichnessGroupTable <- rhandsontable::renderRHandsontable({
      
      speciesRichnessGroupTable <- rhandsontable::rhandsontable(data = speciesRichness_group_wide,
                                                                  rowHeaders = NULL,
                                                                  width = "100%"#,
                                                                  # overflow = "visible",
                                                                  # stretchH = "all"
      ) |>
        rhandsontable::hot_col(col = colnames(speciesRichness_group_wide), halign = "htCenter", readOnly = TRUE) |>
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
        htmlwidgets::onRender("
        function(el, x) {
          var hot = this.hot
          $('a[data-value=\"diversity_panel\"').on('click', function(){
            setTimeout(function() {hot.render();}, 0);
          })
        }")
      
      return(speciesRichnessGroupTable)
      
    })
    
    speciesRichnessGroupTable_rval(rhandsontable::hot_to_r(input$speciesRichnessGroupTable))
    
    
    
    # Update speciesRichnessQuadratTable
    output$speciesRichnessQuadratTable <- rhandsontable::renderRHandsontable({
      
      speciesRichnessQuadratTable <- rhandsontable::rhandsontable(data = speciesRichness_quadrat_wide,
                                                                  rowHeaders = NULL,
                                                                  width = "100%"#,
                                                                  # overflow = "visible",
                                                                  # stretchH = "all"
      ) |>
        rhandsontable::hot_col(col = colnames(speciesRichness_quadrat_wide), halign = "htCenter", readOnly = TRUE) |>
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
        htmlwidgets::onRender("
        function(el, x) {
          var hot = this.hot
          $('a[data-value=\"diversity_panel\"').on('click', function(){
            setTimeout(function() {hot.render();}, 0);
          })
        }")
      
      return(speciesRichnessQuadratTable)
      
    })
    
    speciesRichnessQuadratTable_rval(rhandsontable::hot_to_r(input$speciesRichnessQuadratTable))


# Beta Diversity ----------------------------------------------------------
      
    isolate({
      
      # betaDiversity_w <- vegan::betadiver(surveyTableWide, method = "w")
      # 
      # betaDiversity_w_mat <- as.matrix(betaDiversity_w)
      # 
      # assign(x = "surveyTableWide", value = surveyTableWide, envir = .GlobalEnv)
      # assign(x = "betaDiversity_w", value = betaDiversity_w, envir = .GlobalEnv)
      
    })


# Simpsons Diversity ------------------------------------------------------

    simpsonDiversity <- surveyTableWide |>
      vegan::diversity(index = "simpson") |>
      tibble::as_tibble(rownames = "ID") |>
      dplyr::rename("Simpson.Diversity" = "value")
      
    
    
    shinybusy::remove_modal_spinner()
    
  }) |>
    bindEvent(runAnalysis(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  
  outputOptions(output, "speciesRecordedOneYearOnlyTable", suspendWhenHidden = FALSE)
  outputOptions(output, "speciesRichnessSiteTable", suspendWhenHidden = FALSE)
  outputOptions(output, "speciesRichnessGroupTable", suspendWhenHidden = FALSE)
  outputOptions(output, "speciesRichnessQuadratTable", suspendWhenHidden = FALSE)
  
  # return()
  
}
