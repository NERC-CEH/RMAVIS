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
    
    shinyjs::show(id = "diversitySummaryTable_div")
    shinyjs::show(id = "diversityIndicesTable_div")
    shinyjs::show(id = "speciesRichnessSiteTable_div")
    shinyjs::show(id = "speciesRichnessGroupTable_div")
    shinyjs::show(id = "speciesRichnessQuadratTable_div")
    
  }) |>
    bindEvent(resultsViewDiversity(),
              ignoreNULL = FALSE,
              ignoreInit = FALSE,
              once = TRUE)
  
  observe({
    
    # diversitySummaryTable
    if("diversitySummaryTable" %in% resultsViewDiversity()){
      shinyjs::show(id = "diversitySummaryTable_div")
    } else {
      shinyjs::hide(id = "diversitySummaryTable_div")
    }
    
    # diversityIndicesTable
    if("diversityIndicesTable" %in% resultsViewDiversity()){
      shinyjs::show(id = "diversityIndicesTable_div")
    } else {
      shinyjs::hide(id = "diversityIndicesTable_div")
    }
    
    # speciesRichnessSiteTable
    if("speciesRichnessSite" %in% resultsViewDiversity()){
      shinyjs::show(id = "speciesRichnessSiteTable_div")
    } else {
      shinyjs::hide(id = "speciesRichnessSiteTable_div")
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
    
    shinyjs::show(id = "diversitySummaryTable_div")
    shinyjs::show(id = "diversityIndicesTable_div")
  
  }) |>
    bindEvent(resultsViewDiversity(),
              ignoreNULL = FALSE,
              ignoreInit = FALSE,
              once = TRUE)

# Initialise Summary Table ------------------------------------------------
  diversitySummaryTable_init <- data.frame("Year" = integer(),
                                           "Metric" = character(),
                                           "Value" = character())
  
  diversitySummaryTable_rval <- reactiveVal(diversitySummaryTable_init)
  
  output$diversitySummaryTable <- rhandsontable::renderRHandsontable({
    
    diversitySummaryTable <- rhandsontable::rhandsontable(data = diversitySummaryTable_init,
                                                          rowHeaders = NULL,
                                                          width = "100%"#,
                                                          # overflow = "visible",
                                                          # stretchH = "all"
                                                          ) |>
      rhandsontable::hot_col(col = colnames(diversitySummaryTable_init), halign = "htCenter", readOnly = TRUE) |>
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
      htmlwidgets::onRender("
      function(el, x) {
        var hot = this.hot
        $('a[data-value=\"diversity_panel\"').on('click', function(){
          setTimeout(function() {hot.render();}, 0);
        })
      }")
    
    return(diversitySummaryTable)
    
  })
  

# Intialise Diversity Indices Table ---------------------------------------
  diversityIndicesTable_init <- data.frame("ID" = character(),
                                           "Richness" = integer(),
                                           "Shannon.Diversity" = double(),
                                           "Simpson.Diversity" = double(),
                                           "InverseSimpson.Diversity" = double(),
                                           "Shannon.Evenness" = double(),
                                           "Simpson.Evenness" = double()
                                           # "" = double(),
                                           # "" = double()
                                           )
  
  diversityIndicesTable_rval <- reactiveVal(diversityIndicesTable_init)
  
  output$diversityIndicesTable <- rhandsontable::renderRHandsontable({
    
    diversityIndicesTable <- rhandsontable::rhandsontable(data = diversityIndicesTable_init,
                                                                rowHeaders = NULL,
                                                                width = "100%"#,
                                                                # overflow = "visible",
                                                                # stretchH = "all"
    ) |>
      rhandsontable::hot_col(col = colnames(diversityIndicesTable_init), halign = "htCenter", readOnly = TRUE) |>
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
      htmlwidgets::onRender("
      function(el, x) {
        var hot = this.hot
        $('a[data-value=\"diversity_panel\"').on('click', function(){
          setTimeout(function() {hot.render();}, 0);
        })
      }")
    
    return(diversityIndicesTable)
    
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

# Species Richness --------------------------------------------------------
      
      # assign(x = "surveyTable", value = surveyTable, envir = .GlobalEnv)
      # assign(x = "surveyTableWide", value = surveyTableWide, envir = .GlobalEnv)
  
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
      
      speciesRichness_group_long <- speciesRichness_group |>
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
      
      # Summary Table
      summaryTable <- speciesRichness_quadrat |>
        dplyr::group_by(Year) |>
        dplyr::summarise("Alpha.Mean" = mean(Richness)) |>
        dplyr::left_join(speciesRichness_site_long, by = "Year") |>
        dplyr::rename("Gamma" = "Richness") |>
        dplyr::mutate("Beta" = (Gamma / Alpha.Mean) - 1) |>
        base::t() |>
        janitor::row_to_names(row = 1) |>
        tibble::as_tibble(rownames = "Metric")
      
      # Shannon Diversity
      shannonDiversity <- surveyTableWide |>
        vegan::diversity(index = "shannon") |>
        tibble::as_tibble(rownames = "ID") |>
        dplyr::rename("Shannon.Diversity" = "value")
      
      # Simpson Diversity
      simpsonDiversity <- surveyTableWide |>
        vegan::diversity(index = "simpson") |>
        tibble::as_tibble(rownames = "ID") |>
        dplyr::rename("Simpson.Diversity" = "value")
      
      # Inverse Simpson Diversity
      inverseSimpsonDiversity <- surveyTableWide |>
        vegan::diversity(index = "invsimpson") |>
        tibble::as_tibble(rownames = "ID") |>
        dplyr::rename("InverseSimpson.Diversity" = "value")
      
      # Shannon's/Pielou’s J evenness
      shannonsEvenness <- shannonDiversity |>
        # dplyr::left_join(speciesRichness_quadrat_long, by = "ID") |>
        # dplyr::mutate("Shannon.Evenness" = Shannon.Diversity / log(Richness)) |>
        dplyr::mutate("Shannon.Evenness" = (Shannon.Diversity / max(Shannon.Diversity)), .keep = "unused")
      
      # Simpson's evenness
      simpsonEvenness <- inverseSimpsonDiversity |>
        dplyr::left_join(speciesRichness_quadrat_long, by = "ID") |>
        dplyr::mutate("Simpson.Evenness" = (InverseSimpson.Diversity / Richness), .keep = "unused")
      
      # Rényi diversities and Hill Numbers
      # vegan::renyi(surveyTableWide)
      
      # Diversity Metrics Table
      diversityIndicesTable <- speciesRichness_quadrat_long |>
        dplyr::left_join(shannonDiversity, by = "ID") |>
        dplyr::left_join(simpsonDiversity, by = "ID") |>
        dplyr::left_join(inverseSimpsonDiversity, by = "ID") |>
        dplyr::left_join(shannonsEvenness, by = "ID") |>
        dplyr::left_join(simpsonEvenness, by = "ID")
      
      
    }) # Close isolate
    
    # Update summaryTable
    output$diversitySummaryTable <- rhandsontable::renderRHandsontable({
      
      diversitySummaryTable <- rhandsontable::rhandsontable(data = summaryTable,
                                                            rowHeaders = NULL,
                                                            width = "100%"#,
                                                            # overflow = "visible",
                                                            # stretchH = "all"
      ) |>
        rhandsontable::hot_col(col = colnames(summaryTable), halign = "htCenter", readOnly = TRUE) |>
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
        htmlwidgets::onRender("
        function(el, x) {
          var hot = this.hot
          $('a[data-value=\"diversity_panel\"').on('click', function(){
            setTimeout(function() {hot.render();}, 0);
          })
        }")
      
      return(diversitySummaryTable)
      
    })
    
    # Update diversityIndicesTable
    output$diversityIndicesTable <- rhandsontable::renderRHandsontable({
      
      diversityIndicesTable <- rhandsontable::rhandsontable(data = diversityIndicesTable,
                                                            rowHeaders = NULL,
                                                            width = "100%"#,
                                                            # overflow = "visible",
                                                            # stretchH = "all"
      ) |>
        rhandsontable::hot_col(col = colnames(diversityIndicesTable), halign = "htCenter", readOnly = TRUE) |>
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
        htmlwidgets::onRender("
      function(el, x) {
        var hot = this.hot
        $('a[data-value=\"diversity_panel\"').on('click', function(){
          setTimeout(function() {hot.render();}, 0);
        })
      }")
      
      return(diversityIndicesTable)
      
    })
    
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
      
    # betaDiversity_w <- vegan::betadiver(surveyTableWide, method = "w")
    # 
    # betaDiversity_w_mat <- as.matrix(betaDiversity_w)
    # 
    # assign(x = "surveyTableWide", value = surveyTableWide, envir = .GlobalEnv)
    # assign(x = "betaDiversity_w", value = betaDiversity_w, envir = .GlobalEnv)
    
    shinybusy::remove_modal_spinner()
    
  }) |>
    bindEvent(runAnalysis(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  
  outputOptions(output, "speciesRichnessSiteTable", suspendWhenHidden = FALSE)
  outputOptions(output, "speciesRichnessGroupTable", suspendWhenHidden = FALSE)
  outputOptions(output, "speciesRichnessQuadratTable", suspendWhenHidden = FALSE)
  
  # return()
  
}
