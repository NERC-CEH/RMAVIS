diversityAnalysis <- function(input, output, session, surveyTable, surveyTableWide, sidebar_options) {
  
  ns <- session$ns
  
# Retrieve sidebar options ------------------------------------------------
  runAnalysis <- reactiveVal()
  
  observe({
    
    runAnalysis(sidebar_options()$runAnalysis)
    
  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)


# Initialise Alpha Diversity Quadrat Table --------------------------------
  alphaDiversityQuadratTable_init <- data.frame("Year" = integer(),
                                                "Group" = character(),
                                                "Quadrat" = character(),
                                                "Richness" = double())
  
  alphaDiversityQuadratTable_rval <- reactiveVal(alphaDiversityQuadratTable_init)

  output$alphaDiversityQuadratTable <- rhandsontable::renderRHandsontable({

    alphaDiversityQuadratTable <- rhandsontable::rhandsontable(data = alphaDiversityQuadratTable_init,
                                                               rowHeaders = NULL,
                                                               width = "100%"#,
                                                               # overflow = "visible",
                                                               # stretchH = "all"
                                                               ) |>
      rhandsontable::hot_col(col = colnames(alphaDiversityQuadratTable_init), halign = "htCenter", readOnly = TRUE) |>
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
      htmlwidgets::onRender("
      function(el, x) {
        var hot = this.hot
        $('a[data-value=\"diversity_panel\"').on('click', function(){
          setTimeout(function() {hot.render();}, 0);
        })
      }")

    return(alphaDiversityQuadratTable)

  })
  
  
# Initialise Alpha Diversity Group Table --------------------------------

  
# Initialise Beta Diversity Quadrat Table ---------------------------------


# Initialise Beta Diversity Group Table -----------------------------------
  

# Initialise Gamma Diversity Site Table -----------------------------------
  gammaDiversitySiteTable_init <- data.frame("Year" = integer(),
                                             "Richness" = double()
  )
  
  gammaDiversitySiteTable_rval <- reactiveVal(gammaDiversitySiteTable_init)
  
  output$gammaDiversitySiteTable <- rhandsontable::renderRHandsontable({
    
    gammaDiversitySiteTable <- rhandsontable::rhandsontable(data = gammaDiversitySiteTable_init,
                                                            rowHeaders = NULL,
                                                            width = "100%"#,
                                                            # overflow = "visible",
                                                            # stretchH = "all"
                                                            ) |>
      rhandsontable::hot_col(col = colnames(gammaDiversitySiteTable_init), halign = "htCenter", readOnly = TRUE) |>
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
      htmlwidgets::onRender("
      function(el, x) {
        var hot = this.hot
        $('a[data-value=\"diversity_panel\"').on('click', function(){
          setTimeout(function() {hot.render();}, 0);
        })
      }")
    
    return(gammaDiversitySiteTable)
    
  })
  
  

# Calculate Diversity Metrics, Update Tables ------------------------------
  observe({
    
    shinybusy::show_modal_spinner(
      spin = "fading-circle",
      color = "#3F9280",
      text = "Calculating Diversity Metrics"
    )
    
    shiny::req(surveyTableWide())
    
    # shiny::req(input$metricsTableIDQuad)
    # shiny::req(input$metricsTableID)
    
    isolate({
      
      surveyTable <- surveyTable()
      surveyTableWide <- surveyTableWide()

# Alpha Diversity ---------------------------------------------------------
      
      alphaDiversity_quadrat <- surveyTable |>
        dplyr::group_by(Year, Group, Quadrat) |>
        dplyr::summarise("Richness" = dplyr::n_distinct(Species)) |>
        dplyr::ungroup() |>
        tidyr::unite(col = "ID", c(Year, Group, Quadrat), sep = " - ", remove = TRUE)
      
      alphaDiversity_group <- surveyTable |>
        dplyr::group_by(Year, Group) |>
        dplyr::summarise("Richness" = dplyr::n_distinct(Species)) |>
        dplyr::ungroup() |>
        tidyr::unite(col = "ID", c(Year, Group), sep = " - ", remove = TRUE)
      
      output$alphaDiversityQuadratTable <- rhandsontable::renderRHandsontable({
        
        alphaDiversityQuadratTable <- rhandsontable::rhandsontable(data = alphaDiversity_quadrat,
                                                                   rowHeaders = NULL,
                                                                   width = "100%"#,
                                                                   # overflow = "visible",
                                                                   # stretchH = "all"
        ) |>
          rhandsontable::hot_col(col = colnames(alphaDiversity_quadrat), halign = "htCenter", readOnly = TRUE) |>
          rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
          rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
          htmlwidgets::onRender("
          function(el, x) {
            var hot = this.hot
            $('a[data-value=\"diversity_panel\"').on('click', function(){
              setTimeout(function() {hot.render();}, 0);
            })
          }")
        
        return(alphaDiversityQuadratTable)
        
      })
      
      alphaDiversityQuadratTable_rval(rhandsontable::hot_to_r(input$alphaDiversityQuadratTable))


# Beta Diversity ----------------------------------------------------------
      
      # surveyTableWide_betadisper <- vegan::betadiver(surveyTableWide_id_wide)
      # surveyTableWide_w <- vegan::betadiver(surveyTableWide_id_wide, method = "w")
      

# Gamma Diversity ---------------------------------------------------------
      
      gammaDiversity_site <- surveyTable |>
        dplyr::group_by(Year) |>
        dplyr::summarise("Richness" = dplyr::n_distinct(Species)) |>
        dplyr::ungroup() |>
        dplyr::mutate("ID" = Year, .before = "Year", .keep = "unused")
      
      output$gammaDiversitySiteTable <- rhandsontable::renderRHandsontable({
        
        gammaDiversitySiteTable <- rhandsontable::rhandsontable(data = gammaDiversity_site,
                                                                rowHeaders = NULL,
                                                                width = "100%"#,
                                                                # overflow = "visible",
                                                                # stretchH = "all"
        ) |>
          rhandsontable::hot_col(col = colnames(gammaDiversity_site), halign = "htCenter", readOnly = TRUE) |>
          rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
          rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
          htmlwidgets::onRender("
          function(el, x) {
            var hot = this.hot
            $('a[data-value=\"diversity_panel\"').on('click', function(){
              setTimeout(function() {hot.render();}, 0);
            })
          }")
        
        return(gammaDiversitySiteTable)
        
      })
      
      gammaDiversitySiteTable_rval(rhandsontable::hot_to_r(input$gammaDiversitySiteTable))


# Simpsons Diversity ------------------------------------------------------

      simpsonDiversity <- surveyTableWide |>
        vegan::diversity(index = "simpson") |>
        tibble::as_tibble(rownames = "ID") |>
        dplyr::rename("Simpson.Diversity" = "value")
      
      
    })
    
    
    
    shinybusy::remove_modal_spinner()
    
  }) |>
    bindEvent(runAnalysis(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
}
