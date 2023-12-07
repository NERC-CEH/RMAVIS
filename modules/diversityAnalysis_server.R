diversityAnalysis <- function(input, output, session, surveyTable, surveyTablePrepped, sidebar_options) {
  
  ns <- session$ns
  
  # Retrieve sidebar options ------------------------------------------------
  runAnalysis <- reactiveVal()
  
  observe({
    
    runAnalysis(sidebar_options()$runAnalysis)
    
  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)
  
  
  metricsTableIDQuad_init <- data.frame("ID" = character(),
                                        "Richness" = double(),
                                        "Simpson.Diversity" = double()
  )

  metricsTableIDQuad_rval <- reactiveVal(metricsTableIDQuad_init)

  output$metricsTableIDQuad <- rhandsontable::renderRHandsontable({

    metricsTableIDQuad <- rhandsontable::rhandsontable(data = metricsTableIDQuad_init,
                                                       rowHeaders = NULL,
                                                       width = "100%"#,
                                                       # overflow = "visible",
                                                       # stretchH = "all"
                                                       ) |>
      rhandsontable::hot_col(col = colnames(metricsTableIDQuad_init), halign = "htCenter", readOnly = TRUE) |>
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
      htmlwidgets::onRender("
      function(el, x) {
        var hot = this.hot
        $('a[data-value=\"diversity_panel\"').on('click', function(){
          setTimeout(function() {hot.render();}, 0);
        })
      }")

    return(metricsTableIDQuad)

  })
  
  metricsTableID_init <- data.frame("ID" = character(),
                                    "Richness" = double()
  )
  
  metricsTableID_rval <- reactiveVal(metricsTableID_init)
  
  output$metricsTableID <- rhandsontable::renderRHandsontable({
    
    metricsTableID <- rhandsontable::rhandsontable(data = metricsTableID_init,
                                                 rowHeaders = NULL,
                                                 width = "100%"#,
                                                 # overflow = "visible",
                                                 # stretchH = "all"
                                                 ) |>
      rhandsontable::hot_col(col = colnames(metricsTableID_init), halign = "htCenter", readOnly = TRUE) |>
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
      htmlwidgets::onRender("
      function(el, x) {
        var hot = this.hot
        $('a[data-value=\"diversity_panel\"').on('click', function(){
          setTimeout(function() {hot.render();}, 0);
        })
      }")
    
    return(metricsTableID)
    
  })
  
  
  observe({
    
    shinybusy::show_modal_spinner(
      spin = "fading-circle",
      color = "#3F9280",
      text = "Calculating Diversity Metrics"
    )
    
    shiny::req(surveyTablePrepped())
    
    # shiny::req(input$metricsTableIDQuad)
    # shiny::req(input$metricsTableID)
    
    isolate({
      
      # Prepare Survey Table
      surveyTablePrepped <- surveyTablePrepped()
      
      surveyTablePrepped_id_wide <- surveyTablePrepped |>
        dplyr::group_by(ID, Quadrat) |>
        tidyr::pivot_wider(names_from = Species,
                           values_from = Cover) |>
        tidyr::unite(col = "ID", c(ID, Quadrat), sep = " - ", remove = TRUE) |>
        tibble::column_to_rownames(var = "ID") |>
        dplyr::mutate_all(~replace(., is.na(.), 0)) |>
        as.matrix() 

# Alpha Diversity ---------------------------------------------------------
      
      speciesRichness_idQuad <- surveyTablePrepped |>
        tidyr::unite(col = "ID", c(ID, Quadrat), sep = " - ", remove = TRUE) |>
        dplyr::group_by(ID) |>
        dplyr::summarise("Richness" = dplyr::n_distinct(Species)) |>
        dplyr::ungroup()


# Beta Diversity ----------------------------------------------------------
      
      # surveyTablePrepped_betadisper <- vegan::betadiver(surveyTablePrepped_id_wide)
      surveyTablePrepped_w <- vegan::betadiver(surveyTablePrepped_id_wide, method = "w")
      

# Gamma Diversity ---------------------------------------------------------
      
      speciesRichness_id <- surveyTablePrepped |>
        dplyr::group_by(ID) |>
        dplyr::summarise("Richness" = dplyr::n_distinct(Species)) |>
        dplyr::ungroup()


# Simpsons Diversity ------------------------------------------------------

      simpsonDiversity <- surveyTablePrepped_id_wide |>
        vegan::diversity(index = "simpson") |>
        tibble::as_tibble(rownames = "ID") |> # column_name = "Simpson.Diversity"
        dplyr::rename("Simpson.Diversity" = "value")
      

      
      metricsTableIDQuad_data <- speciesRichness_idQuad |>
        dplyr::left_join(simpsonDiversity, by = "ID")
      
      
    })
    
    output$metricsTableIDQuad <- rhandsontable::renderRHandsontable({
      
      metricsTableIDQuad <- rhandsontable::rhandsontable(data = metricsTableIDQuad_data,
                                                         rowHeaders = NULL,
                                                         width = "100%"#,
                                                         # overflow = "visible",
                                                         # stretchH = "all"
      ) |>
        rhandsontable::hot_col(col = colnames(metricsTableIDQuad_data), halign = "htCenter", readOnly = TRUE) |>
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
        htmlwidgets::onRender("
          function(el, x) {
            var hot = this.hot
            $('a[data-value=\"diversity_panel\"').on('click', function(){
              setTimeout(function() {hot.render();}, 0);
            })
          }")
      
      return(metricsTableIDQuad)
      
    })
    
    metricsTableIDQuad_rval(rhandsontable::hot_to_r(input$metricsTableIDQuad))
    
    
    output$metricsTableID <- rhandsontable::renderRHandsontable({
      
      metricsTableID <- rhandsontable::rhandsontable(data = speciesRichness_id,
                                                     rowHeaders = NULL,
                                                     width = "100%"#,
                                                     # overflow = "visible",
                                                     # stretchH = "all"
                                                     ) |>
        rhandsontable::hot_col(col = colnames(speciesRichness_id), halign = "htCenter", readOnly = TRUE) |>
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
        htmlwidgets::onRender("
          function(el, x) {
            var hot = this.hot
            $('a[data-value=\"diversity_panel\"').on('click', function(){
              setTimeout(function() {hot.render();}, 0);
            })
          }")
      
      return(metricsTableID)
      
    })
    
    metricsTableID_rval(rhandsontable::hot_to_r(input$metricsTableID))
    
    shinybusy::remove_modal_spinner()
    
  }) |>
    bindEvent(runAnalysis(),
              # surveyTablePrepped(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
}