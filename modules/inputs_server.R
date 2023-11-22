inputs <- function(input, output, session) {

  ns <- session$ns
  
# Survey Data Entry Table -------------------------------------------------

  surveyTable_init <- data.frame("Site" = c(1, 1, 1),
                                 "Sample" = c("A", "A", "A"),
                                 "Species" = c("Acer campestre", "Acer campestre", "Acer campestre"),
                                 "Abundance" = c(0.7, 0.15, 0.15)
                                 )
  
  surveyTable_rval <- reactiveVal(surveyTable_init)
  
  output$surveyTable <- rhandsontable::renderRHandsontable({
    
    surveyTable <- rhandsontable::rhandsontable(data = surveyTable_init,
                                                rowHeaders = NULL,
                                                width = "100%"
                                                # overflow = "visible",
                                                # stretchH = "all"
                                                ) |>
      rhandsontable::hot_col(col = colnames(surveyTable_init), halign = "htCenter") |>
      rhandsontable::hot_col(
        col = "Site",
        readOnly = FALSE,
        type = "text"
      ) |>
      rhandsontable::hot_col(
        col = "Sample",
        readOnly = FALSE,
        type = "text"
      ) |>
      rhandsontable::hot_col(
        col = "Species",
        readOnly = FALSE,
        type = "dropdown",
        source = c(species_list),
        strict = TRUE
      ) |>
      rhandsontable::hot_col(
        col = "Abundance",
        readOnly = FALSE,
        type = "numeric",
        strict = FALSE
      ) |>
      rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
      htmlwidgets::onRender("
      function(el, x) {
        var hot = this.hot
        $('a[data-value=\"basic_inputs_panel\"').on('click', function(){
          setTimeout(function() {hot.render();}, 0);
        })
      }")
    
    return(surveyTable)
    
  })
  
  # observe({
  #   
  #   req(input$surveyTable)
  #   
  #   # Retrieve the table, optionally modify the table without triggering recursion.
  #   isolate({
  #     
  #     surveyTable <- as.data.frame(rhandsontable::hot_to_r(input$surveyTable))
  # 
  #   })
  #   
  #   output$surveyTable <- rhandsontable::renderRHandsontable({
  #     
  #     surveyTable <- rhandsontable::rhandsontable(data = surveyTable,
  #                                                 rowHeaders = NULL,
  #                                                 # overflow = "visible",
  #                                                 stretchH = "all"
  #                                                 ) |>
  #       rhandsontable::hot_col(col = colnames(surveyTable), halign = "htCenter") |>
  #       rhandsontable::hot_col(
  #         col = "Site",
  #         readOnly = FALSE,
  #         # type = "character",
  #         source = NULL,
  #         strict = FALSE
  #       ) |>
  #       rhandsontable::hot_col(
  #         col = "Sample",
  #         readOnly = FALSE,
  #         type = "character",
  #         source = NULL,
  #         strict = FALSE
  #       ) |>
  #       rhandsontable::hot_col(
  #         col = "Species",
  #         readOnly = FALSE,
  #         type = "dropdown",
  #         source = c(species_list),
  #         strict = TRUE
  #       ) |>
  #       rhandsontable::hot_col(
  #         col = "Abundance",
  #         readOnly = FALSE,
  #         type = "numeric",
  #         strict = FALSE
  #       ) |>
  #       rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
  #       rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
  #       htmlwidgets::onRender("
  #       function(el, x) {
  #         var hot = this.hot
  #         $('a[data-value=\"basic_inputs_panel\"').on('click', function(){
  #           setTimeout(function() {hot.render();}, 0);
  #         })
  #       }")
  #     
  #     return(surveyTable)
  #     
  #   })
  #   
  #   surveyTable_rval(rhandsontable::hot_to_r(input$surveyTable))
  #   
  # })
  
  observe({
    
    surveyTable_rval(rhandsontable::hot_to_r(input$surveyTable))
    
  }) |>
    bindEvent(input$surveyTable)
    
  
  outputOptions(output, "surveyTable", suspendWhenHidden = FALSE)

}
