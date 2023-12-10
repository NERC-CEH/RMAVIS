nvcInfo <- function(input, output, session, sidebar_options) {
  
  ns <- session$ns
  
  # Retrieve sidebar options ------------------------------------------------
  
  # observe({
  #   
  # }) |>
  #   bindEvent(sidebar_options(), ignoreInit = TRUE)
  
  # Initial survey table data -----------------------------------------------
  
  nvcInfoLookupTable_init <- nvc_community_namesCodes
  
  # Survey Data Entry Table -------------------------------------------------
  
  nvcInfoLookupTable_rval <- reactiveVal(nvcInfoLookupTable_init)
  
  output$nvcInfoLookupTable <- rhandsontable::renderRHandsontable({
    
    nvcInfoLookupTable <- rhandsontable::rhandsontable(data = nvcInfoLookupTable_init,
                                                       rowHeaders = NULL#,
                                                       # width = "100%"#,
                                                       # overflow = "visible"
                                                       # stretchH = "all"
                                                       ) |>
      rhandsontable::hot_col(col = colnames(nvcInfoLookupTable_init), halign = "htCenter") |>
      rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all")# |>
      # htmlwidgets::onRender("
      #   function(el, x) {
      #     var hot = this.hot
      #     $('a[data-value=\"nvcInfo\"').on('click', function(){
      #       setTimeout(function() {hot.render();}, 0);
      #     })
      #   }")
    
    return(nvcInfoLookupTable)
    
  })
  
  # observe({
  # 
  #   # shiny::isolate({
  #   # 
  #   # 
  #   # })
  # 
  #   output$nvcInfoLookupTable <- rhandsontable::renderRHandsontable({
  # 
  #     nvcInfoLookupTable <- rhandsontable::rhandsontable(data = nvcInfoLookupTable,
  #                                                        rowHeaders = NULL,
  #                                                        width = "100%"#,
  #                                                        # overflow = "visible",
  #                                                        # stretchH = "all"
  #                                                        ) |>
  #       rhandsontable::hot_col(col = colnames(nvcInfoLookupTable), halign = "htCenter") |>
  #       rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) |>
  #       rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
  #       htmlwidgets::onRender("
  #       function(el, x) {
  #         var hot = this.hot
  #         $('a[data-value=\"surveyData_panel\"').on('click', function(){
  #           setTimeout(function() {hot.render();}, 0);
  #         })
  #       }")
  # 
  #     return(nvcInfoLookupTable)
  # 
  #   })
  # 
  #   nvcInfoLookupTable_rval(rhandsontable::hot_to_r(input$nvcInfoLookupTable))
  # 
  # })
  
  
  outputOptions(output, "nvcInfoLookupTable", suspendWhenHidden = FALSE)
  
  
  return(nvcInfoLookupTable_rval)
  
}