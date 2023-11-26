surveyTablePrepped <- function(input, output, session, surveyTable, sidebar_options) {
  
  ns <- session$ns
  
# Retrieve sidebar options ------------------------------------------------
  groupMethod <- reactiveVal()
  
  observe({
    
    groupMethod(sidebar_options()$groupMethod)
    
  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)
  
# Prepare surveyTable -----------------------------------------------------
  
  surveyTablePrepped_rval <- reactiveVal()
  
  observe({
    
    shiny::isolate({
      
      surveyTable <- surveyTable()
      
      groupMethod_cols <- names(groupMethod_options)[groupMethod_options %in% groupMethod()]
      
      surveyTable_prepped <- surveyTable |>
        tidyr::unite(col = "ID", groupMethod_cols, sep = " - ", remove = TRUE)
      
    })
    
    surveyTablePrepped_rval(surveyTable_prepped)
    
  }) |>
    bindEvent(surveyTable(),
              groupMethod(), 
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  return(surveyTablePrepped_rval)
  
}