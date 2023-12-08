surveyTableWide <- function(input, output, session, surveyTable, sidebar_options) {
  
  ns <- session$ns
  
# Retrieve sidebar options ------------------------------------------------
  # observe({
  #   
  #   
  # }) |>
  #   bindEvent(sidebar_options(), ignoreInit = FALSE)
  
# Prepare surveyTable -----------------------------------------------------
  
  surveyTableWide_rval <- reactiveVal()
  
  observe({
    
    shiny::isolate({
      
      surveyTable <- surveyTable()
      
      # assign(x = "surveyTable", value = surveyTable, envir = .GlobalEnv)
      
      surveyTableWide <- surveyTable |>
        tidyr::unite(col = "ID", c(Year, Group, Quadrat), sep = " - ", remove = FALSE) |>
        dplyr::select(-c(Year, Group, Quadrat)) |>
        dplyr::group_by(ID) |>
        tidyr::pivot_wider(names_from = Species,
                           values_from = Cover) |>
        tibble::column_to_rownames(var = "ID") |>
        dplyr::mutate_all(~replace(., is.na(.), 0)) |>
        as.matrix() 
      
      # assign(x = "surveyTableWide", value = surveyTableWide, envir = .GlobalEnv)
      
    })
    
    surveyTableWide_rval(surveyTableWide)
    
  }) |>
    bindEvent(surveyTable(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  return(surveyTableWide_rval)
  
}
