surveyTableWide <- function(input, output, session, surveyTable, sidebar_options) {
  
  ns <- session$ns
  
# Retrieve sidebar options ------------------------------------------------
  
  runAnalysis <- reactiveVal()
  
  observe({

    runAnalysis(sidebar_options()$runAnalysis)

  }) |>
    bindEvent(sidebar_options(), ignoreInit = FALSE)
  
# Prepare surveyTable -----------------------------------------------------
  
  surveyTableWide_rval <- reactiveVal()
  
  observe({
    
    shiny::isolate({
      
      req(all(!is.na(surveyTable()$Species)) == TRUE)
      
      surveyTable <- surveyTable()
      
      noCoverValues <- isTRUE(surveyTable$Cover |> unique() |> is.na())
      
      if(noCoverValues == TRUE){
        
        surveyTableWide <- surveyTable |>
          tidyr::unite(col = "ID", c(Year, Group, Quadrat), sep = " - ", remove = FALSE) |>
          dplyr::mutate("Cover" = 1) |>
          dplyr::select(-c(Year, Group, Quadrat)) |>
          dplyr::group_by(ID) |>
          tidyr::pivot_wider(names_from = Species,
                             values_from = Cover) |>
          tibble::column_to_rownames(var = "ID") |>
          dplyr::mutate_all(~replace(., is.na(.), 0)) |>
          as.matrix() 
        
      } else if(noCoverValues == FALSE){
        
        surveyTableWide <- surveyTable |>
          tidyr::unite(col = "ID", c(Year, Group, Quadrat), sep = " - ", remove = FALSE) |>
          dplyr::select(-c(Year, Group, Quadrat)) |>
          dplyr::group_by(ID) |>
          tidyr::pivot_wider(names_from = Species,
                             values_from = Cover) |>
          tibble::column_to_rownames(var = "ID") |>
          dplyr::mutate_all(~replace(., is.na(.), 0)) |>
          as.matrix() 
        
      }
      
    })
    
    # print(surveyTableWide)
    
    surveyTableWide_rval(surveyTableWide)
    
  }) |>
    bindEvent(runAnalysis(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  return(surveyTableWide_rval)
  
}
