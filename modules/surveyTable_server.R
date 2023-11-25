surveyTable <- function(input, output, session, sidebar_options) {

  ns <- session$ns
  

# Retrieve sidebar options ------------------------------------------------
  
  dataEntryFormat <- reactiveVal()
  runAnalysis <- reactiveVal()
  coverMethod <- reactiveVal()
  habitatRestriction <- reactiveVal()
  nTopResults <- reactiveVal()
  
  observe({
    
    dataEntryFormat(sidebar_options()$dataEntryFormat)
    runAnalysis(sidebar_options()$runAnalysis)
    coverMethod(sidebar_options()$coverMethod)
    habitatRestriction(sidebar_options()$habitatRestriction)
    nTopResults(sidebar_options()$nTopResults)
    
  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)
  
# Survey Data Entry Table -------------------------------------------------
  surveyTable_init <- example_data_df
  
  surveyTable_rval <- reactiveVal(surveyTable_init)
  
  output$surveyTable <- rhandsontable::renderRHandsontable({
    
    surveyTable <- rhandsontable::rhandsontable(data = surveyTable_init,
                                                rowHeaders = NULL,
                                                width = "100%"#,
                                                # overflow = "visible"
                                                # stretchH = "all"
    ) |>
      rhandsontable::hot_col(col = colnames(surveyTable_init), halign = "htCenter") |>
      rhandsontable::hot_col(
        col = "Sample",
        readOnly = FALSE,
        type = "text"
      ) |>
      rhandsontable::hot_col(
        col = "Species",
        readOnly = FALSE,
        type = "dropdown",
        source = speciesNames, # [1:50]
        strict = TRUE,
        default = as.character(NA_character_)
      ) |>
      rhandsontable::hot_col(
        col = "Cover",
        readOnly = FALSE,
        type = "numeric",
        strict = FALSE
      ) |>
      rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all")
    
    return(surveyTable)
    
  })
  
  # observe({
  #   
  #   if(dataEntryFormat() == "table"){
  #     
  #     surveyTable_init <- example_data_df
  #     
  #     output$surveyTable <- rhandsontable::renderRHandsontable({
  #       
  #       surveyTable <- rhandsontable::rhandsontable(data = surveyTable_init,
  #                                                   rowHeaders = NULL,
  #                                                   width = "100%"#,
  #                                                   # overflow = "visible",
  #                                                   # stretchH = "all"
  #       ) |>
  #         rhandsontable::hot_col(col = colnames(surveyTable_init), halign = "htCenter") |>
  #         rhandsontable::hot_col(
  #           col = "Sample",
  #           readOnly = FALSE,
  #           type = "text"
  #         ) |>
  #         rhandsontable::hot_col(
  #           col = "Species",
  #           readOnly = FALSE,
  #           type = "dropdown",
  #           source = speciesNames, # [1:50]
  #           strict = TRUE,
  #           default = as.character(NA_character_)
  #         ) |>
  #         rhandsontable::hot_col(
  #           col = "Cover",
  #           readOnly = FALSE,
  #           type = "numeric",
  #           strict = FALSE
  #         ) |>
  #         rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) |>
  #         rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all")
  #       
  #       return(surveyTable)
  #       
  #     })
  #     
  #     
  #   } else if(dataEntryFormat() == "matrix"){
  #     
  #     surveyTable_init <- example_data_matrix
  #     
  #     output$surveyTable <- rhandsontable::renderRHandsontable({
  #       
  #       surveyTable <- rhandsontable::rhandsontable(data = surveyTable_init,
  #                                                   rowHeaders = NULL,
  #                                                   # useTypes = FALSE,
  #                                                   width = "100%"#,
  #                                                   # overflow = "visible",
  #                                                   # stretchH = "all"
  #       ) |>
  #         # rhandsontable::hot_col(col = colnames(surveyTable_init), halign = "htCenter") |>
  #         # rhandsontable::hot_col(
  #         #   col = "Species",
  #         #   readOnly = FALSE,
  #         #   type = "dropdown",
  #         #   source = speciesNames,
  #         #   strict = TRUE,
  #         #   default = as.character(NA_character_)
  #         # ) |>
  #         rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE) |>
  #         rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all", useTypes = FALSE)
  #       
  #       return(surveyTable)
  #       
  #     })
  #   }
  # }) |>
  #   bindEvent(dataEntryFormat(), ignoreInit = TRUE)
  
  
  observe({

    surveyTable_rval(rhandsontable::hot_to_r(input$surveyTable))

  }) |>
    bindEvent(input$surveyTable, ignoreInit = FALSE)
    
  
  outputOptions(output, "surveyTable", suspendWhenHidden = FALSE)
  
  
  return(surveyTable_rval)

}
