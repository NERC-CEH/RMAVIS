results <- function(input, output, session, assignNVC_results) {
  
  ns <- session$ns
  
# Retrieve sidebar options ------------------------------------------------
  # dataEntryFormat <- reactiveVal()
  # runAnalysis <- reactiveVal()
  # coverMethod <- reactiveVal()
  # habitatRestriction <- reactiveVal()
  # nTopResults <- reactiveVal()
  # 
  # observe({
  #   
  #   dataEntryFormat(sidebar_options()$dataEntryFormat)
  #   runAnalysis(sidebar_options()$runAnalysis)
  #   coverMethod(sidebar_options()$coverMethod)
  #   habitatRestriction(sidebar_options()$habitatRestriction)
  #   nTopResults(sidebar_options()$nTopResults)
  #   
  # }) |>
  #   bindEvent(sidebar_options(), ignoreInit = TRUE)
  
  resultsData_init <- tibble::tribble(
    ~Sample, ~Pseudo.Quadrat, ~Jaccard.Similarity, ~NVC.Code,
    "", "", "", ""
  )
  
  resultsTable_rval <- reactiveVal(resultsData_init)
  
  output$resultsTable <- rhandsontable::renderRHandsontable({

    resultsTable <- rhandsontable::rhandsontable(data = resultsData_init,
                                                 rowHeaders = NULL,
                                                 width = "100%"#,
                                                # overflow = "visible",
                                                # stretchH = "all"
                                                ) |>
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all")

    return(resultsTable)

  })
  
  observe({

    req(input$resultsTable)

    # Retrieve the table, optionally modify the table without triggering recursion.
    # isolate({
    # 
    #   resultsTable <- assignNVC_results()
    # 
    # })

    output$resultsTable <- rhandsontable::renderRHandsontable({

      resultsTable <- rhandsontable::rhandsontable(data = assignNVC_results(),
                                                   rowHeaders = NULL#,
                                                   # overflow = "visible",
                                                   # stretchH = "all"
                                                   ) |>
        rhandsontable::hot_col(col = colnames(assignNVC_results()), halign = "htCenter") |>
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all")

      return(resultsTable)

    })

    resultsTable_rval(rhandsontable::hot_to_r(input$resultsTable))

  }) |>
    bindEvent(assignNVC_results(), ignoreInit = TRUE, ignoreNULL = TRUE)


  outputOptions(output, "resultsTable", suspendWhenHidden = FALSE)
  
}
