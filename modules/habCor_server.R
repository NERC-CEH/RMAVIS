habCor <- function(input, output, session, assignNVC_results, sidebar_options) {
  
  ns <- session$ns
  
  # Retrieve sidebar options ------------------------------------------------
  
  habCorClass <- reactiveVal()

  observe({
    
    habCorClass(sidebar_options()$habCorClass)

  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)
  
  habCorData_init <- tibble::tribble(
    ~NVC.Code, ~Relationship, ~Code, ~Label,
    "", "", "", ""
  )
  
  habCorTable_rval <- reactiveVal(habCorData_init)
  
  output$habCorTable <- rhandsontable::renderRHandsontable({
    
    habCorTable <- rhandsontable::rhandsontable(data = habCorData_init,
                                                rowHeaders = NULL,
                                                width = "100%"#,
                                                # overflow = "visible",
                                                # stretchH = "all"
    ) |>
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all")
    
    return(habCorTable)
    
  })
  
  observe({
    
    req(input$habCorTable)
    
    # Retrieve the table, optionally modify the table without triggering recursion.
    isolate({

      habCorTable <- assignNVC_results() |>
        dplyr::select(NVC.Code) |>
        dplyr::left_join(all_habCor_final, relationship = "many-to-many", by = dplyr::join_by(NVC.Code)) |>
        dplyr::filter(Classification == habCorClass()) |>
        dplyr::select(NVC.Code, Relationship, Code, Label) |>
        dplyr::distinct() |>
        dplyr::arrange(NVC.Code)

    })
    
    output$habCorTable <- rhandsontable::renderRHandsontable({
      
      habCorTable <- rhandsontable::rhandsontable(data = habCorTable,
                                                  rowHeaders = NULL#,
                                                  # overflow = "visible",
                                                  # stretchH = "all"
      ) |>
        rhandsontable::hot_col(col = colnames(habCorTable), halign = "htCenter") |>
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all")
      
      return(habCorTable)
      
    })
    
    habCorTable_rval(rhandsontable::hot_to_r(input$habCorTable))
    
  }) |>
    bindEvent(assignNVC_results(), habCorClass(), ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  outputOptions(output, "habCorTable", suspendWhenHidden = FALSE)
  
}