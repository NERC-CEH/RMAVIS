floristicTables <- function(input, output, session, surveyTable) {
  
  ns <- session$ns
  
  # Retrieve sidebar options ------------------------------------------------
  
  # habCorClass <- reactiveVal()
  # 
  # observe({
  #   
  #   habCorClass(sidebar_options()$habCorClass)
  #   
  # }) |>
  #   bindEvent(sidebar_options(), ignoreInit = TRUE)
  
  floristicTables_composed_init <- tibble::tribble(
    ~NVC.Code, ~Constancy,
    "", ""
  )
  
  floristicTables_nvc_init <- tibble::tribble(
    ~NVC.Code, ~Constancy,
    "", ""
  )
  
  
  floristicTables_composed_rval <- reactiveVal(floristicTables_composed_init)
  
  output$floristicTables_composed <- rhandsontable::renderRHandsontable({
    
    floristicTables_composed <- rhandsontable::rhandsontable(data = floristicTables_composed_init,
                                                             rowHeaders = NULL,
                                                             width = "100%"#,
                                                             # overflow = "visible",
                                                             # stretchH = "all"
    ) |>
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all")
    
    return(floristicTables_composed)
    
  })
  
  observe({
    
    req(input$floristicTables_composed)
    
    # Retrieve the table, optionally modify the table without triggering recursion.
    isolate({
        
      floristicTables_composed <- surveyTable() |> # example_data_df
        dplyr::select(-Cover) |>
        dplyr::mutate("Present" = 1) |>
        tidyr::pivot_wider(values_from = Present,
                           names_from = Sample) |>
        dplyr::rowwise() |>
        dplyr::mutate("Sum" = sum(dplyr::c_across(dplyr::where(is.numeric)), na.rm = TRUE)) |>
        dplyr::mutate("Frequency" = Sum / (ncol(dplyr::pick(dplyr::everything())) - 2)) |>
        dplyr::mutate(
          "Constancy" = 
            dplyr::case_when(
              Frequency <= 0.2 ~ "I",
              Frequency <= 0.4 ~ "II",
              Frequency <= 0.6 ~ "III",
              Frequency <= 0.8 ~ "IV",
              Frequency <= 1.0 ~ "V",
              TRUE ~ as.character(Frequency)
            )
        ) |>
        dplyr::select(Species, Constancy) |>
        dplyr::mutate("Constancy" = factor(Constancy, levels = c("V", "IV", "III", "II", "I"))) |>
        dplyr::arrange(Constancy)

    })
    
    output$floristicTables_composed <- rhandsontable::renderRHandsontable({

      floristicTables_composed <- rhandsontable::rhandsontable(data = floristicTables_composed,
                                                  rowHeaders = NULL#,
                                                  # overflow = "visible",
                                                  # stretchH = "all"
      ) |>
        rhandsontable::hot_col(col = colnames(floristicTables_composed), halign = "htCenter") |>
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all")

      return(floristicTables_composed)

    })
    
    floristicTables_composed_rval(rhandsontable::hot_to_r(input$floristicTables_composed))
    
  }) |>
    bindEvent(surveyTable(), ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  outputOptions(output, "floristicTables_composed", suspendWhenHidden = FALSE)
  
}