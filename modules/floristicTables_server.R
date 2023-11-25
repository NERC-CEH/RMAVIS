floristicTables <- function(input, output, session, surveyTable, sidebar_options) {
  
  ns <- session$ns
  
# Retrieve sidebar options ------------------------------------------------
  
  # nvcFloristicTable <- reactiveVal("A1") # "A1"
  # crossTabulate <- reactiveVal(FALSE) # FALSE
  nvcFloristicTable <- reactiveVal()
  crossTabulate <- reactiveVal()

  observe({

    nvcFloristicTable(sidebar_options()$nvcFloristicTable)
    crossTabulate(sidebar_options()$crossTabulate)

  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)

# Composed Floristic Tables -----------------------------------------------

  floristicTables_composed_init <- data.frame("Species" = character(),
                                              "Constancy" = character())
  
  floristicTables_composed_rval <- reactiveVal(floristicTables_composed_init)
  
  output$floristicTables_composed <- rhandsontable::renderRHandsontable({
    
    floristicTables_composed <- rhandsontable::rhandsontable(data = floristicTables_composed_init,
                                                             rowHeaders = NULL,
                                                             width = "100%"#,
                                                             # overflow = "visible",
                                                             # stretchH = "all"
                                                             ) |>
      rhandsontable::hot_col(col = colnames(floristicTables_composed_init), halign = "htCenter", readOnly = TRUE) |>
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
      htmlwidgets::onRender("
      function(el, x) {
        var hot = this.hot
        $('a[data-value=\"floristicTables_panel\"').on('click', function(){
          setTimeout(function() {hot.render();}, 0);
        })
      }")
    
    return(floristicTables_composed)
    
  })
  
  observe({
    
    req(input$floristicTables_composed)
    req(input$floristicTables_nvc)
    
    # Require the survey table to not be empty
    req(nrow(surveyTable()) > 0) 
    # Require the run analysis button to have been clicked
    # req(isFALSE(runAnalysis() == 0))
    
    # Retrieve the table, optionally modify the table without triggering recursion.
    shiny::isolate({
        
      floristicTables_composed <- surveyTable() |> # example_data_df
        dplyr::select(-Cover) |>
        dplyr::mutate("Present" = 1) |>
        tidyr::pivot_wider(values_from = Present,
                           names_from = Sample) |>
        dplyr::rowwise() |>
        dplyr::mutate("Sum" = sum(dplyr::c_across(dplyr::where(is.numeric)), na.rm = TRUE)) |>
        dplyr::ungroup() |>
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
        dplyr::arrange(Constancy, Species)
      
      # floristicTables_nvc <- rhandsontable::hot_to_r(input$floristicTables_nvc)
      
      floristicTables_nvc <- nvc_floristic_tables |>
        dplyr::filter(NVC.Code == nvcFloristicTable()) |>
        dplyr::select(-NVC.Code) |>
        dplyr::mutate("Constancy" = factor(Constancy, levels = c("V", "IV", "III", "II", "I"))) |>
        dplyr::arrange(Constancy, Species)
      
      floristicTables_composed_compToNVC <- floristicTables_nvc |> # floristicTables_nvc_rval()
        dplyr::select(-Constancy) |>
        dplyr::left_join(floristicTables_composed, by = "Species") |>
        dplyr::mutate(
          "Species" = 
            dplyr::case_when(
              is.na(Constancy) ~ "",
              TRUE ~ as.character(Species)
            )
        )
      
      if(crossTabulate() == "No"){
        
        floristicTables_composed_view <- floristicTables_composed
        
      } else if(crossTabulate() == "compToNVC"){
        
        floristicTables_composed_view <- floristicTables_composed_compToNVC
        
      } else if(crossTabulate() == "NVCToComp"){
        
        floristicTables_composed_view <- floristicTables_composed
        
      }

    })
    
    # Create renderer to highlight rows if Constancy is empty
    # row_renderer <- "
    # function (instance, td, row, col, prop, value, cellproperties) {
    #   handsontable.renderers.textrenderer.apply(this, arguments);
    #   var col_value = instance.getdata()[row][1]
    #   if (col_value == '<NA>') {
    #     td.style.background = 'pink';
    #   }
    # }"
    
    output$floristicTables_composed <- rhandsontable::renderRHandsontable({

      floristicTables_composed <- rhandsontable::rhandsontable(data = floristicTables_composed_view,
                                                  rowHeaders = NULL#,
                                                  # overflow = "visible",
                                                  # stretchH = "all"
                                                  ) |>
        rhandsontable::hot_col(col = colnames(floristicTables_composed), halign = "htCenter", readOnly = TRUE) |> # renderer = row_renderer
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
        htmlwidgets::onRender("
        # function(el, x) {
        #   var hot = this.hot
        #   $('a[data-value=\"floristicTables_panel\"').on('click', function(){
        #     setTimeout(function() {hot.render();}, 0);
        #   })
        # }")

      return(floristicTables_composed)

    })
    
    floristicTables_composed_rval(rhandsontable::hot_to_r(input$floristicTables_composed))
    
  }) |>
    bindEvent(surveyTable(), 
              crossTabulate(), 
              # floristicTables_nvc_rval(),
              input$floristicTables_nvc,
              ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  outputOptions(output, "floristicTables_composed", suspendWhenHidden = FALSE)
  
  
  

# NVC Floristic Tables ----------------------------------------------------

  floristicTables_nvc_init <- data.frame("Species" = character(),
                                         "Constancy" = character())
  
  floristicTables_nvc_rval <- reactiveVal(floristicTables_nvc_init)
  
  output$floristicTables_nvc <- rhandsontable::renderRHandsontable({
    
    floristicTables_nvc <- rhandsontable::rhandsontable(data = floristicTables_nvc_init,
                                                        rowHeaders = NULL,
                                                        width = "100%"#,
                                                        # overflow = "visible",
                                                        # stretchH = "all"
                                                        ) |>
      rhandsontable::hot_col(col = colnames(floristicTables_nvc_init), halign = "htCenter", readOnly = TRUE) |>
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
      htmlwidgets::onRender("
      function(el, x) {
        var hot = this.hot
        $('a[data-value=\"floristicTables_panel\"').on('click', function(){
          setTimeout(function() {hot.render();}, 0);
        })
      }")
    
    return(floristicTables_nvc)
    
  })
  
  observe({
    
    req(input$floristicTables_nvc)
    req(input$floristicTables_composed)
    
    # print(nvcFloristicTable())
    # print(crossTabulate())
    
    # Require the survey table to not be empty
    req(nrow(surveyTable()) > 0) 
    # Require the run analysis button to have been clicked
    # req(isFALSE(runAnalysis() == 0))
    
    # Retrieve the table, optionally modify the table without triggering recursion.
    shiny::isolate({
      
      floristicTables_nvc <- nvc_floristic_tables |>
        dplyr::filter(NVC.Code == nvcFloristicTable()) |>
        dplyr::select(-NVC.Code) |>
        dplyr::mutate("Constancy" = factor(Constancy, levels = c("V", "IV", "III", "II", "I"))) |>
        dplyr::arrange(Constancy, Species)

      # floristicTables_composed <- rhandsontable::hot_to_r(input$floristicTables_composed)
      
      floristicTables_composed <- surveyTable() |> # example_data_df
        dplyr::select(-Cover) |>
        dplyr::mutate("Present" = 1) |>
        tidyr::pivot_wider(values_from = Present,
                           names_from = Sample) |>
        dplyr::rowwise() |>
        dplyr::mutate("Sum" = sum(dplyr::c_across(dplyr::where(is.numeric)), na.rm = TRUE)) |>
        dplyr::ungroup() |>
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
        dplyr::arrange(Constancy, Species)

      floristicTables_nvc_NVCToComp <- floristicTables_composed |>
        dplyr::select(-Constancy) |>
        dplyr::left_join(floristicTables_nvc, by = "Species") |>
        dplyr::mutate(
          "Species" =
            dplyr::case_when(
              is.na(Constancy) ~ "",
              TRUE ~ as.character(Species)
            )
        )
      
      if(crossTabulate() == "No"){
        
        floristicTables_nvc_view <- floristicTables_nvc
        
      } else if(crossTabulate() == "compToNVC"){
        
        floristicTables_nvc_view <- floristicTables_nvc
        
      } else if(crossTabulate() == "NVCToComp"){
        
        floristicTables_nvc_view <- floristicTables_nvc_NVCToComp
        
      }
      
    })
    
    output$floristicTables_nvc <- rhandsontable::renderRHandsontable({
      
      floristicTables_nvc <- rhandsontable::rhandsontable(data = floristicTables_nvc_view,
                                                          rowHeaders = NULL#,
                                                          # overflow = "visible",
                                                          # stretchH = "all"
      ) |>
        rhandsontable::hot_col(col = colnames(floristicTables_nvc), halign = "htCenter", readOnly = TRUE) |>
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
        htmlwidgets::onRender("
        function(el, x) {
          var hot = this.hot
          $('a[data-value=\"floristicTables_panel\"').on('click', function(){
            setTimeout(function() {hot.render();}, 0);
          })
        }")
      
      return(floristicTables_nvc)
      
    })
    
    floristicTables_nvc_rval(rhandsontable::hot_to_r(input$floristicTables_nvc))
    
  }) |>
    bindEvent(#surveyTable(),
              nvcFloristicTable(), 
              crossTabulate(), 
              # floristicTables_composed_rval(),
              input$floristicTables_composed,
              ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  outputOptions(output, "floristicTables_nvc", suspendWhenHidden = FALSE)
  
}