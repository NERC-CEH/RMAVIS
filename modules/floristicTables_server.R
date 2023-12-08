floristicTables <- function(input, output, session, surveyTable, sidebar_options) {
  
  ns <- session$ns
  
# Retrieve sidebar options ------------------------------------------------
  
  composedFloristicTable <- reactiveVal()
  nvcFloristicTable <- reactiveVal()
  crossTabulate <- reactiveVal()
  runAnalysis <- reactiveVal()

  observe({

    composedFloristicTable(sidebar_options()$composedFloristicTable)
    nvcFloristicTable(sidebar_options()$nvcFloristicTable)
    crossTabulate(sidebar_options()$crossTabulate)
    runAnalysis(sidebar_options()$runAnalysis)

  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)

# Composed Floristic Tables -----------------------------------------------
  floristicTables_composed_init <- data.frame("Species" = character(),
                                              "Constancy" = character())
  
  output$floristicTables_composed <- rhandsontable::renderRHandsontable({
    
    floristicTables_composed <- rhandsontable::rhandsontable(data = floristicTables_composed_init,
                                                             rowHeaders = NULL,
                                                             width = "100%"#,
                                                             # overflow = "visible",
                                                             # stretchH = "all"
                                                             ) |>
      rhandsontable::hot_col(col = colnames(floristicTables_composed_init), halign = "htCenter", format = "character", readOnly = TRUE) |>
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
  

# Create object containing all composed tables ----------------------------
  floristicTables_composed_all_rval <- reactiveVal()
  
  observe({
    
    shiny::req(input$floristicTables_composed)
    shiny::req(input$floristicTables_nvc)
    shiny::req(surveyTable())

    surveyTable <- surveyTable()

    surveyTable_prepped <- surveyTable |>
      tidyr::unite(col = "ID", c("Year", "Group"), sep = " - ", remove = TRUE)
    
    floristicTables_composed_all <- data.frame("ID" = character(),
                                               "Species" = character(),
                                               "Constancy" = factor())
    
    # assign(x = "surveyTable_prepped", value = surveyTable_prepped, envir = .GlobalEnv)

# Create composed floristic tables across all groups ----------------------
    floristicTables_composed <- surveyTable_prepped |>
      dplyr::filter(!is.na(Cover)) |>
      dplyr::mutate("ID" = stringr::str_extract(string = ID, pattern = "(\\d{4})")) |>
      dplyr::select(-Cover) |>
      dplyr::mutate("Present" = 1) |>
      tidyr::pivot_wider(values_from = Present,
                         names_from = Quadrat) |>
      dplyr::rowwise() |>
      dplyr::mutate("Sum" = sum(dplyr::c_across(dplyr::where(is.numeric)), na.rm = TRUE)) |>
      dplyr::ungroup() |>
      dplyr::mutate("Frequency" = Sum / (ncol(dplyr::pick(dplyr::everything())) - 3)) |> # -2
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
      dplyr::select(ID, Species, Constancy) |>
      dplyr::mutate("Constancy" = factor(Constancy, levels = c("V", "IV", "III", "II", "I"))) |>
      dplyr::arrange(ID, Constancy, Species)

    floristicTables_composed_all <- floristicTables_composed_all |>
      dplyr::bind_rows(floristicTables_composed)

    

# Create composed floristic tables for groups -----------------------------
    for(id in unique(surveyTable_prepped$ID)){
      
      floristicTables_composed <- surveyTable_prepped |>
        dplyr::filter(ID == id) |>
        dplyr::filter(!is.na(Cover)) |>
        dplyr::select(-Cover) |>
        dplyr::mutate("Present" = 1) |>
        tidyr::pivot_wider(values_from = Present,
                           names_from = Quadrat) |>
        dplyr::rowwise() |>
        dplyr::mutate("Sum" = sum(dplyr::c_across(dplyr::where(is.numeric)), na.rm = TRUE)) |>
        dplyr::ungroup() |>
        dplyr::mutate("Frequency" = Sum / (ncol(dplyr::pick(dplyr::everything())) - 3)) |> # -2
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
        dplyr::select(ID, Species, Constancy) |>
        dplyr::mutate("Constancy" = factor(Constancy, levels = c("V", "IV", "III", "II", "I"))) |>
        dplyr::arrange(ID, Constancy, Species)
      
      floristicTables_composed_all <- floristicTables_composed_all |>
        dplyr::bind_rows(floristicTables_composed)
      
      # print(floristicTables_composed)
      
    }
    
    floristicTables_composed_all_rval(floristicTables_composed_all)
    
    # assign(x = "floristicTables_composed_all", value = floristicTables_composed_all, envir = .GlobalEnv)

  }) |>
    bindEvent(runAnalysis(),
              ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  
  
  observe({
    
    shiny::req(floristicTables_composed_all_rval())
    shiny::req(!is.null(composedFloristicTable()))
    
    # Retrieve the table, optionally modify the table without triggering recursion.
    shiny::isolate({
      
      floristicTables_composed_all <- floristicTables_composed_all_rval()
      
      floristicTables_composed_selected <- floristicTables_composed_all |>
        dplyr::filter(ID == composedFloristicTable()) |>
        dplyr::select(-ID)
      
      floristicTables_nvc <- nvc_floristic_tables |>
        dplyr::filter(NVC.Code == nvcFloristicTable()) |>
        dplyr::select(-NVC.Code) |>
        dplyr::mutate("Constancy" = factor(Constancy, levels = c("V", "IV", "III", "II", "I"))) |>
        dplyr::arrange(Constancy, Species)
      
      floristicTables_composed_compToNVC <- floristicTables_nvc |>
        dplyr::select(-Constancy) |>
        dplyr::left_join(floristicTables_composed_selected, by = "Species") |>
        dplyr::mutate(
          "Species" = 
            dplyr::case_when(
              is.na(Constancy) ~ "",
              TRUE ~ as.character(Species)
            )
        )
      
      if(crossTabulate() == "No"){
        
        floristicTables_composed_view <- floristicTables_composed_selected
        
      } else if(crossTabulate() == "compToNVC"){
        
        floristicTables_composed_view <- floristicTables_composed_compToNVC
        
      } else if(crossTabulate() == "NVCToComp"){
        
        floristicTables_composed_view <- floristicTables_composed_selected
        
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
        rhandsontable::hot_col(col = colnames(floristicTables_composed_view), halign = "htCenter", format = "character", readOnly = TRUE) |> # renderer = row_renderer
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
    
  }) |>
    bindEvent(floristicTables_composed_all_rval(), 
              crossTabulate(),
              nvcFloristicTable(),
              composedFloristicTable(),
              ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  outputOptions(output, "floristicTables_composed", suspendWhenHidden = FALSE)
  
  
  

# NVC Floristic Tables ----------------------------------------------------

  floristicTables_nvc_init <- data.frame("Species" = character(),
                                         "Constancy" = character())
  
  output$floristicTables_nvc <- rhandsontable::renderRHandsontable({
    
    floristicTables_nvc <- rhandsontable::rhandsontable(data = floristicTables_nvc_init,
                                                        rowHeaders = NULL,
                                                        width = "100%"#,
                                                        # overflow = "visible",
                                                        # stretchH = "all"
                                                        ) |>
      rhandsontable::hot_col(col = colnames(floristicTables_nvc_init), halign = "htCenter", format = "character", readOnly = TRUE) |>
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
    
    shiny::req(input$floristicTables_nvc)
    shiny::req(input$floristicTables_composed)
    shiny::req(!is.null(composedFloristicTable()))
    
    # Retrieve the table, optionally modify the table without triggering recursion.
    shiny::isolate({
      
      floristicTables_composed_all <- floristicTables_composed_all_rval()
      
      floristicTables_composed_selected <- floristicTables_composed_all |>
        dplyr::filter(ID == composedFloristicTable()) |>
        dplyr::select(-ID)
      
      floristicTables_nvc <- nvc_floristic_tables |>
        dplyr::filter(NVC.Code == nvcFloristicTable()) |>
        dplyr::select(-NVC.Code) |>
        dplyr::mutate("Constancy" = factor(Constancy, levels = c("V", "IV", "III", "II", "I"))) |>
        dplyr::arrange(Constancy, Species)

      floristicTables_nvc_NVCToComp <- floristicTables_composed_selected |>
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
        rhandsontable::hot_col(col = colnames(floristicTables_nvc_view), halign = "htCenter", format = "character", readOnly = TRUE) |>
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
    
  }) |>
    bindEvent(floristicTables_composed_all_rval(),
              nvcFloristicTable(), 
              crossTabulate(), 
              composedFloristicTable(),
              ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  outputOptions(output, "floristicTables_nvc", suspendWhenHidden = FALSE)
  
  return(floristicTables_composed_all_rval)
  
}