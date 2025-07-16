floristicTables <- function(input, output, session, setupData, surveyData, deSidebar_options, sidebar_options) {
  
  ns <- session$ns
  
# Retrieve sidebar options ------------------------------------------------
  coverScale <- reactiveVal()
  removeLowFreqTaxa <- reactiveVal()
  floristicTablesView <- reactiveVal()
  floristicTablesSetView <- reactiveVal()
  composedFloristicTable <- reactiveVal()
  nvcFloristicTable <- reactiveVal()
  matchSpecies <- reactiveVal()
  runAnalysis <- reactiveVal()

  observe({

    coverScale(deSidebar_options()$coverScale)
    removeLowFreqTaxa(sidebar_options()$removeLowFreqTaxa)
    floristicTablesView(sidebar_options()$floristicTablesView)
    floristicTablesSetView(sidebar_options()$floristicTablesSetView)
    composedFloristicTable(sidebar_options()$composedFloristicTable)
    nvcFloristicTable(sidebar_options()$nvcFloristicTable)
    matchSpecies(sidebar_options()$matchSpecies)
    runAnalysis(sidebar_options()$runAnalysis)

  }) |>
    bindEvent(sidebar_options(), 
              deSidebar_options(),
              ignoreInit = TRUE)
  
  
# Retrieve Setup Data -----------------------------------------------------
  floristic_tables <- reactiveVal()
  community_attributes <- reactiveVal()
  
  observe({
    
    shiny::isolate({
      setupData <- setupData()
    })
    
    floristic_tables(setupData$floristic_tables)
    community_attributes(setupData$community_attributes)
    
  }) |>
    bindEvent(runAnalysis(),
              ignoreInit = FALSE)
  

# Create object containing all composed tables ----------------------------
  floristicTables_composed_all_rval <- reactiveVal()
  floristicTables_composed_all_wide_rval <- reactiveVal()
  floristicTables <- reactiveVal()
  
  observe({
    
    shiny::req(surveyData())

    shiny::isolate({
      coverScale <- coverScale()
      surveyData <- surveyData()
      removeLowFreqTaxa <- removeLowFreqTaxa()
    })
    
    surveyData_long <- surveyData$surveyData_long
    
    floristicTables_composed_all <- data.frame("ID" = character(),
                                               "Species" = character(),
                                               "Constancy" = factor())
    
    if(removeLowFreqTaxa == TRUE){
      removeLowFreqTaxa_value <- 0.05
    } else if(removeLowFreqTaxa == FALSE){
      removeLowFreqTaxa_value <- NULL
    }
    
    ## Compose sample floristic tables -----------------------------------------
    floristicTables_composed_year_group <- RMAVIS::composeSyntopicTables(surveyData = surveyData_long, 
                                                                         group_cols = c("Year", "Group"), 
                                                                         species_col_name = "Species", 
                                                                         plot_col_name = "Quadrat",
                                                                         cover_col_name = "Cover",
                                                                         numeral_constancy = TRUE,
                                                                         remove_low_freq_taxa = removeLowFreqTaxa_value)
    
    floristicTables_composed_year <- RMAVIS::composeSyntopicTables(surveyData = surveyData_long, 
                                                                   group_cols = c("Year"), 
                                                                   species_col_name = "Species", 
                                                                   plot_col_name = "Quadrat",
                                                                   cover_col_name = "Cover",
                                                                   numeral_constancy = TRUE,
                                                                   remove_low_freq_taxa = removeLowFreqTaxa_value)
    
    floristicTables_composed_all <- rbind(floristicTables_composed_year, floristicTables_composed_year_group)
    
    ## Prepare cover summary ---------------------------------------------------
    if(coverScale == "domin"){
      
      floristicTables_composed_all <- floristicTables_composed_all |>
        dplyr::mutate("Cover" = paste0(Min.Cover, 
                                       " - ", 
                                       ifelse(Mean.Cover != "+", round(as.numeric(Mean.Cover)), Mean.Cover),
                                       " - ", 
                                       Max.Cover), .keep = "unused")
      
    } else if(coverScale == "percentage"){
      
      floristicTables_composed_all <- floristicTables_composed_all |>
        dplyr::mutate_at(dplyr::vars(Min.Cover, Mean.Cover, Max.Cover),
                         list(
                           ~dplyr::case_when(
                             . > 91 ~ "10",
                             . > 76 ~ "9",
                             . > 51 ~ "8",
                             . > 34 ~ "7",
                             . > 26 ~ "6",
                             . > 11 ~ "5",
                             . > 4 ~ "4",
                             . > 3 ~ "3",
                             . > 2 ~ "2",
                             . > 1 ~ "1",
                             . > 0 ~ "+",
                             TRUE ~ NA
                           )
                         )
                         ) |>
        dplyr::mutate("Cover" = paste0(Min.Cover, 
                                       " - ", 
                                       ifelse(Mean.Cover != "+", round(as.numeric(Mean.Cover)), Mean.Cover),
                                       " - ", 
                                       Max.Cover), .keep = "unused")
      
    } else if(coverScale == "proportional"){
      
      floristicTables_composed_all <- floristicTables_composed_all |>
        dplyr::mutate_at(dplyr::vars(Min.Cover, Mean.Cover, Max.Cover),
                         list(
                           ~dplyr::case_when(
                             . > 0.91 ~ "10",
                             . > 0.76 ~ "9",
                             . > 0.51 ~ "8",
                             . > 0.34 ~ "7",
                             . > 0.26 ~ "6",
                             . > 0.11 ~ "5",
                             . > 0.4 ~ "4",
                             . > 0.3 ~ "3",
                             . > 0.2 ~ "2",
                             . > 0.1 ~ "1",
                             . > 0 ~ "+",
                             TRUE ~ NA
                           )
                         )
        ) |>
        dplyr::mutate("Cover" = paste0(Min.Cover, 
                                       " - ", 
                                       ifelse(Mean.Cover != "+", round(as.numeric(Mean.Cover)), Mean.Cover),
                                       " - ", 
                                       Max.Cover), .keep = "unused")
      
      
    } else if(coverScale %in% c("none", "braunBlanquet")){
      
      floristicTables_composed_all <- floristicTables_composed_all |>
        dplyr::mutate("Cover" = NA) |>
        dplyr::select(-c(Min.Cover, Mean.Cover, Max.Cover))
      
    }
    
    floristicTables_composed_all_rval(floristicTables_composed_all)

    ## Create wide composed floristic tables -----------------------------------
    floristicTables_composed_all_wide <- floristicTables_composed_all |>
      dplyr::mutate("Year" = stringr::str_extract(string = ID, pattern = "\\d{4}")) |>
      dplyr::mutate("Group" = stringr::str_extract(string = ID, pattern = "(?<=\\s-\\s).*$"))
    
    floristicTables_composed_all_wide_year <- floristicTables_composed_all_wide |>
      dplyr::filter(is.na(Group)) |>
      tidyr::pivot_wider(id_cols = c(Species), names_from = Year, values_from = Constancy) |>
      dplyr::mutate("Group" = "All", .before = Species)
    
    floristicTables_composed_all_wide_yearGroup <- floristicTables_composed_all_wide |>
      dplyr::filter(!is.na(Group)) |>
      dplyr::arrange(Group) |>
      tidyr::pivot_wider(id_cols = c(Group, Species), names_from = Year, values_from = Constancy)
    
    floristicTables_composed_all_wide_all <- rbind(floristicTables_composed_all_wide_year,
                                                   floristicTables_composed_all_wide_yearGroup)
    
    
    floristicTables_composed_all_wide_rval(floristicTables_composed_all_wide_all)
    

    ## Create sample community attributes --------------------------------------
    sample_community_attributes_year <- surveyData_long |>
      tidyr::unite(col = "ID", Year, sep = " - ", remove = FALSE) |>
      dplyr::right_join(floristicTables_composed_year |> dplyr::select(ID, Species), by = c("ID", "Species")) |>
      dplyr::select(-ID) |>
      dplyr::group_by(Year, Quadrat) |>
      dplyr::mutate("species_per_quadrat" = dplyr::n_distinct(Species)) |>
      dplyr::group_by(Year) |>
      dplyr::summarise("number_quadrats" = dplyr::n_distinct(Quadrat),
                       "total_species" = dplyr::n_distinct(Species),
                       "min_species" = min(species_per_quadrat),
                       "mean_species" = mean(species_per_quadrat),
                       "max_species" = max(species_per_quadrat)) |>
      dplyr::ungroup() |>
      tidyr::unite(col = "ID", Year, sep = " - ", remove = TRUE)
    
    sample_community_attributes_group <- surveyData_long |>
      tidyr::unite(col = "ID", Year, Group, sep = " - ", remove = FALSE) |>
      dplyr::right_join(floristicTables_composed_year_group |> dplyr::select(ID, Species), by = c("ID", "Species")) |>
      dplyr::select(-ID) |>
      dplyr::group_by(Year, Group, Quadrat) |>
      dplyr::mutate("species_per_quadrat" = dplyr::n_distinct(Species)) |>
      dplyr::group_by(Year, Group) |>
      dplyr::summarise("number_quadrats" = dplyr::n_distinct(Quadrat),
                       "total_species" = dplyr::n_distinct(Species),
                       "min_species" = min(species_per_quadrat),
                       "mean_species" = mean(species_per_quadrat),
                       "max_species" = max(species_per_quadrat)) |>
      dplyr::ungroup() |>
      tidyr::unite(col = "ID", Year, Group, sep = " - ", remove = TRUE)
    
    sample_community_attributes <- dplyr::bind_rows(sample_community_attributes_group, 
                                                    sample_community_attributes_year)

    ## Compose Object to Return From Module ------------------------------------
    floristicTables(list("floristicTables_composed_all" = floristicTables_composed_all_rval(),
                         "floristicTables_composed_all_wide" = floristicTables_composed_all_wide_rval(),
                         "sample_community_attributes" = sample_community_attributes))

  }) |>
    bindEvent(runAnalysis(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)

  # Create Composed Floristic Reactable Table ----------------------------------
  
  ## Initialise ----------------------------------------------------------------
  floristicTables_composed_init <- data.frame("Species" = character(),
                                              "Constancy" = character(),
                                              "Cover" = character())
  
  output$floristicTables_composed <- reactable::renderReactable({
    
    floristicTables_composed <-  reactable::reactable(data = floristicTables_composed_init,
                                                      filterable = FALSE,
                                                      pagination = FALSE, 
                                                      highlight = TRUE,
                                                      bordered = TRUE,
                                                      sortable = FALSE, 
                                                      wrap = FALSE,
                                                      resizable = TRUE,
                                                      class = "my-tbl",
                                                      # style = list(fontSize = "1rem"),
                                                      rowClass = "my-row",
                                                      defaultColDef = reactable::colDef(
                                                        headerClass = "my-header",
                                                        class = "my-col",
                                                        align = "center" # Needed as alignment is not passing through to header
                                                      )
    )
    
    return(floristicTables_composed)
    
  })

  ## Update --------------------------------------------------------------------
  observe({
    
    shiny::req(floristicTables_composed_all_rval())
    shiny::req(!is.null(composedFloristicTable()))
    shiny::req(!is.null(nvcFloristicTable()))
    shiny::req(nvcFloristicTable() != "")
    
    # Retrieve the table, optionally modify the table without triggering recursion.
    shiny::isolate({
      
      floristicTables_composed_all <- floristicTables_composed_all_rval()
      composedFloristicTable_name <- composedFloristicTable()
      nvcFloristicTable_name <- nvcFloristicTable()
      floristic_tables <- floristic_tables()
      
    })
    
    floristicTables_composed_selected <- floristicTables_composed_all |>
      dplyr::filter(ID == composedFloristicTable_name) |>
      dplyr::select(-ID)
    
    floristicTables_nvc <- floristic_tables |>
      dplyr::filter(nvc_code == nvcFloristicTable_name) |>
      dplyr::select("Species" = "nvc_taxon_name", "Constancy" = "constancy") |>
      dplyr::mutate(
        "Constancy" = 
          dplyr::case_when(
            Constancy == 1 ~ "I",
            Constancy == 2 ~ "II",
            Constancy == 3 ~ "III",
            Constancy == 4 ~ "IV",
            Constancy == 5 ~ "V",
            TRUE ~ NA
          )
      ) |>
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
    
    if(matchSpecies() == "No"){
      
      floristicTables_composed_view <- floristicTables_composed_selected
      
    } else if(matchSpecies() == "compToNVC"){
      
      floristicTables_composed_view <- floristicTables_composed_compToNVC
      
    } else if(matchSpecies() == "NVCToComp"){
      
      floristicTables_composed_view <- floristicTables_composed_selected
      
    }
    
    output$floristicTables_composed <- reactable::renderReactable({

      floristicTables_composed <- reactable::reactable(data = floristicTables_composed_view, 
                                                       filterable = FALSE,
                                                       pagination = FALSE, 
                                                       highlight = TRUE,
                                                       bordered = TRUE,
                                                       sortable = FALSE, 
                                                       wrap = FALSE,
                                                       resizable = TRUE,
                                                       class = "my-tbl",
                                                       # style = list(fontSize = "1rem"),
                                                       rowClass = "my-row",
                                                       defaultColDef = reactable::colDef(
                                                         headerClass = "my-header",
                                                         class = "my-col",
                                                         align = "center" # Needed as alignment is not passing through to header
                                                       )
                                                       )

      return(floristicTables_composed)
      
      

    })
    
  }) |>
    bindEvent(floristicTables_composed_all_rval(), 
              matchSpecies(),
              nvcFloristicTable(),
              composedFloristicTable(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  
  outputOptions(output, "floristicTables_composed", suspendWhenHidden = FALSE)
  
  ## Title ---------------------------------------------------------------------
  composedFloristicTableTitle_rval <- reactiveVal(paste("<font size=4.75>",
                                                        "Composed Floristic Table ",
                                                        "</font>") 
  )
  
  observe({
    
    shiny::req(composedFloristicTable())
    
    shiny::isolate({
      samp_floristicTable_name <- composedFloristicTable()
      samp_floristicTables <- floristicTables()
    })
    
    # assign(x = "samp_floristicTable_name", value = samp_floristicTable_name, envir = .GlobalEnv)
    # assign(x = "samp_floristicTables", value = samp_floristicTables, envir = .GlobalEnv)
    
    samp_floristicTable <- samp_floristicTables$sample_community_attributes |>
      dplyr::filter(ID == samp_floristicTable_name)
    
    composedFloristicTableTitle <- paste0("<font size=4.75>",
                                          samp_floristicTable_name,
                                          "<br>",
                                          "Quadrats: ",
                                          samp_floristicTable$number_quadrats,
                                          "<br>",
                                          "Species: ",
                                          samp_floristicTable$total_species,
                                          " (min = ",
                                          samp_floristicTable$min_species,
                                          ", mean = ",
                                          round(samp_floristicTable$mean_species),
                                          ", max = ",
                                          samp_floristicTable$max_species,
                                          ")",
                                          "</font>")
    
    composedFloristicTableTitle_rval(composedFloristicTableTitle)
    
  }) |>
    bindEvent(floristicTables(),
              composedFloristicTable(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  output$composedFloristicTableTitle <- renderText({ 
    
    composedFloristicTableTitle <- composedFloristicTableTitle_rval()
    
    paste(composedFloristicTableTitle) 
    
  })

  # NVC Floristic Tables -------------------------------------------------------

  ## Intialise -----------------------------------------------------------------
  floristicTables_nvc_init <- data.frame("Species" = character(),
                                         "Constancy" = character(),
                                         "Cover" = character())
  
  output$floristicTables_nvc <- reactable::renderReactable({
    
    floristicTables_nvc <- reactable::reactable(data = floristicTables_nvc_init,
                                                filterable = FALSE,
                                                pagination = FALSE, 
                                                highlight = TRUE,
                                                bordered = TRUE,
                                                sortable = FALSE, 
                                                wrap = FALSE,
                                                resizable = TRUE,
                                                class = "my-tbl",
                                                # style = list(fontSize = "1rem"),
                                                rowClass = "my-row",
                                                defaultColDef = reactable::colDef(
                                                  headerClass = "my-header",
                                                  class = "my-col",
                                                  align = "center" # Needed as alignment is not passing through to header
                                                )
                                                )
    
    return(floristicTables_nvc)
    
  })
  

  ## Update --------------------------------------------------------------------
  observe({
    
    shiny::req(!is.null(composedFloristicTable()))
    shiny::req(!is.null(nvcFloristicTable()))
    shiny::req(nvcFloristicTable() != "")
    
    # Retrieve the table, optionally modify the table without triggering recursion.
    shiny::isolate({
      
      floristicTables_composed_all <- floristicTables_composed_all_rval()
      composedFloristicTable <- composedFloristicTable()
      nvcFloristicTable <- nvcFloristicTable()
      floristic_tables <- floristic_tables()
      
    })
      
    floristicTables_composed_selected <- floristicTables_composed_all |>
      dplyr::filter(ID == composedFloristicTable) |>
      dplyr::select(-ID)
    
    floristicTables_nvc <- floristic_tables |>
      dplyr::filter(nvc_code == nvcFloristicTable) |>
      dplyr::select("Species" = "nvc_taxon_name", "Constancy" = "constancy",
                    minimum_cover, mean_cover, maximum_cover) |>
      dplyr::mutate(
        "Constancy" = 
          dplyr::case_when(
            Constancy == 1 ~ "I",
            Constancy == 2 ~ "II",
            Constancy == 3 ~ "III",
            Constancy == 4 ~ "IV",
            Constancy == 5 ~ "V",
            TRUE ~ NA
          )
      ) |>
      dplyr::mutate("Constancy" = factor(Constancy, levels = c("V", "IV", "III", "II", "I"))) |>
      dplyr::arrange(Constancy, Species) |>
      dplyr::mutate("Cover" = paste0(minimum_cover, 
                                     " - ", 
                                     ifelse(mean_cover != "+", round(as.numeric(mean_cover)), mean_cover),
                                     " - ", 
                                     maximum_cover), .keep = "unused")

    floristicTables_nvc_NVCToComp <- floristicTables_composed_selected |>
      dplyr::select(-Constancy, -Cover) |>
      dplyr::left_join(floristicTables_nvc, by = "Species") |>
      dplyr::mutate(
        "Species" =
          dplyr::case_when(
            is.na(Constancy) ~ "",
            TRUE ~ as.character(Species)
          )
      )
    
    if(matchSpecies() == "No"){
      
      floristicTables_nvc_view <- floristicTables_nvc
      
    } else if(matchSpecies() == "compToNVC"){
      
      floristicTables_nvc_view <- floristicTables_nvc
      
    } else if(matchSpecies() == "NVCToComp"){
      
      floristicTables_nvc_view <- floristicTables_nvc_NVCToComp
      
    }
    
    output$floristicTables_nvc <- reactable::renderReactable({
      
      floristicTables_nvc <- reactable::reactable(data = floristicTables_nvc_view, 
                                                  filterable = FALSE,
                                                  pagination = FALSE, 
                                                  highlight = TRUE,
                                                  bordered = TRUE,
                                                  sortable = FALSE, 
                                                  wrap = FALSE,
                                                  resizable = TRUE,
                                                  class = "my-tbl",
                                                  # style = list(fontSize = "1rem"),
                                                  rowClass = "my-row",
                                                  defaultColDef = reactable::colDef(
                                                    headerClass = "my-header",
                                                    class = "my-col",
                                                    align = "center" # Needed as alignment is not passing through to header
                                                  )
                                                  )
      
      return(floristicTables_nvc)
      
    })
    
  }) |>
    bindEvent(floristicTables_composed_all_rval(),
              nvcFloristicTable(), 
              matchSpecies(), 
              composedFloristicTable(),
              ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  outputOptions(output, "floristicTables_nvc", suspendWhenHidden = FALSE)
  
  
  ## Title ---------------------------------------------------------------------
  nvcFloristicTableTitle_rval <- reactiveVal(paste("<font size=4.75>",
                                                   "NVC Floristic Table ",
                                                   "</font>") 
  )
  
  observe({
    
    shiny::req(nvcFloristicTable())
    shiny::req(composedFloristicTable())
    
    shiny::isolate({
      comp_attr <- community_attributes()
      comp_FloristicTable_name <- nvcFloristicTable()
    })
    
    # assign(x = "comp_FloristicTable_name", value = comp_FloristicTable_name, envir = .GlobalEnv)
    # assign(x = "comp_attr", value = comp_attr, envir = .GlobalEnv)
    
    comp_attr_values <- comp_attr |>
      dplyr::filter(nvc_code == comp_FloristicTable_name)
    
    nvcFloristicTableTitle <- paste0("<font size=4.75>",
                                     comp_FloristicTable_name,
                                     "<br>",
                                     "Quadrats: ",
                                     comp_attr_values$num_samples,
                                     "<br>",
                                     "Species: ",
                                     comp_attr_values$species_count,
                                     " (min = ",
                                     comp_attr_values$min_species,
                                     ", mean = ",
                                     round(comp_attr_values$mean_species),
                                     ", max = ",
                                     comp_attr_values$max_species,
                                     ")",
                                     "</font>")
    
    nvcFloristicTableTitle_rval(nvcFloristicTableTitle)
    
  }) |>
    bindEvent(nvcFloristicTable(), 
              composedFloristicTable(),
              ignoreInit = TRUE, ignoreNULL = TRUE)
  
  output$nvcFloristicTableTitle <- renderText({ 
    
    nvcFloristicTableTitle <- nvcFloristicTableTitle_rval()
    
    paste(nvcFloristicTableTitle) 
    
  })
  
  # Create All Wide Composed Year Floristic Table ------------------------------
  
  ## Initialise ----------------------------------------------------------------
  floristicTablesWide_composed_init <- data.frame("Species" = character(),
                                                  "Constancy" = character())
  
  output$floristicTablesWide_composed <- reactable::renderReactable({
    
    floristicTablesWide_composed <-  reactable::reactable(data = floristicTablesWide_composed_init,
                                                          filterable = FALSE,
                                                          pagination = FALSE,
                                                          highlight = TRUE,
                                                          bordered = TRUE,
                                                          sortable = TRUE,
                                                          wrap = FALSE,
                                                          resizable = TRUE,
                                                          class = "my-tbl",
                                                          # style = list(fontSize = "1rem"),
                                                          rowClass = "my-row",
                                                          defaultColDef = reactable::colDef(
                                                            headerClass = "my-header",
                                                            class = "my-col",
                                                            align = "center" # Needed as alignment is not passing through to header
                                                          ),
                                                          columns = list(
                                                            Species = reactable::colDef(
                                                              minWidth = 275,
                                                              sticky = "left",
                                                              style = list(borderRight = "1px solid #eee"),
                                                              headerStyle = list(borderRight = "1px solid #eee")
                                                            )
                                                          )
    )
    
    return(floristicTablesWide_composed)
    
  })
  
  ## Update --------------------------------------------------------------------
  observe({
    
    shiny::req(floristicTables_composed_all_wide_rval())
    
    # Retrieve the table, optionally modify the table without triggering recursion.
    shiny::isolate({
      
      floristicTables_composed_all_wide <- floristicTables_composed_all_wide_rval()
      
      floristicTables_composed_all_wide_selected <- floristicTables_composed_all_wide |>
        dplyr::filter(Group == floristicTablesSetView()) |>
        dplyr::select(-Group)
      
    }) # close isolate
    
    output$floristicTablesWide_composed <- reactable::renderReactable({
      
      floristicTablesWide_composed <- reactable::reactable(data = floristicTables_composed_all_wide_selected,
                                                           filterable = FALSE,
                                                           pagination = FALSE,
                                                           highlight = TRUE,
                                                           bordered = TRUE,
                                                           sortable = TRUE,
                                                           wrap = FALSE,
                                                           resizable = TRUE,
                                                           class = "my-tbl",
                                                           # style = list(fontSize = "1rem"),
                                                           rowClass = "my-row",
                                                           defaultColDef = reactable::colDef(
                                                             headerClass = "my-header",
                                                             class = "my-col",
                                                             align = "center" # Needed as alignment is not passing through to header
                                                           ),
                                                           columns = list(
                                                             Species = reactable::colDef(
                                                               minWidth = 275,
                                                               sticky = "left",
                                                               style = list(borderRight = "1px solid #eee"),
                                                               headerStyle = list(borderRight = "1px solid #eee")
                                                             )
                                                           )
      )
      
      return(floristicTablesWide_composed)
      
    })
    
  }) |>
    bindEvent(floristicTables_composed_all_wide_rval(),
              floristicTablesSetView(),
              ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  outputOptions(output, "floristicTablesWide_composed", suspendWhenHidden = FALSE)
  
  ## Title ---------------------------------------------------------------------
  floristicTablesWide_composedTitle_rval <- reactiveVal(paste("<font size=4.75>",
                                                              "Composed Floristic Tables ",
                                                              "</font>") 
  )
  
  observe({
    
    shiny::req(floristicTables_composed_all_wide_rval())
    
    floristicTablesWide_composedTitle <- paste("<font size=4.75>",
                                               floristicTablesSetView(),
                                               "</font>",
                                               sep = "")
    
    floristicTablesWide_composedTitle_rval(floristicTablesWide_composedTitle)
    
  }) |>
    bindEvent(floristicTablesSetView(),
              ignoreInit = TRUE, ignoreNULL = TRUE)
  
  output$floristicTablesWide_composedTitle <- renderText({ 
    
    floristicTablesWide_composedTitle <- floristicTablesWide_composedTitle_rval()
    
    paste(floristicTablesWide_composedTitle) 
    
  })
  

  # Show/Hide Selected Tables -----------------------------------------------
  observe({
    
    shinyjs::show(id = "singleComposedVsNVC_div")
    shinyjs::show(id = "multipleComposed_div")
    
  }) |>
    bindEvent(floristicTablesView(), 
              ignoreNULL = FALSE,
              ignoreInit = FALSE,
              once = TRUE)
  
  observe({
    
    if(floristicTablesView() == "singleComposedVsNVC") {
      
      shinyjs::show(id = "singleComposedVsNVC_div")
      shinyjs::hide(id = "multipleComposed_div")
      
    } else if(floristicTablesView() == "multipleComposed") {
      
      shinyjs::hide(id = "singleComposedVsNVC_div")
      shinyjs::show(id = "multipleComposed_div")
      
    }
    
  }) |>
    bindEvent(floristicTablesView(), 
              ignoreInit = FALSE)
  
  observe({
    
    shinyjs::show(id = "singleComposedVsNVC_div")
    
  }) |>
    bindEvent(floristicTablesView(), 
              ignoreNULL = FALSE,
              ignoreInit = FALSE,
              once = TRUE)
  

  # Return Data -------------------------------------------------------------
  return(floristicTables)
  
}
