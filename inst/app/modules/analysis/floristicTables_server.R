floristicTables <- function(input, output, session, region, setupData, surveyData, avgEIVs, deSidebar_options, sidebar_options) {
  
  ns <- session$ns
  
# Retrieve sidebar options ------------------------------------------------
  coverScale <- reactiveVal()
  removeLowFreqTaxa <- reactiveVal()
  floristicTablesView <- reactiveVal()
  floristicTablesSetView <- reactiveVal()
  composedFloristicTable <- reactiveVal()
  vcFloristicTable <- reactiveVal()
  matchSpecies <- reactiveVal()
  runAnalysis <- reactiveVal()

  observe({

    coverScale(deSidebar_options()$coverScale)
    removeLowFreqTaxa(sidebar_options()$removeLowFreqTaxa)
    floristicTablesView(sidebar_options()$floristicTablesView)
    floristicTablesSetView(sidebar_options()$floristicTablesSetView)
    composedFloristicTable(sidebar_options()$composedFloristicTable)
    vcFloristicTable(sidebar_options()$vcFloristicTable)
    matchSpecies(sidebar_options()$matchSpecies)
    runAnalysis(sidebar_options()$runAnalysis)

  }) |>
    bindEvent(sidebar_options(), 
              deSidebar_options(),
              ignoreInit = TRUE)
  
# Retrieve setup data -----------------------------------------------------
  regional_availability <- reactiveVal()
  ft_taxon_name_col <- reactiveVal()
  unit_name_col <- reactiveVal()
  floristic_tables <- reactiveVal()
  use_eivs <- reactiveVal()
  
  observe({
    
    shiny::req(nrow(setupData()$floristic_tables) != 0)
      
    shiny::isolate({
      
      regional_availability(setupData()$regional_availability)
      ft_taxon_name_col(setupData()$ft_taxon_name_col)
      unit_name_col(setupData()$unit_name_col)
      use_eivs(setupData()$regional_availability$avgEIVs) 
      floristic_tables_raw <- setupData()$floristic_tables
      
    })
    
    if(region() == "gbnvc"){
      
      floristic_tables(floristic_tables_raw)
      
    } else if(region() == "mnnpc"){

      floristic_tables(
        floristic_tables_raw |>
          dplyr::mutate_at(dplyr::vars(minimum_cover, mean_cover, maximum_cover),
                           list(
                             ~round(.)
                             )
                           )

        ) |>
        suppressWarnings()


    }
    
  }) |>
    shiny::bindEvent(setupData(),
                     ignoreInit = FALSE,
                     ignoreNULL = TRUE)

# Create object containing all composed tables ----------------------------
  floristicTables_composed_all_rval <- reactiveVal()
  floristicTables_composed_all_wide_rval <- reactiveVal()
  floristicTables <- reactiveVal()
  
  # Reset to NULL when Run Analysis is clear to clean the slate
  # observe({
  # 
  #   floristicTables_composed_all_rval(NULL)
  #   floristicTables_composed_all_wide_rval(NULL)
  #   floristicTables(NULL)
  # 
  # }) |>
  #   bindEvent(runAnalysis(),
  #             ignoreNULL = TRUE,
  #             ignoreInit = TRUE)
  
  observe({
    
    shiny::req(surveyData())

    shiny::isolate({
      
      coverScale <- coverScale()
      removeLowFreqTaxa <- removeLowFreqTaxa()
      
      if(isTRUE(regional_availability()$aggTaxa)){
        
        surveyData_long <- surveyData()$surveyData_long_prop_agg
        
      } else {
        
        surveyData_long <- surveyData()$surveyData_long_prop
        
      }
      
    })
    
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
    
    ## Prepare cover summary ----------------------------------------------
    if(region() == "gbnvc"){
      
    floristicTables_composed_all <- floristicTables_composed_all |>
      dplyr::mutate_at(dplyr::vars(Min.Cover, Mean.Cover, Max.Cover),
                       ~as.numeric(.)) |>
      dplyr::mutate_at(dplyr::vars(Min.Cover, Mean.Cover, Max.Cover),
                       list(
                         ~dplyr::case_when(
                           . >= 0.955 ~ "10",
                           . >= 0.83 ~ "9",
                           . >= 0.63 ~ "8",
                           . >= 0.42 ~ "7",
                           . >= 0.3 ~ "6",
                           . >= 0.18 ~ "5",
                           . >= 0.08 ~ "4",
                           . >= 0.03 ~ "3",
                           . >= 0.005 ~ "2",
                           . >= 0.003 ~ "1",
                           . >= 0 ~ "+",
                           TRUE ~ NA
                         )
                       )
      ) |>
      dplyr::mutate("Cover" = paste0(ifelse(is.na(Min.Cover), "?", Min.Cover), 
                                     " - ", 
                                     ifelse(!is.na(Mean.Cover), 
                                            ifelse(Mean.Cover != "+", round(as.numeric(Mean.Cover)), Mean.Cover),
                                            "?"),
                                     " - ", 
                                     ifelse(is.na(Max.Cover), "?", Max.Cover)), 
                    .keep = "unused")
    
    } else if(region() == "mnnpc"){
      
      floristicTables_composed_all <- floristicTables_composed_all |>
        dplyr::mutate_at(dplyr::vars(Min.Cover, Mean.Cover, Max.Cover),
                         ~as.numeric(.)) |>
        dplyr::mutate_at(dplyr::vars(Min.Cover, Mean.Cover, Max.Cover),
                         list(
                           ~round(100 * .)
                         )
        ) |>
        dplyr::mutate("Cover" = paste0(ifelse(is.na(Min.Cover), "?", Min.Cover), 
                                       " - ", 
                                       ifelse(is.na(Mean.Cover), "?", Mean.Cover),
                                       " - ", 
                                       ifelse(is.na(Max.Cover), "?", Max.Cover)), 
                      .keep = "unused")
      
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
    shiny::req(!is.null(vcFloristicTable()))
    shiny::req(vcFloristicTable() != "")
    shiny::req(vcFloristicTable() %in% unique(floristic_tables()[[unit_name_col()]]))
    shiny::req(composedFloristicTable() %in% unique(floristicTables_composed_all_rval()[["ID"]]))
    
    # Retrieve the table, optionally modify the table without triggering recursion.
    shiny::isolate({
      
      floristicTables_composed_all <- floristicTables_composed_all_rval()
      composedFloristicTable_name <- composedFloristicTable()
      vcFloristicTable_name <- vcFloristicTable()
      floristic_tables <- floristic_tables()
      ft_taxon_name_col <- ft_taxon_name_col()
      unit_name_col <- unit_name_col()
      
    })
    
    floristicTables_composed_selected <- floristicTables_composed_all |>
      dplyr::filter(ID == composedFloristicTable_name) |>
      dplyr::select(-ID)
    
    floristicTables_vc <- floristic_tables |>
      dplyr::filter(.data[[unit_name_col]] == vcFloristicTable_name) |>
      dplyr::select("Species" = ft_taxon_name_col, "Constancy" = "constancy") |>
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
    
    floristicTables_composed_compToVC <- floristicTables_vc |>
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
      
    } else if(matchSpecies() == "compToVC"){
      
      floristicTables_composed_view <- floristicTables_composed_compToVC
      
    } else if(matchSpecies() == "VCToComp"){
      
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
              vcFloristicTable(),
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
  
  # Samp EIVs text output ---------------------------------------------------
  samp_eivs_text_output_rval <- reactiveVal(paste("<font size=4.75>",
                                                  " ",
                                                  "</font>")
                                            )
  
  observe({
    
    shiny::isolate({
      samp_floristicTable_name <- composedFloristicTable()
      avgEIVs <- avgEIVs()
    })
    
    if(isTRUE(use_eivs())){
      
      samp_cm_he <- dplyr::bind_rows(
        avgEIVs$unweightedMeanHEValuesSite |> dplyr::mutate("ID" = as.character(Year), .keep = "unused"),
        avgEIVs$unweightedMeanHEValuesGroup |> tidyr::unite("ID", Year, Group, sep = " - ", remove = TRUE)
      ) |>
        dplyr::filter(ID == samp_floristicTable_name)
      
      f_val <- samp_cm_he |>
        dplyr::pull(Moisture.F) |>
        round(digits = 2)
      
      l_val <- samp_cm_he |>
        dplyr::pull(Light.L) |>
        round(digits = 2)
      
      n_val <- samp_cm_he |>
        dplyr::pull(Nitrogen.N) |>
        round(digits = 2)
      
      r_val <- samp_cm_he |>
        dplyr::pull(Reaction.R) |>
        round(digits = 2)
      
      s_val <- samp_cm_he |>
        dplyr::pull(Salinity.S) |>
        round(digits = 2)
      
      samp_eivs_text <- paste0("<font size=4.75>",
                               "Hill-Ellenberg: ",
                               "F = ", f_val, ", ",
                               "L = ", l_val, ", ",
                               "N = ", n_val, ", ",
                               "R = ", r_val, ", ",
                               "S = ", s_val,
                               "</font>")
      
    } else {
      
      samp_eivs_text <- paste("<font size=4.75>",
                              " ",
                              "</font>")
      
    }
    
    samp_eivs_text_output_rval(samp_eivs_text)
    
    
  }) |>
    bindEvent(composedFloristicTableTitle_rval(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
  
  output$samp_eivs_text_output <- renderText({ 
    
    samp_eivs_text <- samp_eivs_text_output_rval()
    
    paste(samp_eivs_text) 
    
  })

  # VC Floristic Tables -------------------------------------------------------

  ## Intialise -----------------------------------------------------------------
  floristicTables_vc_init <- data.frame("Species" = character(),
                                        "Constancy" = character(),
                                        "Cover" = character())
  
  output$floristicTables_vc <- reactable::renderReactable({
    
    floristicTables_vc <- reactable::reactable(data = floristicTables_vc_init,
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
    
    return(floristicTables_vc)
    
  })

  ## Update --------------------------------------------------------------------
  observe({
    
    shiny::req(!is.null(composedFloristicTable()))
    shiny::req(!is.null(vcFloristicTable()))
    shiny::req(vcFloristicTable() != "")
    shiny::req(vcFloristicTable() %in% unique(floristic_tables()[[unit_name_col()]]))
    shiny::req(composedFloristicTable() %in% unique(floristicTables_composed_all_rval()[["ID"]]))
    
    # Retrieve the table, optionally modify the table without triggering recursion.
    shiny::isolate({
      
      floristicTables_composed_all <- floristicTables_composed_all_rval()
      composedFloristicTable <- composedFloristicTable()
      vcFloristicTable <- vcFloristicTable()
      unit_name_col <- unit_name_col()
      ft_taxon_name_col <- ft_taxon_name_col()
      floristic_tables <- floristic_tables()
      
    })
      
    floristicTables_composed_selected <- floristicTables_composed_all |>
      dplyr::filter(ID == composedFloristicTable) |>
      dplyr::select(-ID)
    
    floristicTables_vc <- floristic_tables |>
      dplyr::filter(.data[[unit_name_col]] == vcFloristicTable) |>
      dplyr::select("Species" = ft_taxon_name_col, "Constancy" = "constancy",
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

    floristicTables_vc_VCToComp <- floristicTables_composed_selected |>
      dplyr::select(-Constancy, -Cover) |>
      dplyr::left_join(floristicTables_vc, by = "Species") |>
      dplyr::mutate(
        "Species" =
          dplyr::case_when(
            is.na(Constancy) ~ "",
            TRUE ~ as.character(Species)
          )
      )
    
    if(matchSpecies() == "No"){
      
      floristicTables_vc_view <- floristicTables_vc
      
    } else if(matchSpecies() == "compToVC"){
      
      floristicTables_vc_view <- floristicTables_vc
    
    } else if(matchSpecies() == "VCToComp"){
      
      floristicTables_vc_view <- floristicTables_vc_VCToComp
      
    }
    
    output$floristicTables_vc <- reactable::renderReactable({
      
      floristicTables_vc <- reactable::reactable(data = floristicTables_vc_view, 
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
      
      return(floristicTables_vc)
      
    })
    
  }) |>
    bindEvent(floristicTables_composed_all_rval(),
              vcFloristicTable(), 
              matchSpecies(), 
              composedFloristicTable(),
              ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  outputOptions(output, "floristicTables_vc", suspendWhenHidden = FALSE)
  
  
  ## Title ---------------------------------------------------------------------
  vcFloristicTableTitle_rval <- reactiveVal(paste("<font size=4.75>",
                                                  "VC Floristic Table ",
                                                  "</font>") 
  )
  
  observe({
    
    shiny::req(vcFloristicTable())
    shiny::req(composedFloristicTable())
    
    shiny::isolate({
      setupData <- setupData()
      comp_FloristicTable_name <- vcFloristicTable()
      unit_name_col <- unit_name_col()
    })
    
    comp_attr <- setupData$community_attributes
    
    comp_attr_values <- try({
      comp_attr |> dplyr::filter(.data[[unit_name_col]] == comp_FloristicTable_name)
    })
    
    if(all(class(comp_attr_values) != "try-error")){
      if(nrow(comp_attr_values) == 1){
        name_quadrats <- comp_attr_values$num_samples
        name_total_species <- comp_attr_values$species_count
        name_min_species <- comp_attr_values$min_species
        name_mean_species <- round(comp_attr_values$mean_species)
        name_max_species <- comp_attr_values$max_species
      } else {
        name_quadrats <- paste0("<font color=\"red\">NA</font>")
        name_total_species <- paste0("<font color=\"red\">NA</font>")
        name_min_species <- paste0("<font color=\"red\">NA</font>")
        name_mean_species <- paste0("<font color=\"red\">NA</font>")
        name_max_species <- paste0("<font color=\"red\">NA</font>")
      }
    } else if(class(comp_attr_values) == "try-error"){
      name_quadrats <- paste0("<font color=\"red\">NA</font>")
      name_total_species <- paste0("<font color=\"red\">NA</font>")
      name_min_species <- paste0("<font color=\"red\">NA</font>")
      name_mean_species <- paste0("<font color=\"red\">NA</font>")
      name_max_species <- paste0("<font color=\"red\">NA</font>")
    }
    
    vcFloristicTableTitle <- paste0("<font size=4.75>",
                                    comp_FloristicTable_name,
                                    "<br>",
                                    "Quadrats: ",
                                    name_quadrats,
                                    "<br>",
                                    "Species: ",
                                    name_total_species,
                                    " (min = ",
                                    name_min_species,
                                    ", mean = ",
                                    name_mean_species,
                                    ", max = ",
                                    name_max_species,
                                    ")",
                                    "</font>")
    
    vcFloristicTableTitle_rval(vcFloristicTableTitle)
    
  }) |>
    bindEvent(vcFloristicTable(), 
              composedFloristicTable(),
              ignoreInit = TRUE, ignoreNULL = TRUE)
  
  output$vcFloristicTableTitle <- renderText({ 
    
    vcFloristicTableTitle <- vcFloristicTableTitle_rval()
    
    paste(vcFloristicTableTitle) 
    
  })
  

# Comp EIVs text output ---------------------------------------------------
  comp_eivs_text_output_rval <- reactiveVal(paste("<font size=4.75>",
                                                   " ",
                                                   "</font>") 
                                            )
  
  observe({
    
    shiny::isolate({
      setupData <- setupData()
      comp_FloristicTable_name <- vcFloristicTable()
      unit_name_col <- unit_name_col()
    })
    
    if(isTRUE(use_eivs())){
      
      comm_cm_he <- setupData$comm_cm_he |>
        dplyr::filter(.data[[unit_name_col]] == comp_FloristicTable_name)
      
      f_val <- comm_cm_he |>
        dplyr::filter(indicator == "F") |>
        dplyr::pull(mean) |>
        round(digits = 2)
      
      l_val <- comm_cm_he |>
        dplyr::filter(indicator == "L") |>
        dplyr::pull(mean) |>
        round(digits = 2)
      
      n_val <- comm_cm_he |>
        dplyr::filter(indicator == "N") |>
        dplyr::pull(mean) |>
        round(digits = 2)
      
      r_val <- comm_cm_he |>
        dplyr::filter(indicator == "R") |>
        dplyr::pull(mean) |>
        round(digits = 2)
      
      s_val <- comm_cm_he |>
        dplyr::filter(indicator == "S") |>
        dplyr::pull(mean) |>
        round(digits = 2)
      
      comp_eivs_text <- paste0("<font size=4.75>",
                               "Hill-Ellenberg: ",
                               "F = ", f_val, ", ",
                               "L = ", l_val, ", ",
                               "N = ", n_val, ", ",
                               "R = ", r_val, ", ",
                               "S = ", s_val,
                               "</font>")
      
    } else {
      
      comp_eivs_text <- paste("<font size=4.75>",
                              " ",
                              "</font>")
      
    }
    
    comp_eivs_text_output_rval(comp_eivs_text)
    
    
  }) |>
    bindEvent(vcFloristicTableTitle_rval(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
  
  output$comp_eivs_text_output <- renderText({ 
    
    comp_eivs_text <- comp_eivs_text_output_rval()
    
    paste(comp_eivs_text) 
    
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
    
    shinyjs::show(id = "singleComposedVsVC_div")
    shinyjs::show(id = "multipleComposed_div")
    
  }) |>
    bindEvent(floristicTablesView(), 
              ignoreNULL = FALSE,
              ignoreInit = FALSE,
              once = TRUE)
  
  observe({
    
    if(floristicTablesView() == "singleComposedVsVC") {
      
      shinyjs::show(id = "singleComposedVsVC_div")
      shinyjs::hide(id = "multipleComposed_div")
      
    } else if(floristicTablesView() == "multipleComposed") {
      
      shinyjs::hide(id = "singleComposedVsVC_div")
      shinyjs::show(id = "multipleComposed_div")
      
    }
    
  }) |>
    bindEvent(floristicTablesView(), 
              ignoreInit = FALSE)
  
  observe({
    
    shinyjs::show(id = "singleComposedVsVC_div")
    
  }) |>
    bindEvent(floristicTablesView(), 
              ignoreNULL = FALSE,
              ignoreInit = FALSE,
              once = TRUE)
  

  # Return Data -------------------------------------------------------------
  return(floristicTables)
  
}
