surveyDataValidator <- function(input, output, session, setupData, surveyData, deSidebar_options) {
  
  ns <- session$ns
  
# Retrieve Setup Data -----------------------------------------------------
  speciesNames <- reactiveVal()

  observe({
    
    setupData <- setupData()
    
    speciesNames(setupData$species_names)
    
  }) |>
    bindEvent(setupData(),
              ignoreInit = FALSE)
  
# Retrieve sidebar options ------------------------------------------------
  coverScale <- reactiveVal()
  
  observe({
    
    coverScale(deSidebar_options()$coverScale)
    
  }) |>
    bindEvent(deSidebar_options(), 
              ignoreInit = FALSE)
  
# Initialise Table to Replace Species Not In Accepted List ----------------
  speciesAdjustmentTable_init <- data.frame("Species.Submitted" = character(),
                                            "Species.Adjusted" = character(),
                                            "Species.Ignore" = character(),
                                            "Species.Remove" = character())
  
  speciesAdjustmentTable_rval <- reactiveVal(speciesAdjustmentTable_init)
  
  output$speciesAdjustmentTable <- rhandsontable::renderRHandsontable({
    
    speciesAdjustmentTable <- rhandsontable::rhandsontable(data = speciesAdjustmentTable_init,
                                                           height = 500,
                                                           rowHeaders = NULL,
                                                           width = "100%"
                                                           ) |>
      rhandsontable::hot_col(col = colnames(speciesAdjustmentTable_init), halign = "htCenter") |>
      rhandsontable::hot_col(
        col = "Species.Submitted",
        readOnly = TRUE,
      ) |>
      rhandsontable::hot_col(
        col = "Species.Ignore",
        readOnly = FALSE,
        type = "dropdown",
        source = c("No", "Yes"),
        strict = TRUE,
        default = as.character("No")
      ) |>
      rhandsontable::hot_col(
        col = "Species.Remove",
        readOnly = FALSE,
        type = "dropdown",
        source = c("No", "Yes"),
        strict = TRUE,
        default = as.character("No")
      ) |>
      rhandsontable::hot_cols(colWidths = c(200, 200, 200, 200)) |>
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
      htmlwidgets::onRender("
      function(el, x) {
        var hot = this.hot
        $('a[data-value=\"validatesurveyData\"').on('click', function(){
          setTimeout(function() {hot.render();}, 0);
        })
      }")
    
    return(speciesAdjustmentTable)
    
  })
  
# Initialise Table to Re-allocate groups ---------------------------------
  reallocateGroups_init <- data.frame("Quadrat" = character(),
                                      "Group" = character())
  
  reallocateGroups_rval <- reactiveVal(reallocateGroups_init)
  
  output$reallocateGroupsTable <- rhandsontable::renderRHandsontable({
    
    reallocateGroupsTable <- rhandsontable::rhandsontable(data = reallocateGroups_init,
                                                          height = 300,
                                                          rowHeaders = NULL,
                                                          width = "100%"
                                                          ) |>
      rhandsontable::hot_col(col = colnames(reallocateGroups_init), halign = "htCenter") |>
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
      htmlwidgets::onRender("
      function(el, x) {
        var hot = this.hot
        $('a[data-value=\"validatesurveyData\"').on('click', function(){
          setTimeout(function() {hot.render();}, 0);
        })
      }")
    
    return(reallocateGroupsTable)
    
  })

# Perform Validation Checks on surveyData --------------------------------
  surveyDataValidation_rval <- reactiveVal(
    list(
      "speciesInAccepted" = FALSE,
      "speciesNotAccepted" = FALSE,
      "coverSupplied" = FALSE,
      "coverSuppliedAll" = FALSE,
      "yearComplete" = FALSE,
      "groupComplete" = FALSE,
      "quadratComplete" = FALSE,
      "speciesComplete" = FALSE,
      "speciesQuadratUnique" = FALSE,
      "quadratIDUnique" = FALSE,
      "quadratIDDuplicates" = FALSE,
      "groupIDUnique" = FALSE,
      "groupIDDuplicates" = FALSE,
      "coverValuesOK" = FALSE,
      "surveyData_wide_ok" = FALSE,
      "surveyData_mat_ok" = FALSE,
      "okToProceed" = FALSE
    )
  )
  
  observe({
    
    shiny::req(surveyData())
    shiny::req(speciesNames())
    
    surveyData <- surveyData()
    surveyData_original <- surveyData$surveyData_original
    surveyData_long <- surveyData$surveyData_long
    surveyData_wide <- surveyData$surveyData_wide
    surveyData_mat <- surveyData$surveyData_mat
    
    speciesNames <- speciesNames()
    
    coverScale <- coverScale()

    # Check all species are accepted
    if(!is.null(input$speciesAdjustmentTable)){

      speciesToIgnore <- rhandsontable::hot_to_r(input$speciesAdjustmentTable) |>
        dplyr::filter(Species.Ignore == "Yes") |>
        dplyr::pull(Species.Submitted)

      surveyData_speciesToIgnore <- speciesToIgnore

      species_to_check <- setdiff(unique(surveyData_long$Species), speciesToIgnore)

      surveyData_speciesInAccepted <- isTRUE(all(species_to_check %in% speciesNames))

    } else {

      surveyData_speciesToIgnore <- c()

      surveyData_speciesInAccepted <- isTRUE(all(unique(surveyData_long$Species) %in% speciesNames))

    }

    # Check which species are not accepted
    surveyData_speciesNotAccepted <- setdiff(unique(surveyData_long$Species), speciesNames)

    # Check whether any cover estimates are supplied
    surveyData_coverSupplied <- isTRUE(all(!is.na(unique(surveyData_long$Cover))))

    # Check whether all cover estimates are supplied
    surveyData_coverSuppliedAll <- isTRUE(all(!is.na(surveyData_long$Cover)))

    # Check whether there is any missing data in the Year column
    surveyData_yearComplete <- isTRUE(all(!is.na(surveyData_long$Year)))

    # Check whether there is any missing data in the Group column
    surveyData_groupComplete <- isTRUE(all(surveyData_long$Group != ""))

    # Check whether there is any missing data in the Quadrat column
    surveyData_quadratComplete <- isTRUE(all(surveyData_long$Quadrat != ""))

    # Check whether there is any missing data in the Species column
    surveyData_speciesComplete <- isTRUE(all(surveyData_long$Species != ""))

    # Check whether there are any species-quadrat double-entries
    surveyData_speciesQuadratUnique_df <- surveyData_long |>
      dplyr::select(Year, Group, Quadrat, Species) |>
      dplyr::group_by(Year, Group, Quadrat, Species) |>
      dplyr::filter(dplyr::n() > 1) |>
      dplyr::ungroup()

    surveyData_speciesQuadratUnique <- isTRUE(nrow(surveyData_speciesQuadratUnique_df) == 0)

    # Check whether each quadrat ID is unique
    surveyData_quadratIDUnique_df <- surveyData_long |>
      dplyr::select(Year, Group, Quadrat) |>
      dplyr::distinct() |>
      dplyr::select(Group, Quadrat) |>
      dplyr::distinct() |>
      dplyr::group_by(Quadrat) |>
      dplyr::filter(dplyr::n() > 1) |>
      dplyr::ungroup()

    surveyData_quadratIDUnique <- isTRUE(nrow(surveyData_quadratIDUnique_df) == 0)

    surveyData_quadratIDDuplicates <- surveyData_quadratIDUnique_df |>
      dplyr::pull(Quadrat)

    # Check whether each group ID is unique
    surveyData_groupIDUnique_df <- surveyData_long |>
      dplyr::select(Year, Group) |>
      dplyr::distinct() |>
      dplyr::group_by(Year, Group) |>
      dplyr::filter(dplyr::n() > 1) |>
      dplyr::ungroup()

    surveyData_groupIDDuplicates <- surveyData_groupIDUnique_df
    surveyData_groupIDUnique <- isTRUE(nrow(surveyData_groupIDUnique_df) == 0)

    # Check whether it is ok to create the wide and mat surveyData objects
    okToCreateWideMat <- isTRUE(all(surveyData_yearComplete,
                                    surveyData_groupComplete, surveyData_quadratComplete,
                                    surveyData_speciesComplete, surveyData_quadratIDUnique,
                                    surveyData_groupIDUnique))
    
    # Check whether the cover values are ok
    if(coverScale == "none"){
      
      surveyData_coverValuesOK <- isTRUE(all(is.na(surveyData_original$Cover)))
      
    } else if(coverScale == "percentage"){

      surveyData_coverValuesOK <- isTRUE(all(all(surveyData_original$Cover > 0), all(surveyData_original$Cover <= 100)))

    } else if(coverScale == "proportional"){

      surveyData_coverValuesOK <- isTRUE(all(all(surveyData_original$Cover > 0), all(surveyData_original$Cover <= 1)))

    } else if(coverScale == "domin"){

      surveyData_coverValuesOK <- isTRUE(all(surveyData_original$Cover %in% RMAVIS:::dominConvert$Cover)) # & all(!is.null(levels(surveyData_original$Cover))))

    } else if(coverScale == "braunBlanquet"){

      surveyData_coverValuesOK <- isTRUE(all(surveyData_original$Cover %in% RMAVIS:::braunBlanquetConvert$Cover)) # & all(!is.null(levels(surveyData_original$Cover))))

    }

    # Check whether the survey data wide object is ok
    surveyData_wide_ok <- isTRUE(!is.null(surveyData_wide))

    # Check whether the survey data mat object is ok
    surveyData_mat_ok <- isTRUE(!is.null(surveyData_mat))

    # Check whether the analysis is ok to proceed
    okToProceed <- isTRUE(all(surveyData_speciesInAccepted, surveyData_yearComplete,
                              surveyData_groupComplete, surveyData_quadratComplete,
                              surveyData_speciesComplete, surveyData_quadratIDUnique,
                              surveyData_speciesQuadratUnique,
                              surveyData_coverValuesOK,
                              surveyData_wide_ok, surveyData_mat_ok,
                              surveyData_groupIDUnique))


    # Create list of validation checkes
    surveyDataValidation <- list(
      "speciesToIgnore" = surveyData_speciesToIgnore,
      "speciesInAccepted" = surveyData_speciesInAccepted,
      "speciesNotAccepted" = surveyData_speciesNotAccepted,
      "coverSupplied" = surveyData_coverSupplied,
      "coverSuppliedAll" = surveyData_coverSuppliedAll,
      "yearComplete" = surveyData_yearComplete,
      "groupComplete" = surveyData_groupComplete,
      "quadratComplete" = surveyData_quadratComplete,
      "speciesComplete" = surveyData_speciesComplete,
      "speciesQuadratUnique" = surveyData_speciesQuadratUnique,
      "quadratIDUnique" = surveyData_quadratIDUnique,
      "quadratIDDuplicates" = surveyData_quadratIDDuplicates,
      "groupIDUnique" = surveyData_groupIDUnique,
      "groupIDDuplicates" = surveyData_groupIDDuplicates,
      "coverValuesOK" = surveyData_coverValuesOK,
      "surveyData_wide_ok" = surveyData_wide_ok,
      "surveyData_mat_ok" = surveyData_mat_ok,
      "okToProceed" = okToProceed
    )

    surveyDataValidation_rval(surveyDataValidation)

  }) |>
    bindEvent(surveyData(),
              speciesNames(),
              coverScale(),
              input$adjustSpecies,
              speciesAdjustmentTable_rval(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)

# Update Table to Replace Species Not In Accepted List --------------------
  speciesAdjustmentTable_rval <- reactiveVal()

  observe({

    shiny::req(surveyDataValidation_rval())
    shiny::req(speciesNames())

    surveyDataValidation <- surveyDataValidation_rval()
    speciesNames <- speciesNames()

    if(length(surveyDataValidation$speciesNotAccepted) > 0){

      speciesAdjustmentTable <- data.frame("Species.Submitted" = surveyDataValidation$speciesNotAccepted,
                                           "Species.Adjusted" = as.character(NA_character_),
                                           "Species.Ignore" = "No",
                                           "Species.Remove" = "No") |>
        dplyr::mutate(
          "Species.Ignore" =
            dplyr::case_when(
              Species.Submitted %in% surveyDataValidation$speciesToIgnore ~ "Yes",
              TRUE ~ as.character("No")
            )
        )

    } else {

      speciesAdjustmentTable <- speciesAdjustmentTable_init

    }

    output$speciesAdjustmentTable <- rhandsontable::renderRHandsontable({

      speciesAdjustmentTable <- rhandsontable::rhandsontable(data = speciesAdjustmentTable,
                                                             height = 500,
                                                             rowHeaders = NULL,
                                                             width = "100%"
      ) |>
        rhandsontable::hot_col(col = colnames(speciesAdjustmentTable), halign = "htCenter") |>
        rhandsontable::hot_col(
          col = "Species.Submitted",
          readOnly = TRUE,
        ) |>
        rhandsontable::hot_col(
          col = "Species.Adjusted",
          readOnly = FALSE,
          type = "dropdown",
          source = speciesNames,
          strict = TRUE,
          default = as.character(NA_character_)
        ) |>
        rhandsontable::hot_col(
          col = "Species.Ignore",
          readOnly = FALSE,
          type = "dropdown",
          source = c("No", "Yes"),
          strict = TRUE,
          default = as.character("No")
        ) |>
        rhandsontable::hot_col(
          col = "Species.Remove",
          readOnly = FALSE,
          type = "dropdown",
          source = c("No", "Yes"),
          strict = TRUE,
          default = as.character("No")
        ) |>
        rhandsontable::hot_cols(colWidths = c(200, 200, 200, 200)) |>
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
        htmlwidgets::onRender("
          function(el, x) {
            var hot = this.hot
            $('a[data-value=\"validatesurveyData\"').on('click', function(){
              setTimeout(function() {hot.render();}, 0);
            })
          }")

      return(speciesAdjustmentTable)

    })

    speciesAdjustmentTable_rval(speciesAdjustmentTable)

  }) |>
    bindEvent(input$adjustSpecies,
              surveyDataValidation_rval(),
              speciesNames(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
  
  # Update Table to to Re-allocate groups ------------------------------
  reallocateGroupsTable_rval <- reactiveVal()
  
  observe({
    
    shiny::req(!is.null(surveyData()$surveyData_long))
    shiny::req(reallocateGroups_rval())
    
    surveyData <- surveyData()
    surveyData_long <- surveyData$surveyData_long
    
    reallocateGroups <- reallocateGroups_rval()
      
    reallocateGroupsTable <- surveyData_long |>
      dplyr::select(Quadrat, Group) |>
      dplyr::distinct()
    
    output$reallocateGroupsTable <- rhandsontable::renderRHandsontable({
      
      reallocateGroupsTable <- rhandsontable::rhandsontable(data = reallocateGroupsTable,
                                                            height = 300,
                                                            rowHeaders = NULL,
                                                            width = "100%"#,
                                                            # overflow = "visible",
                                                            # stretchH = "all"
                                                            ) |>
        rhandsontable::hot_col(col = colnames(reallocateGroupsTable), halign = "htCenter") |>
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
        htmlwidgets::onRender("
          function(el, x) {
            var hot = this.hot
            $('a[data-value=\"validatesurveyData\"').on('click', function(){
              setTimeout(function() {hot.render();}, 0);
            })
          }")
      
      return(reallocateGroupsTable)
      
    })
    
    reallocateGroupsTable_rval(reallocateGroupsTable)
    
  }) |>
    bindEvent(surveyData(),
              ignoreInit = FALSE,
              ignoreNULL = TRUE)
  
  
# Create Text Validation Outputs ------------------------------------------
  observe({
    
    surveyDataValidation <- surveyDataValidation_rval()
    
    speciesInAccepted <- surveyDataValidation$speciesInAccepted
    speciesNotAccepted <- surveyDataValidation$speciesNotAccepted
    coverSupplied <- surveyDataValidation$coverSupplied
    coverSuppliedAll <- surveyDataValidation$coverSuppliedAll
    yearComplete <- surveyDataValidation$yearComplete
    groupComplete <- surveyDataValidation$groupComplete
    quadratComplete <- surveyDataValidation$quadratComplete
    speciesComplete <- surveyDataValidation$speciesComplete
    speciesQuadratUnique <- surveyDataValidation$speciesQuadratUnique
    quadratIDUnique <- surveyDataValidation$quadratIDUnique
    quadratIDDuplicates <- surveyDataValidation$quadratIDDuplicates
    groupIDUnique <- surveyDataValidation$groupIDUnique
    groupIDDuplicates <- surveyDataValidation$groupIDDuplicates
    coverValuesOK <- surveyDataValidation$coverValuesOK
    okToProceed <- surveyDataValidation$okToProceed
    
    

# All Species in Accepted List Text ---------------------------------------
    output$speciesInAcceptedText <- shiny::renderText({
      
      paste0("All Species Accepted: ",
             ifelse(
               as.character(speciesInAccepted) == TRUE,
               paste('<font color="green"><b>', 
                     as.character(speciesInAccepted), 
                     '</b></font>'),
               paste('<font color="red"><b>', 
                     as.character(speciesInAccepted), 
                     '</b></font>')
             )
      )
      
    })


# Year Column Complete Text -----------------------------------------------
    output$yearCompleteText <- shiny::renderText({
      
      paste0("Year Column Complete (no missing values): ",
             ifelse(
               as.character(yearComplete) == TRUE,
               paste('<font color="green"><b>', 
                     as.character(yearComplete), 
                     '</b></font>'),
               paste('<font color="red"><b>', 
                     as.character(yearComplete), 
                     '</b></font>')
             )
      )
      
    })


# Group Column Complete Text ----------------------------------------------
    output$groupCompleteText <- shiny::renderText({
      
      paste0("Group Column Complete (no missing values): ",
             ifelse(
               as.character(groupComplete) == TRUE,
               paste('<font color="green"><b>', 
                     as.character(groupComplete), 
                     '</b></font>'),
               paste('<font color="red"><b>', 
                     as.character(groupComplete), 
                     '</b></font>')
             )
      )
      
    })


# Quadrat Column Complete Text --------------------------------------------
    output$quadratCompleteText <- shiny::renderText({
      
      paste0("Quadrat Column Complete (no missing values): ",
             ifelse(
               as.character(quadratComplete) == TRUE,
               paste('<font color="green"><b>', 
                     as.character(quadratComplete), 
                     '</b></font>'),
               paste('<font color="red"><b>', 
                     as.character(quadratComplete), 
                     '</b></font>')
             )
      )
      
    })
    
# Species Column Complete Text --------------------------------------------
    output$speciesCompleteText <- shiny::renderText({
      
      paste0("Species Column Complete (no missing values): ",
             ifelse(
               as.character(speciesComplete) == TRUE,
               paste('<font color="green"><b>', 
                     as.character(speciesComplete), 
                     '</b></font>'),
               paste('<font color="red"><b>', 
                     as.character(speciesComplete), 
                     '</b></font>')
             )
      )
      
    })

# Quadrat Names Unique Text -----------------------------------------------
    output$speciesQuadratUniqueText <- shiny::renderText({
      
      paste0("No Duplicate Species Within Quadrats: ",
             ifelse(
               as.character(speciesQuadratUnique) == TRUE,
               paste('<font color="green"><b>', 
                     as.character(speciesQuadratUnique), 
                     '</b></font>'),
               paste('<font color="red"><b>', 
                     as.character(speciesQuadratUnique), 
                     '</b></font>')
             )
      )
      
    })
    
# Quadrat Names Unique Text -----------------------------------------------
    output$quadratIDUniqueText <- shiny::renderText({
      
      paste0("Quadrat Names Unique: ",
             ifelse(
               as.character(quadratIDUnique) == TRUE,
               paste('<font color="green"><b>', 
                     as.character(quadratIDUnique), 
                     '</b></font>'),
               paste('<font color="red"><b>', 
                     as.character(quadratIDUnique), 
                     '</b></font>')
             )
      )
      
    })


# Group Names Unique Text -------------------------------------------------
    output$groupIDUniqueText <- shiny::renderText({
      
      paste0("Group Names Unique: ",
             ifelse(
               as.character(groupIDUnique) == TRUE,
               paste('<font color="green"><b>', 
                     as.character(groupIDUnique), 
                     '</b></font>'),
               paste('<font color="red"><b>', 
                     as.character(groupIDUnique), 
                     '</b></font>')
             )
      )
      
    })
    

# Cover Values OK Text ----------------------------------------------------
    output$coverValuesOKText <- shiny::renderText({
      
      paste0("Cover Values OK: ",
             ifelse(
               as.character(coverValuesOK) == TRUE,
               paste('<font color="green"><b>', 
                     as.character(coverValuesOK), 
                     '</b></font>'),
               paste('<font color="red"><b>', 
                     as.character(coverValuesOK), 
                     '</b></font>')
             )
      )
      
    })
  

# Ok to proceed Text ------------------------------------------------------
    output$okToProceedText <- shiny::renderText({
      
      paste0("Ok to Proceed: ",
             ifelse(
               as.character(okToProceed) == TRUE,
               paste('<font color="green"><b>', 
                     as.character(okToProceed), 
                     '</b></font>'),
               paste('<font color="red"><b>', 
                     as.character(okToProceed), 
                     '</b></font>')
             )
      )
      
    })
    
  }) |>
    bindEvent(surveyDataValidation_rval(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
  
# Compose Data Object to Return -------------------------------------------
  surveyDataValidatorData_rval <- reactiveVal()
  
  observe({

    surveyDataValidatorData <- list(
      "adjustSpecies" = input$adjustSpecies,
      "reallocateGroups" = input$reallocateGroups,
      "combineDuplicates" = input$combineDuplicates,
      "speciesAdjustmentTable" = rhandsontable::hot_to_r(input$speciesAdjustmentTable),
      "reallocateGroupsTable" = rhandsontable::hot_to_r(input$reallocateGroupsTable),
      "surveyDataValidation" = surveyDataValidation_rval()
    )

    surveyDataValidatorData_rval(surveyDataValidatorData)
    
  }) |>
    bindEvent(input$adjustSpecies,
              input$reallocateGroups,
              input$combineDuplicates,
              input$speciesAdjustmentTable,
              input$reallocateGroupsTable,
              surveyDataValidation_rval(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
  
  
  
  outputOptions(output, "speciesAdjustmentTable", suspendWhenHidden = TRUE)
  
  return(surveyDataValidatorData_rval)
  
}
