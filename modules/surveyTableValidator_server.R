surveyTableValidator <- function(input, output, session, surveyTable, sidebar_options) {
  
  ns <- session$ns
  
# Initialise Table to Replace Species Not In Accepted List ----------------
  speciesCorrectionTable_init <- data.frame("Species.Submitted" = character(),
                                            "Species.Corrected" = character(),
                                            "Species.Ignore" = logical())
  
  speciesCorrectionTable_rval <- reactiveVal(speciesCorrectionTable_init)
  
  output$speciesCorrectionTable <- rhandsontable::renderRHandsontable({
    
    speciesCorrectionTable <- rhandsontable::rhandsontable(data = speciesCorrectionTable_init,
                                                           rowHeaders = NULL,
                                                           width = "100%"#,
                                                           # overflow = "visible",
                                                           # stretchH = "all"
                                                           ) |>
      rhandsontable::hot_col(col = colnames(speciesCorrectionTable_init), halign = "htCenter") |>
      rhandsontable::hot_col(
        col = "Species.Submitted",
        readOnly = TRUE,
      ) |>
      rhandsontable::hot_col(
        col = "Species.Corrected",
        readOnly = FALSE,
        type = "dropdown",
        source = speciesNames,
        strict = TRUE,
        default = as.character(NA_character_)
      ) |>
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
      htmlwidgets::onRender("
      function(el, x) {
        var hot = this.hot
        $('a[data-value=\"validateSurveyTable\"').on('click', function(){
          setTimeout(function() {hot.render();}, 0);
        })
      }")
    
    return(speciesCorrectionTable)
    
  })
  

# Perform Validation Checks on surveyTable --------------------------------
  surveyTableValidation_rval <- reactiveVal()
  
  observe({
    
    surveyTable <- surveyTable()
    
    # Check all species are accepted
    surveyTable_speciesInAccepted <- isTRUE(all(unique(surveyTable$Species) %in% speciesNames))
    
    # Check which species are not accepted
    surveyTable_speciesNotAccepted <- setdiff(unique(surveyTable$Species), speciesNames)
    
    # Check whether any cover estimates are supplied
    surveyTable_coverSupplied <- isTRUE(!is.na(unique(surveyTable$Cover)))
    
    # Check whether all cover estimates are supplied
    surveyTable_coverSuppliedAll <- isTRUE(any(is.na(surveyTable$Cover)))
    
    # Check whether there is any missing data in the Year column
    surveyTable_yearComplete <- isTRUE(any(!is.na(surveyTable$Year)))
    
    # Check whether there is any missing data in the Group column
    surveyTable_groupComplete <- isTRUE(any(!is.na(surveyTable$Group)))
    
    # Check whether there is any missing data in the Quadrat column
    surveyTable_quadratComplete <- isTRUE(any(!is.na(surveyTable$Quadrat)))
    
    # Check whether there is any missing data in the Species column
    surveyTable_speciesComplete <- isTRUE(any(!is.na(surveyTable$Species)))
    
    # assign(x = "surveyTable", value = surveyTable, envir = .GlobalEnv)
    
    # Check whether there are any species-quadrat double-entries
    surveyTable_speciesQuadratDuplicates <- surveyTable |>
      dplyr::select(Year, Group, Quadrat, Species) |>
      dplyr::group_by(Year, Group, Quadrat, Species) |>
      dplyr::filter(dplyr::n() > 1) |>
      dplyr::ungroup()
    
    # print(surveyTable_speciesQuadratDuplicates)
    
    surveyTable_speciesQuadratDuplicates <- isTRUE(nrow(surveyTable_speciesQuadratDuplicates) == 0)
    urveyTable_speciesQuadratDuplicates <- surveyTable_speciesQuadratDuplicates
    
    # Check whether each quadrat ID is unique
    surveyTable_quadratIDUnique <- surveyTable |>
      dplyr::select(Year, Group, Quadrat) |>
      dplyr::distinct() |>
      dplyr::group_by(Year, Group, Quadrat) |>
      dplyr::filter(dplyr::n() > 1) |>
      dplyr::pull(Quadrat)
    
    surveyTable_quadratIDUnique <- isTRUE(length(surveyTable_quadratIDUnique) == 0)
    surveyTable_quadratIDDuplicates <- surveyTable_quadratIDUnique
    
    # Check whether each group ID is unique
    surveyTable_groupIDUnique <- surveyTable |>
      dplyr::select(Year, Group) |>
      dplyr::distinct() |>
      dplyr::group_by(Year, Group) |>
      dplyr::filter(dplyr::n() > 1) |>
      dplyr::pull(Group)
    
    surveyTable_groupIDUnique <- isTRUE(length(surveyTable_groupIDUnique) == 0)
    surveyTable_groupIDDuplicates <- surveyTable_groupIDUnique
    
    # Create a dataframe containing the number of quadrats, per group, per year.
    
    
    # Check whether the analysis is ok to proceed
    okToProceed <- isTRUE(all(surveyTable_speciesInAccepted, surveyTable_yearComplete, 
                              surveyTable_groupComplete, surveyTable_quadratComplete,
                              surveyTable_speciesQuadratDuplicates,
                              surveyTable_speciesComplete, surveyTable_quadratIDUnique,
                              surveyTable_groupIDUnique))
    
    
    # Create list of validation checkes
    surveyTableValidation <- list(
      "speciesInAccepted" = surveyTable_speciesInAccepted,
      "speciesNotAccepted" = surveyTable_speciesNotAccepted,
      "coverSupplied" = surveyTable_coverSupplied,
      "coverSuppliedAll" = surveyTable_coverSuppliedAll,
      "yearComplete" = surveyTable_yearComplete,
      "groupComplete" = surveyTable_groupComplete,
      "quadratComplete" = surveyTable_quadratComplete,
      "speciesComplete" = surveyTable_speciesComplete,
      "speciesQuadratDuplicates" = surveyTable_speciesQuadratDuplicates,
      "quadratIDUnique" = surveyTable_quadratIDUnique,
      "quadratIDDuplicates" = surveyTable_quadratIDDuplicates,
      "groupIDUnique" = surveyTable_groupIDUnique,
      "groupIDDuplicates" = surveyTable_groupIDDuplicates,
      # "" = ,
      # "" = ,
      # "" = ,
      # "" = ,
      "okToProceed" = okToProceed
    )
    
    # print(surveyTableValidation)
    
    surveyTableValidation_rval(surveyTableValidation)
    
  }) |>
    bindEvent(surveyTable(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)

# Update Table to Replace Species Not In Accepted List --------------------
  observe({
    
    req(surveyTableValidation_rval())
    
    surveyTableValidation <- surveyTableValidation_rval()
    
    # print(surveyTableValidation$speciesNotAccepted)
    
    if(length(surveyTableValidation$speciesNotAccepted) > 0){
      
      speciesCorrectionTable <- data.frame("Species.Submitted" = surveyTableValidation$speciesNotAccepted,
                                           "Species.Corrected" = as.character(NA_character_),
                                           "Species.Ignore" = FALSE)
      
      output$speciesCorrectionTable <- rhandsontable::renderRHandsontable({
        
        speciesCorrectionTable <- rhandsontable::rhandsontable(data = speciesCorrectionTable,
                                                               rowHeaders = NULL,
                                                               width = "100%"#,
                                                               # overflow = "visible",
                                                               # stretchH = "all"
        ) |>
          rhandsontable::hot_col(col = colnames(speciesCorrectionTable), halign = "htCenter") |>
          rhandsontable::hot_col(
            col = "Species.Submitted",
            readOnly = TRUE,
          ) |>
          rhandsontable::hot_col(
            col = "Species.Corrected",
            readOnly = FALSE,
            type = "dropdown",
            source = speciesNames,
            strict = TRUE,
            default = as.character(NA_character_)
          ) |>
          rhandsontable::hot_validate_character(cols = "Species.Corrected", choices = speciesNames) |>
          rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
          rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
          htmlwidgets::onRender("
          function(el, x) {
            var hot = this.hot
            $('a[data-value=\validateSurveyTable\"').on('click', function(){
              setTimeout(function() {hot.render();}, 0);
            })
          }")
        
        return(speciesCorrectionTable)
        
      })
      
    }
     
  }) |>
    bindEvent(surveyTableValidation_rval(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
  

# Create Text Validation Outputs ------------------------------------------
  observe({
    
    surveyTableValidation <- surveyTableValidation_rval()
    
    speciesInAccepted <- surveyTableValidation$speciesInAccepted
    speciesNotAccepted <- surveyTableValidation$speciesNotAccepted
    coverSupplied <- surveyTableValidation$coverSupplied
    coverSuppliedAll <- surveyTableValidation$coverSuppliedAll
    yearComplete <- surveyTableValidation$yearComplete
    groupComplete <- surveyTableValidation$groupComplete
    quadratComplete <- surveyTableValidation$quadratComplete
    speciesComplete <- surveyTableValidation$speciesComplete
    speciesQuadratDuplicates <- surveyTableValidation$speciesQuadratDuplicates
    quadratIDUnique <- surveyTableValidation$quadratIDUnique
    quadratIDDuplicates <- surveyTableValidation$quadratIDDuplicates
    groupIDUnique <- surveyTableValidation$groupIDUnique
    groupIDDuplicates <- surveyTableValidation$groupIDDuplicates
    okToProceed <- surveyTableValidation$okToProceed
    
    

# All Species in Accepted List Text ---------------------------------------
    output$speciesInAcceptedText <- shiny::renderUI({
      
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
    

# Cover Supplied Text -----------------------------------------------------
    output$coverSuppliedText <- shiny::renderUI({
      
      paste0("Are Cover Estimate Values Provided: ",
             ifelse(
               as.character(coverSupplied) == TRUE,
               paste('<font color="green"><b>', 
                     as.character(coverSupplied), 
                     '</b></font>'),
               paste('<font color="red"><b>', 
                     as.character(coverSupplied), 
                     '</b></font>')
             )
      )
      
      
    })


# Year Column Complete Text -----------------------------------------------
    output$yearCompleteText <- shiny::renderUI({
      
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
    output$groupCompleteText <- shiny::renderUI({
      
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
    output$quadratCompleteText <- shiny::renderUI({
      
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
    output$speciesCompleteText <- shiny::renderUI({
      
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
    output$speciesQuadratDuplicatesText <- shiny::renderUI({
      
      paste0("No Duplicate Species Within Quadrats: ",
             ifelse(
               as.character(speciesQuadratDuplicates) == TRUE,
               paste('<font color="green"><b>', 
                     as.character(speciesQuadratDuplicates), 
                     '</b></font>'),
               paste('<font color="red"><b>', 
                     as.character(speciesQuadratDuplicates), 
                     '</b></font>')
             )
      )
      
    })
    
# Quadrat Names Unique Text -----------------------------------------------
    output$quadratIDUniqueText <- shiny::renderUI({
      
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
    output$groupIDUniqueText <- shiny::renderUI({
      
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
  

# Ok to proceed Text ------------------------------------------------------
    output$okToProceedText <- shiny::renderUI({
      
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
    bindEvent(surveyTableValidation_rval(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
  

  outputOptions(output, "speciesCorrectionTable", suspendWhenHidden = FALSE)
  
  return(surveyTableValidation_rval)
  
}
