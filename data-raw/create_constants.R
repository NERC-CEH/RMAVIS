# Taxonomic Backbone Methods ----------------------------------------------
taxonomicBackboneMethod_options <- c("Bundled" = "bundled",
                                     "Upload" = "upload",
                                     "Kew WCVP" = "wcvp",
                                     "Syntopic Tables" = "syntopicTables")
usethis::use_data(taxonomicBackboneMethod_options, overwrite = TRUE, internal = TRUE)

# Input method options ----------------------------------------------------
inputMethod_options <- c("Manual" = "manual",
                         "Example" = "example",
                         "Upload" = "upload")
usethis::use_data(inputMethod_options, overwrite = TRUE, internal = TRUE)

# Example data options ----------------------------------------------------
example_data_options <- c("None" = "none", 
                          "Parsonage Down (CG)" = "Parsonage Down",
                          "Whitwell Common (M)" = "Whitwell Common",
                          "Leith Hill Place Wood (W)" = "Leith Hill Place Wood",
                          "Newborough Warren (SD)" = "Newborough Warren")
usethis::use_data(example_data_options, overwrite = TRUE, internal = TRUE)

# Data Entry Format Options -----------------------------------------------
dataEntryFormat_options <- c("Long" = "long",
                             "Wide" = "wide",
                             "Matrix" = "matrix",
                             "MAVIS" = "mavis")
usethis::use_data(dataEntryFormat_options, overwrite = TRUE, internal = TRUE)

# Domin Cover -------------------------------------------------------------
dominCoverMidPointPerc <- c("10" = 0.955,
                            "9" = 0.83,
                            "8" = 0.58,
                            "7" = 0.42,
                            "6" = 0.295,
                            "5" = 0.18,
                            "4" = 0.07,
                            "3" = 0.04,
                            "2" = 0.025,
                            "1" = 0.01)
usethis::use_data(dominCoverMidPointPerc, overwrite = TRUE, internal = TRUE)

dominCoverVals <- c("91-100%" = 10,
                    "76-90%" = 9,
                    "51-75%" = 8,
                    "34-50%" = 7,
                    "26-33%" = 6,
                    "11-25%" = 5,
                    "4-10%" = 4,
                    "<4% (many individuals)" = 3,
                    "<4% (several individuals)" = 2,
                    "<4% (few individuals)" = 1)
usethis::use_data(dominCoverVals, overwrite = TRUE, internal = TRUE)

dominCoverValsRev <- c("10" = "91-100%",
                       "9" = "76-90%",
                       "8" = "51-75%",
                       "7" = "34-50%",
                       "6" = "26-33%",
                       "5" = "11-25%",
                       "4" = "4-10%",
                       "3" = "<4% (many individuals)",
                       "2" = "<4% (several individuals)",
                       "1" = "<4% (few individuals)")
usethis::use_data(dominCoverValsRev, overwrite = TRUE, internal = TRUE)

# Frequency Classes -------------------------------------------------------
freqClasses_numToPerc <- list(
  "I" = "1-20%",
  "II" = "21-40%",
  "III" = "41-60%",
  "IV" = "61-80%",
  "V" = "81-100%"
)
usethis::use_data(freqClasses_numToPerc, overwrite = TRUE, internal = TRUE)

freqClasses_numToName <- list(
  "I" = "Scarce",
  "II" = "Occasional",
  "III" = "Frequent",
  "IV" = "Constant",
  "V" = "Contant"
)
usethis::use_data(freqClasses_numToName, overwrite = TRUE, internal = TRUE)

# Cover Method Options ----------------------------------------------------
coverMethod_options <- list(
  "Direct Percentage" = "directPercentage",
  "Domin Class" = "dominCover"
)
usethis::use_data(coverMethod_options, overwrite = TRUE, internal = TRUE)

# Habitat Restriction Options ---------------------------------------------
habitatRestriction_options <- list(
  "Woodland and scrub (W)" = "W",
  "Mires (M)" = "M",
  "Heaths (H)" = "H",
  "Mesotrophic grasslands (MG)" = "MG",
  "Calcicolous grasslands (CG)" = "CG",
  "Calcifugous grasslands and montane communities (U)" = "U",
  "Aquatic communities (A)" = "A",
  "Swamps and tall-herb fens (S)" = "S",
  "Salt-marsh communities (SM)" = "SM",
  "Shingle, strandline and sand-dune communities (SD)" = "SD",
  "Maritime cliff communities (MC)" = "MC",
  "Vegetation of open habitats (OV)" = "OV"
)
usethis::use_data(habitatRestriction_options, overwrite = TRUE, internal = TRUE)

# Floristic Tables Options ------------------------------------------------
floristicTablesView_options <- c("Single Composed vs NVC" = "singleComposedVsNVC",
                                 "Multiple Composed" = "multipleComposed")
usethis::use_data(floristicTablesView_options, overwrite = TRUE, internal = TRUE)

# Initialise the set of floristic tables to view, will be updated in sidebar_server
floristicTablesSetView_options <- c("all")
usethis::use_data(floristicTablesSetView_options, overwrite = TRUE, internal = TRUE)

matchSpecies_options <- c("No" = "No",
                          "Composed to NVC" = "compToNVC",
                          "NVC to Composed" = "NVCToComp")
usethis::use_data(matchSpecies_options, overwrite = TRUE, internal = TRUE)

# Results to View NVC Assignment ------------------------------------------
resultsViewNVCAssign_options <- c("Site, Czekanowski" = "nvcAssignSiteCzekanowski",
                                  "Group, Czekanowski" = "nvcAssignGroupCzekanowski",
                                  "Quadrat, Jaccard" = "nvcAssignPlotJaccard")
usethis::use_data(resultsViewNVCAssign_options, overwrite = TRUE, internal = TRUE)

# Results to View EIVs ----------------------------------------------------
resultsViewEIVs_options <- c("Weighted Mean Hill-Ellenberg Values, by Site" = "weightedMeanHEValuesSite",
                             "Unweighted Mean Hill-Ellenberg Values, by Site" = "unweightedMeanHEValuesSite",
                             "Weighted Mean Hill-Ellenberg Values, by Group" = "weightedMeanHEValuesGroup",
                             "Unweighted Mean Hill-Ellenberg Values, by Group" = "unweightedMeanHEValuesGroup",
                             "Weighted Mean Hill-Ellenberg Values, by Quadrat" = "weightedMeanHEValuesQuadrat",
                             "Unweighted Mean Hill-Ellenberg Values, by Quadrat" = "unweightedMeanHEValuesQuadrat")
usethis::use_data(resultsViewEIVs_options, overwrite = TRUE, internal = TRUE)

# Results to View Diversity -----------------------------------------------
resultsViewDiversity_options <- c("Site Summary Table"  = "diversitySummaryTable",
                                  "Quadrat Diversity Indices Table" = "diversityIndicesTable",
                                  "Species Richness, by Site" = "speciesRichnessSite",
                                  "Species Richness, by Group" = "speciesRichnessGroup",
                                  "Species Richness, by Quadrat" = "speciesRichnessQuadrat")
usethis::use_data(resultsViewDiversity_options, overwrite = TRUE, internal = TRUE)

# DCA Axis Selection Options ----------------------------------------------
dcaAxisSelection_options <- c("DCA1 vs DCA2" = "dca1dca2",
                              "DCA1 vs DCA3" = "dca1dca3",
                              "DCA2 vs DCA3" = "dca2dca3")
usethis::use_data(dcaAxisSelection_options, overwrite = TRUE, internal = TRUE)

# DCA Variable Options To Show --------------------------------------------
dcaVars_options <- c("Survey Quadrats" = "surveyQuadrats",
                     "Pseudo-Quadrats" = "pseudoQuadrats",
                     "Reference Space" = "referenceSpace",
                     "Reference Centroids" = "referenceCentroids",
                     "Species" = "species",
                     "Unique Survey Species" = "uniqSurveySpecies",
                     "Hill-Ellenberg" = "hillEllenberg",
                     "Survey Quadrat Change" = "surveyQuadratChange")
usethis::use_data(dcaVars_options, overwrite = TRUE, internal = TRUE)

# CCA options -------------------------------------------------------------
ccaVars_vals <- list("FN" = c("F", "N"),
                     "FR" = c("F", "R"),
                     "FL" = c("F", "L"),
                     "FS" = c("F", "S"),
                     "NR" = c("N", "R"),
                     "NL" = c("N", "L"),
                     "NS" = c("N", "S"),
                     "RL" = c("R", "L"),
                     "RS" = c("R", "S"),
                     "LS" = c("L", "S"))
usethis::use_data(ccaVars_vals, overwrite = TRUE, internal = TRUE)

ccaVars_options <- c("Moisture (F) x Nitrogen (N)" = "FN",
                     "Moisture (F) x Reaction (R)" = "FR",
                     "Moisture (F) x Light (L)" = "FL",
                     "Moisture (F) x Salinity (S)" = "FS",
                     "Nitrogen (N) x Reaction (R)" = "NR",
                     "Nitrogen (N) x Light (L)" = "NL",
                     "Nitrogen (N) x Salinity (S)" = "NS",
                     "Reaction (R) x Light (L)" = "RL",
                     "Reaction (R) x Salinity (S)" = "RS",
                     "Light (L) x Salinity (S)" = "LS")
usethis::use_data(ccaVars_options, overwrite = TRUE, internal = TRUE)

# Global Reference DCA Space Options --------------------------------------
nationalReferenceSpaces_options <- sort(c(c("A", "CG", "H", "M", "MC", "MG", "OV", "S", "SD", "SM", "U", "W"), setdiff(readRDS(file = "./inst/extdata/nvc_community_codes.rds"), c("SM1", "SM1a", "SM1b"))))
usethis::use_data(nationalReferenceSpaces_options, overwrite = TRUE, internal = TRUE)

# DCA Survey Quadrat Options ----------------------------------------------
surveyQuadratSelection_options <- c("All" = "all",
                                    "Select Years" = "selectYears",
                                    "Select Groups" = "selectGroups",
                                    "Select Quadrats" = "selectQuadrats")
usethis::use_data(surveyQuadratSelection_options, overwrite = TRUE, internal = TRUE)

selectSurveyYears_options <- c()
usethis::use_data(selectSurveyYears_options, overwrite = TRUE, internal = TRUE)

selectSurveyQuadrats_options <- c()
usethis::use_data(selectSurveyQuadrats_options, overwrite = TRUE, internal = TRUE)

selectSurveyGroups_options <- c()
usethis::use_data(selectSurveyGroups_options, overwrite = TRUE, internal = TRUE)

# Group Survey Plots Options ----------------------------------------------
groupSurveyPlots_options <- c("No" = "no",
                              "Group" = "group",
                              "Year" = "year")
usethis::use_data(groupSurveyPlots_options, overwrite = TRUE, internal = TRUE)

# Report Options ----------------------------------------------------------
reportOptions_options <- list(`NVC Assignment` = c("Site, Czekanowski" = "nvcAssignmentResultsSite_Czekanowski",
                                                   "Group, Czekanowski" = "nvcAssignmentResultsGroup_Czekanowski",
                                                   "Quadrat, Jaccard" = "nvcAssignmentResultsQuadrat_Jaccard"),
                              `Floristic Tables` = c("Site" = "composedFloristicTablesSite",
                                                     "Group" = "composedFloristicTablesGroup"),
                              `Species Frequency` = c("Species Frequency" = "speciesFrequencyTable"),
                              `EIVs (incl. Mean Hill-Ellenberg)` = c("Weighted, Site" = "weightedMeanHEValuesSite",
                                                                     "Unweighted, Site" = "unweightedMeanHEValuesSite",
                                                                     "Weighted, Group" = "weightedMeanHEValuesGroup",
                                                                     "Unweighted, Group" = "unweightedMeanHEValuesGroup",
                                                                     "Weighted, Quadrat" = "weightedMeanHEValuesQuadrat",
                                                                     "Unweighted, Quadrat" = "unweightedMeanHEValuesQuadrat"),
                              `Diversity` = c("Summary" = "diversitySummary",
                                              "Quadrat Indices" = "diversityIndices",
                                              "Richness, Site" = "speciesRichnessSite",
                                              "Richness, Group" = "speciesRichnessGroup",
                                              "Richness, Quadrat" = "speciesRichnessQuadrat"),
                              `MVA` = c("National" = "mvaNationalReference",
                                        "Local (restricted)" = "mvaLocalReferenceRestricted",
                                        "Local (unrestricted)" = "mvaLocalReferenceUnrestricted"),
                              `Survey Table` = c("Survey Table" = "surveyData")
    )
usethis::use_data(reportOptions_options, overwrite = TRUE, internal = TRUE)
