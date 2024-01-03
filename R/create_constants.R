# Input method options ----------------------------------------------------
inputMethod_options <- c("Manual" = "manual",
                         "Example" = "example",
                         "Upload" = "upload")


# Example data options ----------------------------------------------------
example_data_options <- c("None" = "none", 
                          "Parsonage Down (CG)" = "Parsonage Down",
                          "Whitwell Common (M)" = "Whitwell Common",
                          "Leith Hill Place Wood (W)" = "Leith Hill Place Wood",
                          "Newborough Warren (SD)" = "Newborough Warren")



# Data Entry Format Options -----------------------------------------------
dataEntryFormat_options <- c("Long" = "long",
                             "Wide" = "wide",
                             "Matrix" = "matrix")

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
                            "1" = 0.01
)

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


# Frequency Classes -------------------------------------------------------
freqClasses_numToPerc <- list(
  "I" = "1-20%",
  "II" = "21-40%",
  "III" = "41-60%",
  "IV" = "61-80%",
  "V" = "81-100%"
)

freqClasses_numToName <- list(
  "I" = "Scarce",
  "II" = "Occasional",
  "III" = "Frequent",
  "IV" = "Constant",
  "V" = "Contant"
)

# Cover Method Options ----------------------------------------------------
coverMethod_options <- list(
  "Direct Percentage" = "directPercentage",
  "Domin Class" = "dominCover"
)

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


# Cross-tabulate NVC floristic tables options -----------------------------
matchSpecies_options <- c("No" = "No",
                          "Composed to NVC" = "compToNVC",
                          "NVC to Composed" = "NVCToComp")


# Results to View NVC Assignment ------------------------------------------
resultsViewNVCAssign_options <- c("Site, Pseudo-quadrat" = "nvcAssignSitePseudo",
                                  "Group, Pseudo-quadrat" = "nvcAssignGroupPseudo",
                                  "Quadrat, Pseudo-quadrat" = "nvcAssignQuadratPseudo"#,
                                  # "Site, Czekanowski" = "nvcAssignSiteCzekanowski",
                                  # "Group, Czekanowski" = "nvcAssignGroupCzekanowski"
                                  )

# Results to View EIVs ----------------------------------------------------
resultsViewEIVs_options <- c("Weighted Mean Hill-Ellenberg Values, by Site" = "weightedMeanHEValuesSite",
                             "Unweighted Mean Hill-Ellenberg Values, by Site" = "unweightedMeanHEValuesSite",
                             "Weighted Mean Hill-Ellenberg Values, by Group" = "weightedMeanHEValuesGroup",
                             "Unweighted Mean Hill-Ellenberg Values, by Group" = "unweightedMeanHEValuesGroup",
                             "Weighted Mean Hill-Ellenberg Values, by Quadrat" = "weightedMeanHEValuesQuadrat",
                             "Unweighted Mean Hill-Ellenberg Values, by Quadrat" = "unweightedMeanHEValuesQuadrat"
                             )


# Results to View Diversity -----------------------------------------------
resultsViewDiversity_options <- c("Site Summary Table"  = "diversitySummaryTable",
                                  "Quadrat Diversity Indices Table" = "diversityIndicesTable",
                                  "Species Richness, by Site" = "speciesRichnessSite",
                                  "Species Richness, by Group" = "speciesRichnessGroup",
                                  "Species Richness, by Quadrat" = "speciesRichnessQuadrat")



# DCA Axis Selection Options ----------------------------------------------
dcaAxisSelection_options <- c("DCA1 vs DCA2" = "dca1dca2",
                              "DCA1 vs DCA3" = "dca1dca3",
                              "DCA2 vs DCA3" = "dca2dca3")


# DCA Variable Options To Show --------------------------------------------
dcaVars_options <- c("Survey Quadrats" = "surveyQuadrats",
                     "Pseudo-Quadrats" = "pseudoQuadrats",
                     "Reference Space" = "referenceSpace",
                     "Species" = "species",
                     "Unique Survey Species" = "uniqSurveySpecies",
                     "Hill-Ellenberg" = "hillEllenberg",
                     "Survey Quadrat Change" = "surveyQuadratChange")


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
                     
                     "LS" = c("L", "S")
                     )

ccaVars_options <- c("Moisture (F) x Nitrogen (N)" = "FN",
                     "Moisture (F) x Reaction (R)" = "FR",
                     "Moisture (F) x Light (L)" = "FL",
                     "Moisture (F) x Salinity (S)" = "FS",
                     "Nitrogen (N) x Reaction (R)" = "NR",
                     "Nitrogen (N) x Light (L)" = "NL",
                     "Nitrogen (N) x Salinity (S)" = "NS",
                     "Reaction (R) x Light (L)" = "RL",
                     "Reaction (R) x Salinity (S)" = "RS",
                     "Light (L) x Salinity (S)" = "LS"
                     )



# Global Reference DCA Space Options --------------------------------------
nationalReferenceSpaces_options <- sort(c(c("A", "CG", "H", "M", "MC", "MG", "OV", "S", "SD", "SM", "U", "W"), setdiff(readRDS(file = "./data/bundled_data/nvc_community_codes.rds"), c("SM1", "SM1a", "SM1b"))))


# DCA Survey Quadrat Options ----------------------------------------------
surveyQuadratSelection_options <- c("All" = "all",
                                    "Select Years" = "selectYears",
                                    "Select Groups" = "selectGroups",
                                    "Select Quadrats" = "selectQuadrats")

selectSurveyYears_options <- c()

selectSurveyQuadrats_options <- c()

selectSurveyGroups_options <- c()


# Report Options ----------------------------------------------------------

reportOptions_options <- list(`NVC Assignment` = c("Site" = "nvcAssignmentResultsSite",
                                                   "Group" = "nvcAssignmentResultsGroup",
                                                   "Quadrat" = "nvcAssignmentResultsQuadrat"),
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
                              `Survey Table` = c("Survey Table" = "surveyTable")
    )


# reportOptions_options <- c("NVC Assignment, Site" = "nvcAssignmentResultsSite",
#                            "NVC Assignment, Group" = "nvcAssignmentResultsGroup",
#                            "NVC Assignment, Quadrat" = "nvcAssignmentResultsQuadrat",
#                            "Floristic Tables, Site" = "composedFloristicTablesSite",
#                            "Floristic Tables, Group" = "composedFloristicTablesGroup",
#                            "Species Frequency" = "speciesFrequencyTable",
#                            "Weighted Mean Hill-Ellenberg Values, Site" = "weightedMeanHEValuesSite",
#                            "Unweighted Mean Hill-Ellenberg Values, Site" = "unweightedMeanHEValuesSite",
#                            "Weighted Mean Hill-Ellenberg Values, Group" = "weightedMeanHEValuesGroup",
#                            "Unweighted Mean Hill-Ellenberg Values, Group" = "unweightedMeanHEValuesGroup",
#                            "Weighted Mean Hill-Ellenberg Values, Quadrat" = "weightedMeanHEValuesQuadrat",
#                            "Unweighted Mean Hill-Ellenberg Values, Quadrat" = "unweightedMeanHEValuesQuadrat",
#                            "MVA, National" = "mvaNationalReference",
#                            "MVA, Local (restricted)" = "mvaLocalReferenceRestricted",
#                            "MVA, Local (unrestricted)" = "mvaLocalReferenceUnrestricted",
#                            "Survey Table" = "surveyTable"
#                            )

