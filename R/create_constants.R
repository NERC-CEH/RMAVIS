# Input method options ----------------------------------------------------
inputMethod_options <- c("Manual" = "manual",
                         "Example" = "example",
                         "Upload" = "upload")


# Example data options ----------------------------------------------------
example_data_options <- c("None" = "none", 
                          "Parsonage Down (CG)" = "Parsonage Down",
                          "Whitwell Common (M)" = "Whitwell Common",
                          "Leith Hill Place Wood (W)" = "Leith Hill Place Wood")



# Data Entry Format Options -----------------------------------------------
dataEntryFormat_options <- c("Long" = "long",
                             "Wide" = "wide")

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
crossTabulate_options <- c("No" = "No",
                           "Composed to NVC" = "compToNVC",
                           "NVC to Composed" = "NVCToComp")


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


# DCA Variable Options To Show --------------------------------------------
dcaVars_options <- c("Survey Quadrats" = "surveyQuadrats",
                     "Pseudo-Quadrats" = "pseudoQuadrats",
                     "Reference Space" = "referenceSpace",
                     "Species" = "species",
                     "Unique Survey Species" = "uniqSurveySpecies",
                     "Hill-Ellenberg" = "hillEllenberg",
                     "Survey Quadrat Change" = "surveyQuadratChange")


# CCA options -------------------------------------------------------------
ccaVars_options <- c("Moisture (F)" = "F",
                     "Nitrogen (N)" = "N",
                     "Reaction (R)" = "R",
                     "Light (L)" = "L",
                     "Salinity (S)" = "S")



# Global Reference DCA Space Options --------------------------------------
globalReferenceSpaces_options <- sort(c(c("A", "CG", "H", "M", "MC", "MG", "OV", "S", "SD", "SM", "U", "W"), setdiff(readRDS(file = "./data/bundled_data/nvc_community_codes.rds"), c("SM1", "SM1a", "SM1b"))))


# DCA Survey Quadrat Options ----------------------------------------------
surveyQuadratSelection_options <- c("All" = "all",
                                    "Select Years" = "selectYears",
                                    "Select Groups" = "selectGroups",
                                    "Select Quadrats" = "selectQuadrats")

selectSurveyYears_options <- c()

selectSurveyQuadrats_options <- c()

selectSurveyGroups_options <- c()
