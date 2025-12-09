# Regions -----------------------------------------------------------------
region_options <- c("Great Britain (GB-NVC)" = "gbnvc",
                    "Minnesota (MNNPC)" = "mnnpc")

# Regional Availability ---------------------------------------------------
regional_availability <- tibble::tribble(
  ~module, ~gbnvc, ~mnnpc,
  "habCor", TRUE, FALSE,
  "avgEIVs", TRUE, FALSE,
  "taxonNameUpdates", TRUE, FALSE,
  "aggTaxa", FALSE, TRUE
)

# NVC types ---------------------------------------------------------------
nvcType_options <- c("Original",
                     "Calthion",
                     "SOWG")

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
                             "Matrix" = "matrix",
                             "MAVIS" = "mavis",
                             "MNNPC Relevés" = "mnnpc_releves")


# Constancy Conversion ----------------------------------------------------
constancyConversion <- tibble::tibble(
  "Constancy" = c("I", "II", "III", "IV", "V"), 
  "ConstancyNumeric" = c(1, 2, 3, 4, 5),
  "ConstancyPercentMid" = c(10, 30, 50, 70, 90), 
  "ConstancyProportionMid" = c(0.1, 0.3, 0.5, 0.7, 0.9),
  "ConstancyPercentLower" = c(0, 20, 40, 60, 80),
  "ConstancyPercentUpper" = c(20, 40, 60, 80, 100)
)

# Cover Scale Options -----------------------------------------------------
coverScale_options <- c("None" = "none",
                        "Percentage" = "percentage",
                        "Proportional" = "proportional",
                        "Domin" = "domin",
                        "Braun-Blanquet" = "braunBlanquet")

domin_options <- c("+", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")

braunBlanquet_options <- c("+", "1", "2", "3", "4", "5")


# Cover Scale Conversion Values -------------------------------------------
dominConvert <- c("10" = 0.955,
                  "9" = 0.83,
                  "8" = 0.63,
                  "7" = 0.42,
                  "6" = 0.30,
                  "5" = 0.18,
                  "4" = 0.08,
                  "3" = 0.03,
                  "2" = 0.005,
                  "1" = 0.003,
                  "+" = 0.001) |>
  tibble::enframe(name = "Cover", value = "Value")

braunBlanquetConvert <- c("5" = 0.875,
                          "4" = 0.625,
                          "3" = 0.375,
                          "2" = 0.175,
                          "1" = 0.05,
                          "+" = 0.01) |>
  tibble::enframe(name = "Cover", value = "Value")

# Habitat Restriction Options ---------------------------------------------
gbnvc_vc_types_named <- list(
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
  "Vegetation of open habitats (OV)" = "OV",
  "Scottish Oceanic Wet Grasslands (SOWG)" = "SOWG"
)

# Floristic Tables Options ------------------------------------------------
floristicTablesView_options <- c("Single Composed vs VC" = "singleComposedVsVC",
                                 "Multiple Composed" = "multipleComposed")

# Initialise the set of floristic tables to view, will be updated in sidebar_server
floristicTablesSetView_options <- c("all")

matchSpecies_options <- c("No" = "No",
                          "Composed to VC" = "compToVC",
                          "VC to Composed" = "VCToComp")

# Results to View NVC Assignment ------------------------------------------
resultsViewVCAssign_options <- c("Site, Czekanowski" = "vcAssignSiteCzekanowski",
                                 "Group, Czekanowski" = "vcAssignGroupCzekanowski",
                                 "Quadrat, Jaccard" = "vcAssignPlotJaccard")

# Results to View EIVs ----------------------------------------------------
resultsViewEIVs_options <- c("Weighted Mean Hill-Ellenberg Values, by Site" = "weightedMeanHEValuesSite",
                             "Unweighted Mean Hill-Ellenberg Values, by Site" = "unweightedMeanHEValuesSite",
                             "Weighted Mean Hill-Ellenberg Values, by Group" = "weightedMeanHEValuesGroup",
                             "Unweighted Mean Hill-Ellenberg Values, by Group" = "unweightedMeanHEValuesGroup",
                             "Weighted Mean Hill-Ellenberg Values, by Quadrat" = "weightedMeanHEValuesQuadrat",
                             "Unweighted Mean Hill-Ellenberg Values, by Quadrat" = "unweightedMeanHEValuesQuadrat")

# Results to View Diversity -----------------------------------------------
resultsViewDiversity_options <- c("Year"  = "year",
                                  "Group" = "group",
                                  "Quadrat" = "quadrat")

# DCA Axis Selection Options ----------------------------------------------
dcaAxisSelection_options <- c("DCA1 vs DCA2" = "dca1dca2",
                              "DCA1 vs DCA3" = "dca1dca3",
                              "DCA2 vs DCA3" = "dca2dca3")

# DCA Variable Options To Show --------------------------------------------
dcaVars_options <- c("Survey Quadrats" = "surveyQuadrats",
                     "Pseudo-Quadrats" = "pseudoQuadrats",
                     "Reference Space" = "referenceSpace",
                     "Reference Centroids" = "referenceCentroids",
                     "Species" = "species",
                     "Unique Survey Species" = "uniqSurveySpecies",
                     "Hill-Ellenberg" = "hillEllenberg",
                     "Trajectory" = "trajectory")


# Hill-Ellenberg Values ---------------------------------------------------
he_options <- c("Moisture (F)" = "F",
                "Nitrogen (N)" = "N",
                "Reaction (R)" = "R",
                "Light (L)" = "L",
                "Salinity (S)" = "S")

# DCA Survey Quadrat Options ----------------------------------------------
surveyQuadratSelection_options <- c("All" = "all",
                                    "Select Years" = "selectYears",
                                    "Select Groups" = "selectGroups",
                                    "Select Quadrats" = "selectQuadrats")

selectSurveyYears_options <- c()

selectSurveyQuadrats_options <- c()

selectSurveyGroups_options <- c()

# Group Survey Plots Options ----------------------------------------------
groupSurveyPlots_options <- c("No" = "no",
                              "Group" = "group",
                              "Year" = "year")

# Report Options ----------------------------------------------------------
reportOptions_options <- list(`VC Assignment` = c("Site, Czekanowski" = "vcAssignmentResultsSite_Czekanowski",
                                                  "Group, Czekanowski" = "vcAssignmentResultsGroup_Czekanowski",
                                                  "Quadrat, Jaccard" = "vcAssignmentResultsQuadrat_Jaccard"),
                              `Floristic Tables` = c("Site" = "composedFloristicTablesSite",
                                                     "Group" = "composedFloristicTablesGroup"),
                              `Habitat Correspondence` = c("Site" = "habitatCorrespondenceSite"),
                              `Species Frequency` = c("Species Frequency" = "speciesFrequencyTable"),
                              `EIVs (incl. Mean Hill-Ellenberg)` = c("Weighted, Site" = "weightedMeanHEValuesSite",
                                                                     "Unweighted, Site" = "unweightedMeanHEValuesSite",
                                                                     "Weighted, Group" = "weightedMeanHEValuesGroup",
                                                                     "Unweighted, Group" = "unweightedMeanHEValuesGroup",
                                                                     "Weighted, Quadrat" = "weightedMeanHEValuesQuadrat",
                                                                     "Unweighted, Quadrat" = "unweightedMeanHEValuesQuadrat"),
                              `Diversity` = c("Year"  = "year",
                                              "Group" = "group",
                                              "Quadrat" = "quadrat"),
                              `MVA` = c("National" = "mvaNationalReference",
                                        "Local (restricted)" = "mvaLocalReferenceRestricted",
                                        "Local (unrestricted)" = "mvaLocalReferenceUnrestricted"),
                              `Survey Table` = c("Survey Table" = "surveyData")
    )


# Broad habitat names and codes -------------------------------------------
bh_lookup <- tibble::tribble(
  ~Habitat.Name, ~Habitat.Code,
  "Broadleaved, mixed and yew woodland", "1",
  "Coniferous woodland", "2",
  "Boundary and linear features", "3",
  "Arable and horticultural", "4",
  "Improved grassland", "5",
  "Neutral grassland", "6",
  "Calcareous grassland", "7",
  "Acid grassland", "8",
  "Bracken", "9",
  "Dwarf shrub heath", "10",
  "Fen, marsh and swamp", "11",
  "Bog", "12",
  "Standing water and canals", "13",
  "Rivers and streams", "14",
  "Montane habitats", "15",
  "Inland rock", "16",
  "Built-up areas and gardens", "17",
  "Supralittoral rock", "18",
  "Supralittoral sediment", "19",
  # "", "20",
  "Littoral sediment", "21",
  # "", "22",
  "Inshore sublittoral sediment", "23"
)

# Habitat Restriction Prefixes --------------------------------------------
habitatRestrictionPrefixes <- list(
  "W" = c("W"),
  "M" = c("M"),
  "H" = c("H"),
  "MG" = c("MG"),
  "CG" = c("CG"),
  "U" = c("U"),
  "A" = c("A"),
  "S" = c("S"),
  "SM" = c("SM"),
  "SD" = c("SD"),
  "MC" = c("MC"),
  "OV" = c("OV"),
  "SOWG" = c("AgBp", "AgCf", "CaCn", "CnPe", "MG")
)


# Habitat correspondence classifications ----------------------------------
habitat_correspondence_classifications <- readRDS(file = "./inst/extdata/habitat_correspondences.rds") |>
  dplyr::pull(classification) |>
  unique()

# Save all constants as internal data -------------------------------------
usethis::use_data(region_options,
                  regional_availability,
                  nvcType_options,
                  inputMethod_options,
                  example_data_options,
                  dataEntryFormat_options,
                  constancyConversion,
                  coverScale_options,
                  domin_options,
                  braunBlanquet_options,
                  dominConvert,
                  braunBlanquetConvert,
                  gbnvc_vc_types_named,
                  floristicTablesView_options,
                  floristicTablesSetView_options,
                  matchSpecies_options,
                  resultsViewVCAssign_options,
                  resultsViewEIVs_options,
                  resultsViewDiversity_options,
                  dcaAxisSelection_options,
                  dcaVars_options,
                  he_options,
                  surveyQuadratSelection_options,
                  selectSurveyYears_options,
                  selectSurveyQuadrats_options,
                  selectSurveyGroups_options,
                  groupSurveyPlots_options,
                  reportOptions_options,
                  bh_lookup,
                  habitatRestrictionPrefixes,
                  habitat_correspondence_classifications,
                  overwrite = TRUE,
                  internal = TRUE)

# Compress sysdata.rda
tools::resaveRdaFiles(paths = "./R", compress = "xz")
tools::checkRdaFiles(paths = "./R")
