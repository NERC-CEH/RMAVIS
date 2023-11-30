# Retrieve the BSBI taxon concepts dataset
# For use in manually checking errors
bsbiTaxonConcepts <- readr::read_delim("./data/raw_data/bsbi_checklist_BRC_taxonConcepts.csv",
                                       delim = "\t", escape_double = FALSE,
                                       trim_ws = TRUE,
                                       show_col_types = FALSE)

nvc_pquads_uniqSpecies_plants <- nvc_pquads_noMissCodes |>
  dplyr::select(species, BRC) |>
  dplyr::distinct() |>
  dplyr::filter(stringr::str_detect(string = BRC, pattern = "^[9]")) 

# write.table(nvc_pquads_uniqSpecies_plants, file = './data/raw_data/nvc_pquads_uniqSpecies_plants.tsv', quote = FALSE, sep = '\t', row.names = FALSE)

# Read results from BSBI taxon parser
bsbiParserData_1 <- readr::read_delim("data/raw_data/results20231129103853.csv",
                                      delim = "\t", escape_double = FALSE, 
                                      trim_ws = TRUE)

bsbiParserData_1_noErrors <- bsbiParserData_1 |>
  dplyr::filter(is.na(`taxon errors/warnings`))

bsbiParserData_1_prepped <- bsbiParserData_1_noErrors

bsbiParserData_1_Errors <- bsbiParserData_1 |>
  dplyr::filter(!is.na(`taxon errors/warnings`))

typesOfError <- bsbiParserData_1_Errors |>
  dplyr::count(`taxon errors/warnings`)

# Names not matched by BSBI taxon parser
typesOfError_notMatched <- bsbiParserData_1_Errors |>
  dplyr::filter(`taxon errors/warnings` == "Taxon name not matched in DDb")

# Most of these species are tree species with a term for the strata level (g), (s), (c)
# 13 species need checking more closely.
typesOfError_notMatched_strataErrors <- typesOfError_notMatched |>
  dplyr::filter(!(species %in% c("Allium ampeloprasum babingtonii", "Betula seedling/sp", "Brassica oleracea (cultivated)",
                                 "Festuca ovina/rubra (vegetative)", "Hieracium 'indeterminate'", "Hieracium pilosella group",
                                 "Larix sp.(c)", "Potamogeton 'hybrids'", "Quercus seedling/sp",
                                 "Salix repens agg.", "Solanum sarachoides", "Trifolium seedling/sp",
                                 "Ulmus sp. (c)")))

typesOfError_notMatched_strataErrors_all <- typesOfError_notMatched_strataErrors |>
  dplyr::select(species) |>
  dplyr::mutate("speciestoJoin" = stringr::str_replace(string = species, pattern = "\\s\\([a-z]\\)", replacement = "")) |>
  dplyr::rename(species = "speciestoJoin", pQuadSpecies = "species") |>
  dplyr::left_join(bsbiParserData_1_noErrors, by = "species") |>
  dplyr::select(-species) |>
  dplyr::rename("species" = pQuadSpecies)

typesOfError_notMatched_strataErrors_withId <- typesOfError_notMatched_strataErrors_all |>
  dplyr::filter(!is.na(`DDb matched name (id)`))

typesOfError_notMatched_strataErrors_withId_prepped <- typesOfError_notMatched_strataErrors_withId

typesOfError_notMatched_strataErrors_noId <- typesOfError_notMatched_strataErrors_all |>
  dplyr::filter(is.na(`DDb matched name (id)`)) |>
  dplyr::select("speciestoJoin" = species) |>
  dplyr::mutate("speciestoSearch" = stringr::str_replace(string = speciestoJoin, pattern = "\\s\\([a-z]\\)", replacement = "")) |>
  # dplyr::distinct() |>
  dplyr::mutate(
    "speciestoSearch" = 
      dplyr::case_when(
        speciestoJoin == "Aster tripolium (rayed)" ~ "Aster tripolium",
        speciestoJoin == "Aster tripolium (unrayed)" ~ "Aster tripolium",
        speciestoJoin == "Juncus gerardi" ~ "Juncus gerardii",
        TRUE ~ as.character(speciestoSearch)
      )
  )

typesOfError_other_to_Resubmit1 <- typesOfError_notMatched_strataErrors_noId |>
  dplyr::select(speciestoSearch) |>
  dplyr::distinct()

# write.table(typesOfError_other_to_Resubmit1, file = './data/raw_data/typesOfError_other_to_Resubmit1.tsv', quote = FALSE, sep = '\t', row.names = FALSE)

bsbiParserData_2 <- readr::read_delim("data/raw_data/results20231129111717.csv",
                                      delim = "\t", escape_double = FALSE, 
                                      trim_ws = TRUE)

bsbiParserData_2_prepped <- typesOfError_notMatched_strataErrors_noId |>
  dplyr::rename("species" = "speciestoJoin") |>
  dplyr::left_join(nvc_pquads_uniqSpecies_plants, by = "species") |>
  dplyr::left_join(bsbiParserData_2, by = "speciestoSearch") |>
  dplyr::select(-speciestoSearch)

# Manually match the taxon id by checking the BSBI taxon concepts checklist
typesOfError_notMatched_other <- typesOfError_notMatched |>
  dplyr::filter(species %in% c("Allium ampeloprasum babingtonii", "Betula seedling/sp", "Brassica oleracea (cultivated)",
                               "Festuca ovina/rubra (vegetative)", "Hieracium 'indeterminate'", "Hieracium pilosella group",
                               "Larix sp.(c)", "Potamogeton 'hybrids'", "Quercus seedling/sp",
                               "Salix repens agg.", "Solanum sarachoides", "Trifolium seedling/sp",
                               "Ulmus sp. (c)")) |>
  dplyr::mutate(
    "speciesSearch" = 
      dplyr::case_when(
        species == "Allium ampeloprasum babingtonii" ~ "Allium ampeloprasum var. babingtonii",
        species == "Betula seedling/sp" ~ "Betula",
        species == "Brassica oleracea (cultivated)" ~ "Brassica oleracea cv",
        species == "Festuca ovina/rubra (vegetative)" ~ "Festuca",
        species == "Hieracium 'indeterminate'" ~ "Hieracium agg.",
        species == "Hieracium pilosella group" ~ "Hieracium pilosella",
        species == "Larix sp.(c)" ~ "Larix",
        species == "Potamogeton 'hybrids'" ~ "Potamogeton",
        species == "Quercus seedling/sp" ~ "Quercus",
        species == "Salix repens agg." ~ "Salix repens",
        species == "Solanum sarachoides" ~ "Solanum sarrachoides",
        species == "Trifolium seedling/sp" ~ "Trifolium",
        species == "Ulmus sp. (c)" ~ "Ulmus",
        TRUE ~ as.character(species)
      )
  )

typesOfError_other_to_Resubmit2 <- typesOfError_notMatched_other |>
  dplyr::select(speciesSearch)

# write.table(typesOfError_other_to_Resubmit2, file = './data/raw_data/typesOfError_other_to_Resubmit2.tsv', quote = FALSE, sep = '\t', row.names = FALSE)

bsbiParserData_3 <- readr::read_delim("data/raw_data/results20231129104808.csv",
                                      delim = "\t", escape_double = FALSE, 
                                      trim_ws = TRUE)

bsbiParserData_3_prepped <- typesOfError_notMatched_other |>
  dplyr::select(species, speciesSearch, BRC) |>
  dplyr::left_join(bsbiParserData_3, by = "speciesSearch") |>
  dplyr::select(-speciesSearch)

# Other warnings and errors
typesOfError_other <- bsbiParserData_1_Errors |>
  dplyr::filter(`taxon errors/warnings` != "Taxon name not matched in DDb")

typesOfError_other_prepped <- typesOfError_other |>
  dplyr::filter(!(BRC %in% c(9202135.0, 920815.0)))

typesOfError_other_noID <- typesOfError_other |>
  dplyr::filter(is.na(`DDb matched name (id)`))

typesOfError_other_noID_sppFix <- data.frame("species" = c("Vaccinium myrtillus x vitis-idaea (V. x intermediu", 
                                                           "Festuca pratensis x Lolium perenne (x Festulolium loli",
                                                           "Festuca pratensis x Lolium perenne (x Festulolium"),
                                             "speciestoSubmit" = c("Vaccinium myrtillus x vitis-idaea", 
                                                                   "Festuca pratensis x Lolium perenne",
                                                                   "Festuca pratensis x Lolium perenne")) |>
  dplyr::left_join(nvc_pquads_uniqSpecies_plants, by = "species")

# Manually create names to re-submit
typesOfError_other_to_Resubmit3 <- typesOfError_other_noID_sppFix |>
  dplyr::select(speciestoSubmit) |>
  dplyr::distinct()

# write.table(typesOfError_other_to_Resubmit3, file = './data/raw_data/typesOfError_other_to_Resubmit3.tsv', quote = FALSE, sep = '\t', row.names = FALSE)

bsbiParserData_4 <- readr::read_delim("data/raw_data/results20231129110045.csv",
                                      delim = "\t", escape_double = FALSE, 
                                      trim_ws = TRUE)

bsbiParserData_4_prepped <- bsbiParserData_4 |>
  dplyr::left_join(typesOfError_other_noID_sppFix, by = "speciestoSubmit") |>
  dplyr::select(-speciestoSubmit)

# Get concordance data for plants present in assignNVC::NVC_communities but not in 
nvc_comms_uniqSpecies <- assignNVC::NVC_communities |>
  dplyr::select(Species, BRC) |>
  dplyr::distinct() |>
  dplyr::filter(!(Species %in% unique(assignNVC::nvc_pquads$species)))

nvcCommSpp_tosubmit <- nvc_comms_uniqSpecies |>
  dplyr::select(Species) |>
  dplyr::filter(Species != "") |>
  dplyr::mutate(
    "Species" =
      dplyr::case_when(
        Species == "Galium palustre sens.lat." ~ "Galium palustre", #  This taxon does not resolve from the parser as Galium palustre sens.lat.
        TRUE ~ as.character(Species)
      )
  )

# write.table(nvcCommSpp_tosubmit, file = './data/raw_data/nvcCommSpp_tosubmit.tsv', quote = FALSE, sep = '\t', row.names = FALSE)

bsbiParserData_5 <- readr::read_delim("data/raw_data/results20231130071119.csv",
                                      delim = "\t", escape_double = FALSE,
                                      trim_ws = TRUE)

bsbiParserData_5_prepped <- bsbiParserData_5 |>
  dplyr::rename("species" = "Species")|>
  dplyr::mutate(
    "species" =
      dplyr::case_when(
        species == "Galium palustre" ~ "Galium palustre sens.lat.",
        TRUE ~ as.character(species)
      )
  ) |>
  # Add missing BRC_old codes from BRC dictionary
  dplyr::mutate(
    "BRC" =
      dplyr::case_when(
        species == "Nasturtium officinale" ~ 9201347, # Rorippa nasturtium-aquaticum
        species == "Callitriche agg." ~ 09202249,
        species == "Alisma lanceolatum" ~ 092062,
        species == "Galium palustre sens.lat." ~ 0920882,
        species == "Catabrosa aquatica" ~ 920433, # present in assignNVC::ps_quad
        species == "Eleocharis parvula" ~ 920676, # present in assignNVC::ps_quad
        species == "Spartina alterniflora" ~ 9201984,
        TRUE ~ as.numeric(NA)
      )
  )


colnames(bsbiParserData_1_prepped)
colnames(typesOfError_other_prepped)
colnames(typesOfError_notMatched_strataErrors_withId_prepped)
colnames(bsbiParserData_2_prepped)
colnames(bsbiParserData_3_prepped)
colnames(bsbiParserData_4_prepped)
colnames(bsbiParserData_5_prepped)

# Create final dataset for plants.
concordance_plants_draft <- bsbiParserData_1_prepped |>
  dplyr::bind_rows(typesOfError_other_prepped) |>
  dplyr::bind_rows(typesOfError_notMatched_strataErrors_withId_prepped) |>
  dplyr::bind_rows(bsbiParserData_2_prepped) |>
  dplyr::bind_rows(bsbiParserData_3_prepped) |>
  dplyr::bind_rows(bsbiParserData_4_prepped) |>
  dplyr::bind_rows(bsbiParserData_5_prepped) |>
  dplyr::select("assignNVCSpecies" = species,
                "bsbiTaxonId" = `DDb matched name (id)`,
                "bsbiQualifiedTaxonName" = `DDb matched name (full)`,
                "bsbiTaxonName" = `DDb matched name (taxon name)`,
                "BRC_new" = `DDb matched name (brc id)`,
                "TVK" = `TVK of matched name`,
                "BRC_old" = `BRC`) |>
  dplyr::distinct() |>
  # Apply some manual fixes for BRC_new and TVK codes that could not be found via the BSBI taxon name parser, where possible
  # TVK codes found using NHM UK species inventory, found here: https://data.nhm.ac.uk/dataset/uk-species-inventory-simplified-copy
  # BRC_new codes found using the BRC species dictionary
  dplyr::mutate(
    "TVK" = dplyr::case_when(
      assignNVCSpecies == "Bromus racemosus" ~ "NBNSYS0000002585",
      assignNVCSpecies == "Polygala oxyptera" ~ NA, # is a synonym for Polygala vulgaris, the BSBI taxon resolver should have picked this up, replace info below
      assignNVCSpecies == "Rosa villosa agg." ~ "NBNSYS0000156025",
      assignNVCSpecies == "Utricularia vulgaris sens.lat." ~ "NBNSYS0000042407", #  BSBI resolves to NHMSYS0000464750 but this is wrong
      assignNVCSpecies == "Dactylorhiza traunsteineri" ~ "NHMSYS0020116813",
      assignNVCSpecies == "Brassica oleracea (cultivated)" ~ "NBNSYS0000002800",
      TRUE ~ as.character(TVK)
    )
  ) |>
  dplyr::mutate(
    "BRC_new" = dplyr::case_when(
      assignNVCSpecies == "Bromus racemosus" ~ "Vas_271",
      # assignNVCSpecies == "Polygala oxyptera" ~ ,
      assignNVCSpecies == "Rosa villosa agg." ~ "Vas_1722", # Listed under the synonym Rosa mollis agg. according to old BRC code
      assignNVCSpecies == "Utricularia vulgaris sens.lat." ~ "Vas_2132",
      assignNVCSpecies == "Dactylorhiza traunsteineri" ~ "Vas_614",
      assignNVCSpecies == "Brassica oleracea (cultivated)" ~ "Vas_253.2",
      TRUE ~ as.character(BRC_new)
    )
  )

concordance_plants_polygala_oxyptera <- concordance_plants_draft |>
  dplyr::filter(assignNVCSpecies == "Polygala vulgaris") |>
  dplyr::mutate("assignNVCSpecies" = "Polygala oxyptera")

concordance_plants <- concordance_plants_draft |>
  dplyr::filter(assignNVCSpecies != "Polygala oxyptera") |>
  dplyr::bind_rows(concordance_plants_polygala_oxyptera) |>
  dplyr::mutate(
    "proposedSpecies" = 
      dplyr::case_when(
        assignNVCSpecies |> stringr::str_detect(pattern = "(\\w*)(\\s)(\\w*)(\\s)(\\([sgc]\\))") == TRUE ~ assignNVCSpecies,
        TRUE ~ as.character(bsbiTaxonName)
      )
  ) |>
  dplyr::select(-bsbiQualifiedTaxonName, -bsbiTaxonName) |>
  # Remove incorrect duplicate code for Geranium molle
  dplyr::filter(BRC_old != 92041.0)

# Check that the number of plant species in nvc_pquads_uniqSpecies_plants is equal to the length of concordance_plants
# This should be -7
nrow(nvc_pquads_uniqSpecies_plants) - nrow(concordance_plants)

# Missing species - should be an empty data frame
missing_species <- nvc_pquads_uniqSpecies_plants |>
  dplyr::filter(!(species %in% concordance_plants$assignNVCSpecies))

# Check whether there are any missing BRC_new or TVK codes
concordance_plants_missingCodes <- concordance_plants |>
  dplyr::filter(is.na(BRC_new) | is.na(TVK))

# Check whether there is any missing data
concordance_plants_naRows <- concordance_plants |>
  dplyr::filter(is.na(dplyr::if_any(dplyr::everything(), is.na)))

