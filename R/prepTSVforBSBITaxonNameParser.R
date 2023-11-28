# Retrieve the BSBI taxon concepts dataset
# For use in manually checking errors
bsbiTaxonConcepts <- readr::read_delim("./data/raw_data/bsbi_checklist_BRC_taxonConcepts.csv",
                                       delim = "\t", escape_double = FALSE,
                                       trim_ws = TRUE,
                                       show_col_types = FALSE)

nvc_pquads_uniqSpecies_algae <- assignNVC::nvc_pquads |>
  dplyr::select(species, BRC) |>
  dplyr::distinct() |>
  dplyr::filter(stringr::str_detect(string = BRC, pattern = "^[2]")) 

nvc_pquads_uniqSpecies_lichens <- assignNVC::nvc_pquads |>
  dplyr::select(species, BRC) |>
  dplyr::distinct() |>
  dplyr::filter(stringr::str_detect(string = BRC, pattern = "^[5]")) 

nvc_pquads_uniqSpecies_charophytes <- assignNVC::nvc_pquads |>
  dplyr::select(species, BRC) |>
  dplyr::distinct() |>
  dplyr::filter(stringr::str_detect(string = BRC, pattern = "^[7]")) 

nvc_pquads_uniqSpecies_bryophytes <- assignNVC::nvc_pquads |>
  dplyr::select(species, BRC) |>
  dplyr::distinct() |>
  dplyr::filter(stringr::str_detect(string = BRC, pattern = "^[8]")) 

nvc_pquads_uniqSpecies_plants <- assignNVC::nvc_pquads |>
  dplyr::select(species, BRC) |>
  dplyr::distinct() |>
  dplyr::filter(stringr::str_detect(string = BRC, pattern = "^[9]")) 

write.table(nvc_pquads_uniqSpecies_plants, file = './data/raw_data/nvc_pquads_uniqSpecies_plants.tsv', quote = FALSE, sep = '\t', row.names = FALSE)

# Read results from BSBI taxon parser
bsbiParserData <- readr::read_delim("data/raw_data/results20231128174957.csv",
                                    delim = "\t", escape_double = FALSE, 
                                    trim_ws = TRUE)

bsbiParserData_noErrors <- bsbiParserData |>
  dplyr::filter(is.na(`taxon errors/warnings`))

bsbiParserData_Errors <- bsbiParserData |>
  dplyr::filter(!is.na(`taxon errors/warnings`))

typesOfError <- bsbiParserData_Errors |>
  dplyr::count(`taxon errors/warnings`)



# Names not matched by BSBI taxon parser
typesOfError_notMatched <- bsbiParserData_Errors |>
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
  dplyr::left_join(bsbiParserData_noErrors, by = "species") |>
  dplyr::select(-species) |>
  dplyr::rename("species" = pQuadSpecies)

typesOfError_notMatched_strataErrors_withId <- typesOfError_notMatched_strataErrors_all |>
  dplyr::filter(!is.na(`DDb matched name (id)`))

typesOfError_notMatched_strataErrors_noId <- typesOfError_notMatched_strataErrors_all |>
  dplyr::filter(is.na(`DDb matched name (id)`)) |>
  dplyr::select("speciestoJoin" = species) |>
  dplyr::mutate("speciestoJoin" = stringr::str_replace(string = speciestoJoin, pattern = "\\s\\([a-z]\\)", replacement = "")) |>
  dplyr::distinct()

typesOfError_other_to_Resubmit1 <- typesOfError_notMatched_strataErrors_noId

write.table(typesOfError_other_to_Resubmit1, file = './data/raw_data/typesOfError_other_to_Resubmit1.tsv', quote = FALSE, sep = '\t', row.names = FALSE)

bsbiParserData_1 <- readr::read_delim("data/raw_data/results20231128192927.csv",
                                      delim = "\t", escape_double = FALSE, 
                                      trim_ws = TRUE)

foo <- typesOfError_notMatched_strataErrors_all |>
  dplyr::filter(is.na(`DDb matched name (id)`)) |>
  dplyr::mutate("speciestoJoin" = stringr::str_replace(string = species, pattern = "\\s\\([a-z]\\)", replacement = "")) |>
  dplyr::select(species, speciestoJoin)
  

bsbiParserData_1_prepped <- typesOfError_notMatched_strataErrors_all |>
  dplyr::filter(is.na(`DDb matched name (id)`)) |>
  dplyr::mutate("speciestoJoin" = stringr::str_replace(string = species, pattern = "\\s\\([a-z]\\)", replacement = "")) |>
  dplyr::select(species, speciestoJoin) |>
  dplyr::left_join(bsbiParserData_1, by = "speciestoJoin")

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

write.table(typesOfError_other_to_Resubmit2, file = './data/raw_data/typesOfError_other_to_Resubmit2.tsv', quote = FALSE, sep = '\t', row.names = FALSE)

bsbiParserData_2 <- readr::read_delim("data/raw_data/results20231128190447.csv",
                                      delim = "\t", escape_double = FALSE, 
                                      trim_ws = TRUE)

bsbiParserData_2_prepped <- typesOfError_notMatched_other |>
  dplyr::select(species, speciesSearch) |>
  dplyr::left_join(bsbiParserData_2, by = "speciesSearch") |>
  dplyr::select(-speciesSearch)

# Other warnings and errors
typesOfError_other <- bsbiParserData_Errors |>
  dplyr::filter(`taxon errors/warnings` != "Taxon name not matched in DDb")

# After manual inspection it appears as if only two taxa need re-submitting to
# the BSBI parser
typesOfError_other_to_Resubmit3 <- data.frame("species" = c("Vaccinium myrtillus x vitis-idaea", 
                                                            "Festuca pratensis x Lolium perenne"))

write.table(typesOfError_other_to_Resubmit3, file = './data/raw_data/typesOfError_other_to_Resubmit3.tsv', quote = FALSE, sep = '\t', row.names = FALSE)

bsbiParserData_3 <- readr::read_delim("data/raw_data/results20231128181934.csv",
                                      delim = "\t", escape_double = FALSE, 
                                      trim_ws = TRUE)

bsbiParserData_3_prepped <- bsbiParserData_3 |>
  dplyr::mutate(
    "species" = dplyr::case_when(
      species == "Vaccinium myrtillus x vitis-idaea" ~ "Vaccinium myrtillus x vitis-idaea (V. x intermediu",
      species == "Festuca pratensis x Lolium perenne" ~ "Festuca pratensis x Lolium perenne (x Festulolium",
      
    )
  )


# Create final dataset for plants.
concordance_plants <- bsbiParserData_noErrors |>
  dplyr::bind_rows(typesOfError_other) |>
  dplyr::filter(!(BRC %in% c(9202135.0, 920815.0))) |>
  dplyr::bind_rows(bsbiParserData_2_prepped) |>
  dplyr::bind_rows(bsbiParserData_3) |>
  dplyr::bind_rows(typesOfError_notMatched_strataErrors_withId) |>
  dplyr::bind_rows(bsbiParserData_1_prepped)


# Check that the number of plant species in nvc_pquads_uniqSpecies_plants is equal to the length of concordance_plants



nrow(nvc_pquads_uniqSpecies_plants) - nrow(concordance_plants)
