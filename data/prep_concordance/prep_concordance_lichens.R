nvc_pquads_uniqSpecies_lichens <- nvc_pquads_noMissCodes |>
  dplyr::select(species, BRC) |>
  dplyr::distinct() |>
  dplyr::filter(stringr::str_detect(string = BRC, pattern = "^[5]"))

# Read BLS Lichen species checklist
# Retrieved from https://lists.nbnatlas.org/speciesListItem/list/dr1806
blsChecklist <- read.csv(file = "./data/raw_data/concordance_data/BLS_lichen_species_list.csv")

blsChecklist_prepped <- blsChecklist |>
  dplyr::select("TVK" = guid,
                "species" = Supplied.Name,
                "blsSpecies" = scientificName)

# Read the BLS lichen taxon database
# Retrieved from https://britishlichensociety.org.uk/resources/lichen-taxon-database
# blsTaxonDict <- read.csv(file = "./data/raw_data/bls-taxon-dictionary-2023-11-29.csv")
# blsTaxonDict_prepped <- blsTaxonDict |>
#   dplyr::mutate("species" = Name)

# Read the BRC dictionary
brc_dict_prepped <- read.csv(file = "./data/raw_data/concordance_data/brc_spp_names_vasc_bryo.csv") |>
  dplyr::rename("species" = "NAME")

# Join BLS checklist to nvc_pquads_uniqSpecies_lichens

nvc_pquads_uniqSpecies_lichens_blsJoined <- nvc_pquads_uniqSpecies_lichens |>
  dplyr::distinct() |>
  dplyr::left_join(blsChecklist_prepped, by = "species") |>
  dplyr::distinct() |>
  dplyr::left_join(brc_dict_prepped, by = "species") |>
  dplyr::select(species, BRC, TVK, CONCEPT) |>
  dplyr::mutate(
    "TVK" = 
      dplyr::case_when(
        species == "Cetraria islandica" ~ "NBNSYS0000018334",
        species == "Cladonia uncialis" ~ "NHMSYS0001477683",
        species == "Cladonia arbuscula" ~ "NHMSYS0000361869",
        species == "Peltigera aphthosa" ~ "NBNSYS0000019330",
        species == "Collema tenax" ~ "NBNSYS0000018453",
        species == "Cladonia papillaria" ~ "NHMSYS0001495833", # Pycnothelia papillaria
        species == "Buellia epigaea" ~ "NHMSYS0001476310",
        species == "Cladonia impexa" ~ "NBNSYS0000018396",
        species == "Cladonia tenuis" ~ "NHMSYS0001477611",
        species == "Cornicularia aculeata" ~ "BMSSYS0000003564",
        species == "Cladonia squamosa" ~ "NBNSYS0000018405",
        species == "Cladonia furcata" ~ "NBNSYS0000018374",
        species == "Cladonia coccifera" ~ "BMSSYS0000533659",
        species == "Cornicularia muricata" ~ "BMSSYS0000003585",
        species == "Cladonia crispata" ~ "NBNSYS0000018363",
        species == "Cladonia chlorophaea" ~ "BMSSYS0000533657",
        species == "Cetraria nivalis" ~ "BMSSYS0000006834",
        species == "Thamnolia vermicularis" ~ "NHMSYS0001499500",
        species == "Ochrolechia frigida" ~ "NBNSYS0000019366",
        species == "Cetraria glauca" ~ "NHMSYS0001493938",
        species == "Alectoria sarmentosa" ~ "NBNSYS0000018169",
        species == "Cladonia leucophaea" ~ "NHMSYS0001477611",
        species == "Cetraria norvegica/platysma lacunosum" ~ "NBNSYS0000018951", # Platismatia norvegica
        species == "Cladonia subrangiformis" ~ "NHMSYS0001477639",
        species == "Cladonia verticillata" ~ "BMSSYS0000003992",
        species == "Collema sp." ~ "BMSSYS0000043973",
        species == "Parmelia saxatilis" ~ "NHMSYS0001491791",
        species == "Dermatocarpon lachnaeum" ~ "NHMSYS0001493873", # Placidium squamulosum
        species == "Toninia caeruleonigricans" ~ "NBNSYS0000158101", # Toninia sedifolia
        species == "Squamarina crassa" ~ "NHMSYS0001498818",
        species == "Cladonia cervicornis" ~ "NBNSYS0000018354",
        species == "Cetraria hiascens/delisei" ~ "BMSSYS0000003592", # Cetrariella delisei
        species == "Stereocaulon vesuvianum" ~ "NBNSYS0000019067",
        species == "Cladonia squamules/sp" ~ "NHMSYS0001477591", # Cladonia
        species == "Cladonia polydactyla" ~ "NHMSYS0001477660",
        TRUE ~ as.character(TVK)
      )
  ) |>
  dplyr::mutate(
    "proposedSpecies" = 
      dplyr::case_when(
        species == "Cladonia papillaria" ~ "Pycnothelia papillaria",
        species == "Cetraria norvegica/platysma lacunosum" ~ "Platismatia norvegica",
        species == "Dermatocarpon lachnaeum" ~ "Placidium squamulosum",
        species == "Toninia caeruleonigricans" ~ "Toninia sedifolia",
        species == "Cetraria hiascens/delisei" ~ "Cetrariella delisei",
        species == "Cladonia squamules/sp" ~ "Cladonia",
        species == "Collema sp." ~ "Collema",
        TRUE ~ as.character(species)
      )
  ) |>
  dplyr::select("assignNVCSpecies" = species,
                "BRC_old" = BRC,
                "TVK" = TVK,
                "BRC_new" = CONCEPT,
                "proposedSpecies" = proposedSpecies)




# Compile concordance
concordance_lichens <- nvc_pquads_uniqSpecies_lichens_blsJoined


# Check that the number of plant species in nvc_pquads_uniqSpecies_lichens is equal to the length of concordance_lichens
nrow(nvc_pquads_uniqSpecies_lichens) - nrow(concordance_lichens)

# Check whether there is any missing data
concordance_lichens_naRows <- concordance_lichens |>
  dplyr::filter(is.na(dplyr::if_any(dplyr::everything(), is.na)))


# Save concordance
saveRDS(object = concordance_lichens, file = "./data/bundled_data/concordance_lichens.rds")
