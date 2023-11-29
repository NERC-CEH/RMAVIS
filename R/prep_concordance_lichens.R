nvc_pquads_uniqSpecies_lichens <- nvc_pquads_noMissCodes |>
  dplyr::select(species, BRC) |>
  dplyr::distinct() |>
  dplyr::filter(stringr::str_detect(string = BRC, pattern = "^[5]")) 

# Read BLS Lichen species checklist
# Retrieved from https://lists.nbnatlas.org/speciesListItem/list/dr1806
blsChecklist <- read.csv(file = "./data/raw_data/BLS_lichen_species_list.csv")

blsChecklist_prepped <- blsChecklist |>
  dplyr::select("TVK" = guid,
                "species" = Supplied.Name,
                "blsSpecies" = scientificName)

# Read the BLS lichen taxon database
# Retrieved from https://britishlichensociety.org.uk/resources/lichen-taxon-database

blsTaxonDict <- read.csv(file = "./data/raw_data/bls-taxon-dictionary-2023-11-29.csv")







# Join BLS checklist to nvc_pquads_uniqSpecies_lichens

nvc_pquads_uniqSpecies_lichens_blsJoined <- nvc_pquads_uniqSpecies_lichens |>
  dplyr::left_join(blsChecklist_prepped, by = "species")

nvc_pquads_uniqSpecies_lichens_blsJoined_present <- nvc_pquads_uniqSpecies_lichens_blsJoined |>
  dplyr::filter(!is.na(TVK))

nvc_pquads_uniqSpecies_lichens_blsJoined_absent <- nvc_pquads_uniqSpecies_lichens_blsJoined |>
  dplyr::filter(is.na(TVK))



# Compile concordance
# concordance_lichens


# Check that the number of plant species in nvc_pquads_uniqSpecies_lichens is equal to the length of concordance_lichens
nrow(nvc_pquads_uniqSpecies_lichens) - nrow(concordance_lichens)

# Check whether there is any missing data
concordance_lichens_naRows <- concordance_lichens |>
  dplyr::filter(is.na(dplyr::if_any(dplyr::everything(), is.na)))