nvc_pquads_uniqSpecies_bryophytes <- nvc_pquads_noMissCodes |>
  dplyr::select(species, BRC) |>
  dplyr::distinct() |>
  dplyr::filter(stringr::str_detect(string = BRC, pattern = "^[8]")) |>
  dplyr::rename("BRC_old" = BRC)

# Read National Species Inventory and filter for bryophytes
nhm_nsi <- read.csv(file = "C:/Users/User/Desktop/resource.csv")

nhm_nsi$INFORMAL_GROUP |> unique()

nhm_nsi_bryophytes <- nhm_nsi |>
  dplyr::filter(INFORMAL_GROUP %in% c("clubmoss", "hornwort", "liverwort", "moss")) |>
  dplyr::filter(RANK == "Species") |>
  dplyr::select(NBN_TAXON_VERSION_KEY_FOR_RECOMMENDED_NAME, RECOMMENDED_SCIENTIFIC_NAME, TAXON_NAME) |>
  dplyr::rename("species" = "TAXON_NAME") |>
  dplyr::distinct()

# Manually remove incorrect NVK for species in check_nhm_nsi
nhm_nsi_bryophytes <- nhm_nsi_bryophytes |>
  dplyr::filter(
    !(NBN_TAXON_VERSION_KEY_FOR_RECOMMENDED_NAME %in% c(
    "NHMSYS0021195178", # Anomobryum julaceum
    "NBNSYS0000036624", # Bryum bicolor
    "NHMSYS0020695943", # Fissidens bryoides
    "NHMSYS0021195191", # Campylium stellatum
    "NHMSYS0000310860", # Ceratodon purpureus
    "NHMSYS0000310861", # Chiloscyphus polyanthos
    "NHMSYS0020695939", # Ephemerum serratum
    "NHMSYS0000310871", # Fissidens pusillus
    "NHMSYS0000310872", # Fissidens viridulus
    "NHMSYS0021239468", # Oncophorus virens
    "NHMSYS0021239484", # Orthotrichum pumilum
    "NHMSYS0000310891", # Phaeoceros laevis
    "NHMSYS0000310893", # Plagiochila asplenioides
    "NHMSYS0000310903", # Pseudoleskeella catenulata
    "NBNSYS0000189249", # Racomitrium affine
    "NHMSYS0000310906", # Racomitrium canescens
    "NHMSYS0000310909", # Racomitrium heterostichum
    "NBNSYS0000189251", # Racomitrium microcarpon
    "NHMSYS0000309281", # Schistidium apocarpum
    "NHMSYS0000310914", # Schistidium rivulare
    "NHMSYS0020083535", # Sphagnum contortum
    "NHMSYS0000310916", # Sphagnum denticulatum
    "NHMSYS0021232431", # Sphagnum fuscum
    "NHMSYS0000310918", # Sphagnum recurvum
    "NHMSYS0000310921" # Syntrichia ruralis
  )))

# Check for species occurrences
check_nhm_nsi <- nhm_nsi_bryophytes |>
  dplyr::group_by(species) |>
  dplyr::filter(dplyr::n() > 1) |>
  dplyr::ungroup() |>
  dplyr::arrange(species)

bryoatt_raw <- readxl::read_xls(path = "./data/raw_data/Bryoatt_updated_2017.xls", sheet = "BRYOATT")

bryoatt_names <- bryoatt_raw |>
  dplyr::select("bryoattSpecies" = `Taxon name`,
                "BRC_old" = BRC_num,
                "BRC_new" = Concept) |>
  dplyr::mutate("BRC_old" = stringr::str_replace_all(string = BRC_old, pattern = "\\s*", replacement = "")) |>
  dplyr::mutate("BRC_old" = as.numeric(BRC_old))


nvc_pquads_uniqSpecies_bryophytes_joined <- nvc_pquads_uniqSpecies_bryophytes |>
  dplyr::left_join(bryoatt_names, by = "BRC_old")

nvc_pquads_uniqSpecies_bryophytes_joined_present <- nvc_pquads_uniqSpecies_bryophytes_joined |>
  dplyr::filter(!is.na(BRC_new))


# 12 species from nvc_pquads_uniqSpecies_bryophytes could not be matched up to BRYOATT, resolve manually.
nvc_pquads_uniqSpecies_bryophytes_missing <- nvc_pquads_uniqSpecies_bryophytes_joined |>
  dplyr::filter(is.na(BRC_new))


nvc_pquads_uniqSpecies_bryophytes_missing_fixed <- nvc_pquads_uniqSpecies_bryophytes_missing |>
  dplyr::mutate(
    "BRC_new" =
      dplyr::case_when(
        species == "Hypnum cupressiforme sens.lat." ~ "Bry_351",
        species == "Weissia sp." ~ "Bry_1260",
        species == "Fissidens sp." ~ "Bry_1147",
        species == "Calypogeia sp." ~ "Bry_1275",
        species == "Cephaloziella sp." ~ "Bry_1277",
        species == "Racomitrium heterostichum sens.lat." ~ "Bry_524",
        species == "Bryum erythrocarpum" ~ "Bry_1106", # Not that Bry_1106 refers to the taxon concept "Bryum"
        species == "Pottia starkeana subsp.conica" ~ "Bry_1781",
        species == "Lophocolea bidentata var.bidentata" ~ "Bry_1056",
        species == "Riccia sp." ~ "Bry_1335",
        species == "Barbula sp." ~ "Bry_1098",
        species == "Pohlia proligera sensu Smith (1978)" ~ "Bry_1354",
        TRUE ~ NA
      )
  ) |>
  dplyr::mutate(
    "proposedSpecies" =
      dplyr::case_when(
        species == "Hypnum cupressiforme sens.lat." ~ "Hypnum cupressiforme",
        species == "Weissia sp." ~ "Weissia",
        species == "Fissidens sp." ~ "Fissidens",
        species == "Calypogeia sp." ~ "Calypogeia",
        species == "Cephaloziella sp." ~ "Cephaloziella",
        species == "Racomitrium heterostichum sens.lat." ~ "Racomitrium heterostichum",
        species == "Bryum erythrocarpum" ~ "Bryum",
        species == "Pottia starkeana subsp.conica" ~ "Microbryum davallianum",
        species == "Lophocolea bidentata var.bidentata" ~ "Lophocolea bidentata",
        species == "Riccia sp." ~ "Riccia",
        species == "Barbula sp." ~ "Barbula",
        species == "Pohlia proligera sensu Smith (1978)" ~ "Pohlia proligera",
        TRUE ~ NA
      )
  ) |>
  dplyr::select(-bryoattSpecies)


# Check for duplicates
# nvc_pquads_uniqSpecies_bryophytes_missing_fixed |>
#   dplyr::group_by(proposedSpecies, species) |>
#   dplyr::filter(dplyr::n() > 1) |>
#   dplyr::ungroup() |>
#   dplyr::arrange(proposedSpecies)
# 
# nvc_pquads_uniqSpecies_bryophytes_joined_present |>
#   dplyr::group_by(species, bryoattSpecies) |>
#   dplyr::filter(dplyr::n() > 1) |>
#   dplyr::ungroup() |>
#   dplyr::arrange(bryoattSpecies)
# 
# foo <- nvc_pquads_uniqSpecies_bryophytes_joined_present |>
#   dplyr::bind_rows(nvc_pquads_uniqSpecies_bryophytes_missing_fixed) |>
#   dplyr::left_join(nhm_nsi_bryophytes, by = "species") |>
#   # dplyr::distinct() |>
#   dplyr::group_by(species, bryoattSpecies) |>
#   dplyr::filter(dplyr::n() > 1) |>
#   dplyr::ungroup() |>
#   dplyr::arrange(bryoattSpecies)


nvc_pquads_uniqSpecies_bryophytes_fixedBRC <- nvc_pquads_uniqSpecies_bryophytes_joined_present |>
  dplyr::bind_rows(nvc_pquads_uniqSpecies_bryophytes_missing_fixed) |>
  # Add TVK from natural history museum species inventory
  dplyr::left_join(nhm_nsi_bryophytes, by = "species") |>
  dplyr::distinct() |>
  dplyr::mutate(
    "proposedSpecies" =
      dplyr::case_when(
        is.na(proposedSpecies) ~ bryoattSpecies,
        is.na(bryoattSpecies) ~ proposedSpecies,
        TRUE ~ as.character(proposedSpecies)
      )
  ) |>
  dplyr::select(-bryoattSpecies, -RECOMMENDED_SCIENTIFIC_NAME) |>
  dplyr::rename("assignNVCSpecies" = species,
                "TVK" = NBN_TAXON_VERSION_KEY_FOR_RECOMMENDED_NAME) |>
  dplyr::mutate(
    "TVK" = 
      dplyr::case_when(
        assignNVCSpecies == "Sphagnum auriculatum var.auriculatum" ~ "NBNSYS0000157062",
        assignNVCSpecies == "Ditrichum flexicaule sens.lat." ~ "NHMSYS0000310869",
        assignNVCSpecies == "Lophocolea bidentata sens.lat." ~ "NHMSYS0000309660",
        assignNVCSpecies == "Racomitrium canescens sens.lat." ~ "NHMSYS0000310906",
        assignNVCSpecies == "Campylium stellatum var.protensum" ~ "NBNSYS0000143585",
        assignNVCSpecies == "Gymnostomum recurvirostrum sens.lat." ~ "NHMSYS0000309278",
        assignNVCSpecies == "Sphagnum auriculatum var.inundatum" ~ "NHMSYS0000310674",
        assignNVCSpecies == "Chiloscyphus polyanthos var.pallescens" ~ "NHMSYS0000309622",
        assignNVCSpecies == "Pohlia wahlenbergii var.glacialis" ~ "NHMSYS0000310383",
        assignNVCSpecies == "Bryum bicolor sens.lat." ~ "NHMSYS0000310854",
        assignNVCSpecies == "Tortula ruralis subsp.ruralis" ~ "NHMSYS0000310723",
        assignNVCSpecies == "Tortula ruralis subsp.ruraliformis" ~ "NHMSYS0021194758",
        assignNVCSpecies == "Hypnum cupressiforme sens.lat." ~ "NHMSYS0000310884",
        assignNVCSpecies == "Weissia sp." ~ "NHMSYS0000310828",
        assignNVCSpecies == "Fissidens sp." ~ "NHMSYS0000309865",
        assignNVCSpecies == "Calypogeia sp." ~ "NHMSYS0000309532",
        assignNVCSpecies == "Cephaloziella sp." ~ "NHMSYS0000309598",
        assignNVCSpecies == "Racomitrium heterostichum sens.lat." ~ "NHMSYS0000310908",
        assignNVCSpecies == "Bryum erythrocarpum" ~ "NHMSYS0000309470",
        assignNVCSpecies == "Pottia starkeana subsp.conica" ~ "NBNSYS0100004045",
        assignNVCSpecies == "Lophocolea bidentata var.bidentata" ~ "NHMSYS0000309660",
        assignNVCSpecies == "Riccia sp." ~ "NHMSYS0000310446",
        assignNVCSpecies == "Barbula sp." ~ "NHMSYS0000309401",
        assignNVCSpecies == "Pohlia proligera sensu Smith (1978)" ~ "NBNSYS0000036560",
        assignNVCSpecies == "Plagiochila spinulosa agg." ~ "NHMSYS0000310896",
        assignNVCSpecies == "Sphagnum auriculatum" ~ "NHMSYS0000310916",
        assignNVCSpecies == "Hypnum cupressiforme var. lacunosum" ~ "NBNSYS0000036934",
        TRUE ~ as.character(TVK)
      )
  ) |>
  dplyr::distinct()

# Check for duplicates
nvc_pquads_uniqSpecies_bryophytes_fixedBRC |>
  dplyr::group_by(proposedSpecies, assignNVCSpecies) |>
  dplyr::filter(dplyr::n() > 1) |>
  dplyr::ungroup() |>
  dplyr::arrange(proposedSpecies) |>
  print()


# Check for species with no TVK
nvc_pquads_uniqSpecies_bryophytes_noTVK <- nvc_pquads_uniqSpecies_bryophytes_fixedBRC |>
  dplyr::filter(is.na(TVK))

concordance_bryophytes <- nvc_pquads_uniqSpecies_bryophytes_fixedBRC |>
  dplyr::filter(BRC_old %in% nvc_pquads_uniqSpecies_bryophytes$BRC_old)
  
# Check that the number of plant species in nvc_pquads_uniqSpecies_bryophytes is equal to the length of concordance_bryophytes
nrow(nvc_pquads_uniqSpecies_bryophytes) - nrow(concordance_bryophytes)

# Check missing species
setdiff(nvc_pquads_uniqSpecies_bryophytes$species, concordance_bryophytes$assignNVCSpecies) 
setdiff(concordance_bryophytes$assignNVCSpecies, nvc_pquads_uniqSpecies_bryophytes$species) 

# Find missing rows
concordance_bryophytes |> dplyr::filter(is.na(assignNVCSpecies))
concordance_bryophytes |> dplyr::filter(is.na(BRC_old))
concordance_bryophytes |> dplyr::filter(is.na(BRC_new))
concordance_bryophytes |> dplyr::filter(is.na(proposedSpecies))
concordance_bryophytes |> dplyr::filter(is.na(TVK))

# Check whether there are any duplicate assignNVCSpecies and BRC_old combinations
concordance_bryophytes_nonUniqpropSpecies <- concordance_bryophytes |>
  dplyr::group_by(proposedSpecies, assignNVCSpecies) |>
  dplyr::filter(dplyr::n() > 1) |>
  dplyr::ungroup() |>
  dplyr::arrange(proposedSpecies) |>
  print()

# Check whether there is any missing data
concordance_bryophytes_naRows <- concordance_bryophytes |>
  dplyr::filter(is.na(dplyr::if_any(dplyr::everything(), is.na)))

