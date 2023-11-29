speciesNames <- assignNVC::nvc_pquads |>
  dplyr::pull(species)

saveRDS(object = speciesNames, file = "./data/bundled_data/speciesNames.rds")
