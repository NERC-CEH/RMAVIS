
master_data <- plants_data |>
  dplyr::bind_rows(bryophytes_data) #|>
  # dplyr::bind_rows(algae_data) |>
  # dplyr::bind_rows(lichens_data) |>
  # dplyr::bind_rows(charophytes_data)

length(master_data$taxonId) == nrow(master_data)
length(master_data$preferredTaxon) == nrow(master_data)

saveRDS(object = master_data, file = "./data/bundled_data/master_data.rds")