
master_data <- plants_data |>
  dplyr::bind_rows(bryophytes_data)

length(unique(master_data$species)) == nrow(master_data)

saveRDS(object = master_data, file = "./data/bundled_data/master_data.rds")
