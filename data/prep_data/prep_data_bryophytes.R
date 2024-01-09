concordance_all <- readRDS(file = "./data/bundled_data/concordance_all.rds")
concordance_all_trimmed <- concordance_all |>
  dplyr::select("proposedSpecies" = proposedSpecies,
                "BRC_old" = BRC_old
                )

bryoatt_raw <- readxl::read_xls(path = "./data/raw_data/bryophytes/Bryoatt_updated_2017.xls", sheet = "BRYOATT")

bryoatt_codesNames <- bryoatt_raw |>
  dplyr::select("species" = `Taxon name`, 
                "BRC_old" = BRC_num, 
                "BRC_new" = Concept,
                "L" = L,	
                "F" = `F`, 
                "R" = R,	
                "N" = N, 
                "S" = S) |>
  dplyr::mutate("BRC_old" = stringr::str_replace_all(string = BRC_old, pattern = "\\s*", replacement = "")) |>
  dplyr::mutate("BRC_old" = as.numeric(BRC_old))

# Check for duplicate BRC_old codes
length(bryoatt_codesNames$BRC_old) == nrow(bryoatt_codesNames)

# Check for missing BRC_old codes
bryoatt_codesNames_missBRC_old <- bryoatt_codesNames |>
  dplyr::filter(is.na(BRC_old)) |>
  print()

# Check for duplicate BRC_new codes
length(bryoatt_codesNames$BRC_new) == nrow(bryoatt_codesNames)

# Check for missing BRC_new codes
bryoatt_codesNames_missBRC_new <- bryoatt_codesNames |>
  dplyr::filter(is.na(BRC_new)) |>
  print()

bryophytes_data <- bryoatt_codesNames |>
  dplyr::left_join(concordance_all_trimmed, by = "BRC_old") |>
  dplyr::filter(!is.na(proposedSpecies)) |>
  dplyr::select(-species, -BRC_old, -BRC_new) |>
  dplyr::rename("species" = "proposedSpecies")
  
