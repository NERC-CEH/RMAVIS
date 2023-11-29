bryoatt_raw <- readxl::read_xls(path = "./data/raw_data/Bryoatt_updated_2017.xls", sheet = "BRYOATT")

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
bryoatt_codesNames_missBRC_old <-bryoatt_codesNames |>
  dplyr::filter(is.na(BRC_old))

# Check for duplicate BRC_new codes
length(bryoatt_codesNames$BRC_new) == nrow(bryoatt_codesNames)

# Check for missing BRC_new codes
bryoatt_codesNames_missBRC_new <- bryoatt_codesNames |>
  dplyr::filter(is.na(BRC_new))

# Fix missing BRC_old and BRC_new codes
bryoatt_codesNames_fixed <- bryoatt_codesNames |>
  dplyr::filter(!is.na(BRC_old)) |> # The only observations with missing BRC_codes have no species or indicator values, remove.
  # dplyr::mutate(
  #   "BRC_new" =
  #     dplyr::case_when(
  #       species == "Bryum archangelicum" ~ ,
  #       species == "Bryum lawersianum" ~ ,
  #       species == "Bryum mamillatum" ~ ,
  #       species == "Bryum neodamense" ~ ,
  #       species == "Bryum stirtonii" ~ ,
  #       species == "Ephemerum stellatum" ~ ,
  #       species == "Fontinalis squamosa var. dixonii" ~ ,
  #       species == "Fossombronia caespitiformis" ~ ,
  #       species == "Plagiothecium ruthei" ~ ,
  #       species == "Schistidium elegantulum subsp. elegantulum" ~ ,
  #       species == "Schistidium elegantulum subsp. wilsonii" ~ ,
  #       species == "Tortula subulata var. angustata" ~ ,
  #       species == "Tortula subulata var. graeffii" ~ ,
  #       species == "Tortula subulata var. subulata" ~ ,
  #       TRUE ~ as.numeric(BRC_new)
  #     )
  # ) |>
  dplyr::select("preferredTaxon" = species, BRC_old, BRC_new, L, `F`, R, N, S)

bryophytes_data <- bryoatt_codesNames_fixed
