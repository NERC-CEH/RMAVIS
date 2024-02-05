# Read Concordance --------------------------------------------------------
concordance_bryophytes <- readRDS(file = "./data/bundled_data/concordance_bryophytes.rds") |>
  dplyr::select(assignNVCSpecies, TVK, proposedSpecies) |>
  tibble::as_tibble()


# Read and Clean BRYOATT --------------------------------------------------
bryoatt_raw <- readxl::read_xls(path = "./data/raw_data/bryophytes/Bryoatt_updated_2017.xls", sheet = "BRYOATT") |>
  tibble::as_tibble()

bryoatt_codesNames <- bryoatt_raw |>
  dplyr::select("bryoattSpecies" = `Taxon name`, 
                "BRC_old" = BRC_num, 
                "BRC_new" = Concept,
                "L" = L,	
                "F" = `F`, 
                "R" = R,	
                "N" = N, 
                "S" = S) |>
  dplyr::mutate("BRC_old" = stringr::str_replace_all(string = BRC_old, pattern = "\\s*", replacement = "")) |>
  dplyr::mutate("BRC_old" = as.numeric(BRC_old))


# Check which BRYOATT species aren't in concordance -----------------------
bryoatt_names <- bryoatt_codesNames$bryoattSpecies |> unique()
concordance_names <- concordance_bryophytes$proposedSpecies |> unique()
bryoatt_names_notin_concordance_names <- bryoatt_names[!(bryoatt_names %in% concordance_names)]
bryoatt_names_in_concordance_names <- bryoatt_names[bryoatt_names %in% concordance_names]

concordance_names_notin_bryoatt_names <- bryoatt_names[!(concordance_names %in% bryoatt_names)]


# Create Altered BRYOATT With Added NBN Names -----------------------------
bryoatt_nbn_names <- concordance_bryophytes |>
  dplyr::select(proposedSpecies, TVK) |>
  dplyr::mutate("bryoattSpecies" = proposedSpecies) |>
  dplyr::right_join(bryoatt_codesNames, by = "bryoattSpecies") |>
  # Strip " s.l." and " s.str." for now 
  dplyr::mutate("bryoattSpecies" = stringr::str_replace(string = bryoattSpecies, pattern = " s.l.", replacement = "")) |>
  dplyr::mutate("bryoattSpecies" = stringr::str_replace(string = bryoattSpecies, pattern = " s.str.", replacement = ""))

# Submit BRYOATT names which don't match to NBN API -----------------------
# https://api.nbnatlas.org/
query_url <- "https://species-ws.nbnatlas.org/species/lookup/bulk?"

unmatched_names <- bryoatt_nbn_names |>
  dplyr::filter(is.na(proposedSpecies)) |>
  dplyr::pull(bryoattSpecies)

req_list <- list("names" = unmatched_names)

req_body <- jsonlite::toJSON(req_list, auto_unbox = TRUE)

response <- httr::POST(url = query_url, body = req_body)

responseContent <- httr::content(response)

names(responseContent) <- unmatched_names

response_df <- data.table::rbindlist(responseContent, idcol = "bryoattSpecies", fill = FALSE)

response_df_trimmed <- response_df |>
  dplyr::select("bryoattSpecies" = bryoattSpecies, "TVK" = identifier, "proposedSpecies" = name) # "proposedSpecies" = name

# Add resolved names ------------------------------------------------------
bryoatt_nbn_names2 <- bryoatt_nbn_names |>
  dplyr::filter(is.na(TVK)) |>
  dplyr::select(-proposedSpecies, -TVK) |>
  dplyr::left_join(response_df_trimmed, by = "bryoattSpecies") |>
  dplyr::filter(!is.na(bryoattSpecies))

# Compile completed data --------------------------------------------------
bryophytes_data <- bryoatt_nbn_names |>
  dplyr::filter(!is.na(TVK)) |>
  rbind(bryoatt_nbn_names2) |>
  dplyr::select(-bryoattSpecies, -TVK, -BRC_old, -BRC_new) |>
  dplyr::rename("species" = "proposedSpecies") |>
  dplyr::distinct()

# Save Bryophytes data ----------------------------------------------------
saveRDS(object = bryophytes_data, file = "./data/bundled_data/bryophytes_data.rds")  
