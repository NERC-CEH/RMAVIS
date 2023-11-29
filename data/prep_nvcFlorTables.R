nvc_floristic_tables_raw <- read.csv(file = "./data/raw_data/NVC-floristic-tables.csv")

nvc_floristic_tables_tidied <- nvc_floristic_tables_raw |>
  dplyr::filter(is.na(Special.variable.value)) |>
  dplyr::select("NVC.Code" = Community.or.sub.community.code, 
                "Species" = Species.name.or.special.variable,
                "Constancy" = Species.constancy.value) |>
  dplyr::mutate("Species" = stringr::str_trim(Species))


saveRDS(object = nvc_floristic_tables_tidied, file = "./data/bundled_data/nvc_floristic_tables.rds")


nvc_name_to_code <- read.csv(file = "./data/raw_data/NVC-floristic-tables.csv") |>
  dplyr::select("Community.or.sub.community.name", "Community.or.sub.community.code") |>
  dplyr::distinct()

nvc_community_codes <- nvc_name_to_code |>
  dplyr::pull("Community.or.sub.community.code")

saveRDS(object = nvc_community_codes, file = "./data/bundled_data/nvc_community_codes.rds")