source("R/create_constants.R")


# Rothamstead test datasets, grouped quadrats with frequency data ---------
# example_data_raw <- read.csv(file = "./data/raw_data/Rothobs_ac4_test_data.csv")
# 
# dominCoverMidPointPerc_df <- tibble::enframe(dominCoverMidPointPerc) |>
#   dplyr::rename("Domin" = name,
#                 "Cover" = value)
# 
# example_data_prepped <- example_data_raw |>
#   dplyr::select("Species" = BRC_names, # assignNVCSpecies
#                 "BRC_old" = spp,
#                 "Domin" = freq2,
#                 "plot_id" = plot_id,
#                 "pid" = pid) |>
#   # dplyr::left_join(dominCoverMidPointPerc_df, by = "Domin") |>
#   # dplyr::mutate("Cover" = Cover.Perc/100, .keep = "unused") |>
#   dplyr::mutate("Cover" = NA) |>
#   dplyr::mutate("Year" = stringr::str_extract(string = plot_id, pattern = "(\\d{4})"), .keep = "unused") |>
#   # dplyr::mutate(Quadrat = stringr::str_extract(string = plot_id, pattern = "[^_]+$")) |>
#   dplyr::mutate("Quadrat" = pid, .keep = "unused") |>
#   dplyr::mutate("Site" = "Rothamstead") |>
#   dplyr::select(Year, Site, Quadrat, Species, Cover)
# 
# example_data_df <- example_data_prepped
# 
# exampleDataOptions <- c("Rothamstead" = "Rothamstead")
# 
# saveRDS(object = exampleDataOptions, file = "./data/bundled_data/exampleDataOptions.rds")
# 
# saveRDS(object = example_data_df, file = "./data/bundled_data/example_data_df.rds")




# AssignNVC test datasets -------------------------------------------------
# dominCoverMidPointPerc_df <- tibble::enframe(dominCoverMidPointPerc) |>
#   dplyr::rename("domin" = name,
#                 "Cover" = value)
# 
# example_data_df_all <- readRDS(file = "./data/raw_data/assignNVC_test_dataset.rds") |>
#   dplyr::arrange(ID) |>
#   dplyr::select("Quadrat" = ID, "Species" = species, "Site" = site, domin) |>
#   dplyr::mutate("domin" = as.character(domin)) |>
#   dplyr::left_join(dominCoverMidPointPerc_df, by = "domin") |>
#   dplyr::select(-domin) |>
#   dplyr::filter(Site != "")
# 
# exampleDataOptions <- example_data_df_all$Site |> unique()
# 
# setNames(exampleDataOptions, exampleDataOptions)
# 
# exampleDataOptions <- c("None" = "none", exampleDataOptions)
# 
# example_data_df_quadrats_per_site <- example_data_df_all |>
#   dplyr::group_by(Site) |>
#   dplyr::summarise(quadrats_per_site = dplyr::n())
# 
# example_data_df_trimmed <- example_data_df_all |>
#   # Create a site quadrat number
#   dplyr::group_by(Site) |>
#   dplyr::mutate("Site.Quadrat" = as.integer(factor(Quadrat))) |>
#   dplyr::ungroup() |>
#   # Select only 5 quadrats site as example data
#   dplyr::filter(Site.Quadrat <= 5) |>
#   dplyr::select(-Site.Quadrat) |>
#   # Add Group
#   dplyr::mutate("Group" = "A")
# 
# saveRDS(object = exampleDataOptions, file = "./data/bundled_data/exampleDataOptions.rds")
# 
# saveRDS(object = example_data_df_trimmed, file = "./data/bundled_data/example_data_df.rds")

exampleData_pd_prepped <- exampleData_pd |>
  dplyr::mutate("Site" = "Parsonage Down")

example_data_all <- rbind(exampleData_pd_prepped)

saveRDS(object = example_data_all, file = "./data/bundled_data/example_data_all.rds")

