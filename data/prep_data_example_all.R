exampleData_pd <- readRDS(file = "./data/bundled_data/exampleData_pd.rds")
exampleData_whitwell <- readRDS(file = "./data/bundled_data/exampleData_whitwell.rds")
exampleData_stormWoods <- readRDS(file = "./data/bundled_data/exampleData_stormWoods.rds")
exampleData_sandDunes <- readRDS(file = "./data/bundled_data/exampleData_sandDunes.rds")

example_data_all <- rbind(exampleData_pd, 
                          exampleData_whitwell, 
                          exampleData_stormWoods, 
                          exampleData_sandDunes)

saveRDS(object = example_data_all, file = "./data/bundled_data/example_data_all.rds")

