freqMethod_options <- list(
  "Direct Percentage" = "directPercentage",
  "Frequency Class" = "frequencyClass"
)

species_list <- list(
  "Acer campestre"
)


suppressWarnings(
  
  suppressMessages(
   
    raw_JNCC_habCor <- readxl::read_xls(path = "data/Habitat-correspondences-2008.xls",
                                        sheet = "master table - sheet protected")
    
  )
  
)

# nvc_floristic_tables <- read.csv()

tidied_JNCC_habCor <- raw_JNCC_habCor |>
  dplyr::filter(CLASSN1 == "National Vegetation Classification") |>
  dplyr::select(-BIOTOPE_SHORT_TERM1,
                -BIOTOPE_CODE1,
                -CLASSN1,
                -`...10`) |>
  dplyr::relocate(RELATIONSHIP, .after = BIOTOPE_FULL_TERM1) |>
  dplyr::distinct()
  
tidied_JNCC_habCor$BIOTOPE_FULL_TERM1 <- gsub("<[^>]+>", "", tidied_JNCC_habCor$BIOTOPE_FULL_TERM1)
