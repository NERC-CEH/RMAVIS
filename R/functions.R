# Base R or tidyverse?

# Convert surveyTable to surveyTableWide

# Calculate DCA using surveyTableWide and selected nvc_pquad_dca_list DCA results

# Produce a DCA plot ggplot2 graph with points only

# Produce a DCA plot ggplot2 graph with points and convex hulls

# Produce a DCA plot ggplot2 graph with sample quadrat points and convex hulls

# Calculate diversity metrics using surveyTable 

# Calculate mean cover-weighted EIVs using surveyTable
 
# Calculate mean EIVs using surveyTable

# Compose floristic tables from surveyTable
composeSyntopicTables <- function(surveyTable){
  
  syntopicTables <- surveyTable |>
    # tidyr::unite(col = "ID", c("Year", "Group"), sep = " - ", remove = TRUE) |>
    dplyr::mutate("ID" = as.character(Year)) |>
    dplyr::select(-Year, -Group) |>
    dplyr::select(-Cover) |>
    dplyr::mutate("Present" = 1) |>
    tidyr::pivot_wider(id_cols = c(ID, Species),
                       values_from = Present,
                       names_from = Quadrat) |>
    dplyr::rowwise() |>
    dplyr::mutate("Sum" = sum(dplyr::c_across(dplyr::where(is.numeric)), na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate("Frequency" = Sum / (ncol(dplyr::pick(dplyr::everything())) - 3)) |> # -2
    dplyr::select(ID, Species, Sum, Frequency) |>
    dplyr::mutate(
      "Constancy" =
        dplyr::case_when(
          Frequency <= 0.2 ~ "I",
          Frequency <= 0.4 ~ "II",
          Frequency <= 0.6 ~ "III",
          Frequency <= 0.8 ~ "IV",
          Frequency <= 1.0 ~ "V",
          TRUE ~ as.character(Frequency)
        )
    ) |>
    dplyr::select(ID, Species, Constancy) |>
    dplyr::mutate("Constancy" = factor(Constancy, levels = c("V", "IV", "III", "II", "I"))) |>
    dplyr::arrange(ID, Constancy, Species)
  
  return(syntopicTables)
  
}

                                