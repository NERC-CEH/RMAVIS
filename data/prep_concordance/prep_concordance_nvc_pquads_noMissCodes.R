nvc_pquads_noMissCodes <- assignNVC::nvc_pquads |>
  dplyr::select(species, BRC) |>
  dplyr::distinct() |>
  dplyr::mutate(
    "BRC" = 
      dplyr::case_when(
        species == "Stellaria media agg." ~ 09202012,
        species == "Cochlearia officinalis" ~ 0920535,
        species == "Matricaria maritima" ~ 09201241.3,
        species == "Atriplex prostrata" ~ 0920214,
        species == "Polygonum aviculare" ~ 09201523,
        species == "Spergula arvensis" ~ 09201987,
        species == "Amaranthus retroflexus" ~ 092092,
        species == "Aster tripolium (rayed)" ~ 0920204,
        species == "Algal mat" ~ 22,
        species == "Triglochin maritima" ~ 09202101,
        species == "Juncus gerardi" ~ 09201069,
        species == "Amblystegium riparium" ~ 0820366,
        species == "Aster tripolium (unrayed)" ~ 0920204,
        species == "Zostera marina" ~ 09202239,
        species == "Zostera angustifolia" ~ 09202238,
        species == "Zostera noltii" ~ 09202240,
        species == "[Alga]" ~ 22,
        species == "Bromus racemosus" ~ 0920271,
        species == "Bromus commutatus" ~ 0920262,
        species == "Oenanthe silaifolia" ~ 09201368,
        species == "Polygala oxyptera" ~ 09201515,
        species == "Festuca pratensis x Lolium perenne (x Festulolium loli" ~ 0920815,
        TRUE ~ as.numeric(BRC)
      )
  )


# Check there are no missing BRC codes.
nvc_pquads_noMissCodes |>
  dplyr::filter(is.na(BRC))

!all(is.na(nvc_pquads_noMissCodes$BRC))
