# RMAVIS 1.1.0 ()
*   Modifications:
      -   Changing the minimum number of plots per group for Czekanowski similarity
          calculations to 2.
      -   Tidying the names and order of the RMAVIS results .xlsx download.
      -   Changing "National Reference Spaces" option in the MVA module to
          "Reference Spaces" and altering the module so that the local reference
          ordinations are also linked to this option.
      -   Updating the taxonomic backbone to the XXXXXX UKSI release ().
      -   Updating the habitat correspondences to include a more comprehensive
          set of NVC-to-EUNIS correspondences, floodplain meadows correspondences,
          gap-filled NPMS habitat correspondences, and .......
*   Fixes: NA
*   New Features:
      -   Adding the ability to trim leading and trailing white space from data
          in the survey data validation module.
      -   Adding the ability to lump taxa into higher ranks,
          where the parent taxa of a higher rank exist in the NVC floristic tables,
          but the lower rank taxa do not.
          This ensures that related taxon concepts are matched, where desired.
          For example, using this functionality one could lump 
          Festuca rubra s.s. into Festuca rubra agg.
      -   Adding additional NVC community attributes to the NVC lookup table,
          floristic tables, and ... modules.
      -   Adding an option to view the min, mean, and max cover values to
          composed floristic tables.
      -   Adding a new application section "NicheModels", where ecological
          niche models can be viewed and used to estimate the probability of
          occurrence.

# RMAVIS 1.0.1 (2024-08-08)
*   Modifications: NA
*   Fixes:
      -   Removing superfluous file - "./app.R".
      -   Ensuring all internal constants are prefixed in "surveyData_server.R".
*   New Features:
      -   Adding deployment helper file "./deploy.R".
      -   Adding reference to published JOSS paper and updating "CITATION.cff" file.

# RMAVIS 1.0 (2024-07-02)
*   Modifications:
      -   Updating README to include instructions on installing a LaTeX distribution using `{tinytex}`.
      -   Removing Dockerfile and docker-compose.yml as they are currently superfluous.
      -   Bundling app files into ./inst/app
*   Fixes:
      -   Streamlining dependencies, see the imports listed in DESCRIPTION and packages loaded in
          ./inst/app/app.R
*   New Features:
      -   Adding News section.
      -   Adding function to run app - `RMAVIS::runApp()`.
      -   Adding contribution guidelines in CONTRIBUTING.md.

# RMAVIS 0.9998 (2024-05-22)
*   Modifications:
      -   Improving the README and including it in a home page of the app.
      -   Adding ability to close modal popups by clicking outside the modal.
      -   Transferring repository to NERC-CEH organisation.
      -   Setting documentation iframe to 85vh.
      -   Removing BRC_old and BRC_new from `RMAVIS::concordance` as it is currently incomplete.
      -   Changing names in `RMAVIS::concordance`: "assignNVCName" -> "originalTaxonName"
          and "proposedSpecies" -> "rmavisTaxonName".
      -   Updating JOSS paper to reflect the addition of the Alage taxa present
          in SM1.
      -   Moving Options sidebar to application nav_panel only.
*   Fixes:
      -   Fixing issue where changing the cover scale does not trigger re-validation.
*   New Features: NA

# RMAVIS 0.9997 (2024-05-06)
*   Modifications:
      -   Updating minimum R version to R 4.4 to address security concerns. Note
          that R 4.4.0 does not actually fix this, see here: https://aitap.github.io/2024/05/02/unserialize.html
*   Fixes: NA
*   New Features: NA

# RMAVIS 0.9996 (2024-04-30)
*   Modifications:
      -   Adding package dependency references to JOSS paper.
      -   Removing {janitor} dependency.
      -   Changing MVA option "Survey Quadrat Change" to "Trajectory".
      -   Ensuring accepted species are sorted alphabetically.
      -   Moving to NBN Tracheophyta and Pteridophyta child taxa for vascular plants 
          to align with bryophytes and provide a single taxonomic backbone.
*   Fixes: 
      -   Ensuring author information in DESCRIPTION file are all separated by commas.
*   New Features: NA

# RMAVIS 0.9995 (2024-04-26)
*   Modifications:
      -   Changing licence to the OSI-approved LGPL-3.0 licence to ensure compliance with JOSS.
*   Fixes: NA
*   New Features: NA

# RMAVIS 0.9994 (2024-04-21)
*   Modifications: NA
*   Fixes: 
      -   Fixing missing bryophytes following NBN concordance move.
*   New Features: NA

# RMAVIS 0.9993 (2024-04-21)
*   Modifications:
      -   Changing licence to CC BY 4.0 ready for v1.
*   Fixes:
      -   Fixing issue where group-re-allocation table fails if quadrat ID's are integers.
*   New Features: NA

# RMAVIS 0.9992 (2024-04-17)
*   Modifications:
      -   Updating author list.
*   Fixes: NA
*   New Features: NA

# RMAVIS 0.9991 (2024-04-17)
*   Modifications:
      -   Updating author list.
*   Fixes: NA
*   New Features: NA

# RMAVIS 0.999 (2024-04-16)
*   Modifications:
      -   Updating to taxonomic backbone to use BSBI February 2024 taxon list.
      -   Minimising the number of bundled datasets.
      -   Removing assignNVC dependency.
*   Fixes:
      -   Ensuring cover values are passed to surveyData_mat for use in Simpson 
          diversity indices.
      -   Ensure all assignment results reset upon Run Analysis, even if not run.
*   New Features:
      -   Adding option to disable the calculation of similarities for quadrats.
      -   Adding Scottish oceanic wet grassland communities.
      -   Ensuring all functions are covered by tests.
      -   Adding JOSS paper.
      -   Adding ability to turn quadrat similarities on or off.
      -   Adding Scottish oceanic wet grassland communities.

# RMAVIS 0.995 (2024-04-01)
*   Modifications:
      -   Improving function to compose syntopic tables.
*   Fixes:
      -   Adding .csv extension to downloadable csv files.
*   New Features: NA

# RMAVIS 0.994 (2024-03-26)
*   Modifications: NA
*   Fixes:
      -   Ensuring cover scale is correctly linked to survey data table.
      -   Ensuring RMAVIS results .xlsx can be downloaded if the number of plots per group and year-group are less than 5.
      -   Fixing NPMS M habitat correspondences.
*   New Features:
      -   Adding button to clear survey data table.

# RMAVIS 0.993 (2024-03-21)
*   Modifications: NA
*   Fixes:
      -   Fixing MAVIS upload.
*   New Features: NA

# RMAVIS 0.992 (2024-03-19)
*   Modifications: NA
*   Fixes:
      -   Fixing bryophyte filtering from example data.
*   New Features: NA

# RMAVIS 0.991 (2024-03-19)
*   Modifications: NA
*   Fixes:
      -   Ensuring "Confirm Upload" button is disabled when the uploaded data column names are incorrect.
*   New Features:
      -   Adding downloadable .xslx containing RMAVIS results.
      -   Adding Cover Scale option.
      -   Adding option to upload old MAVIS files.
      -   Adding Habitat Correspondences to report.

# RMAVIS 0.99 (2024-03-14)
*   Modifications: NA
*   Fixes:
      -   Ensuring all constants are bundled in sysdata.rda.
      -   Ensuring `RMAVIS` package contents are loaded when starting app.
*   New Features: NA

# RMAVIS 0.983 (2024-03-14)
*   Modifications:
      -   Converting to R package format.
*   Fixes:
      -   Fixing MVA select quadrats.
*   New Features:
      -   Adding multiple composed floristic tables.

# RMAVIS 0.982 (2024-03-11)

*   Modifications: NA
*   Fixes:
      -   Fixing Species.Ignore == "Yes"
*   New Features: NA

# RMAVIS 0.981 (2024-03-09)

*   Modifications: NA
*   Fixes:
      -   Ensuring species-quadrat duplicate check is included in ok to proceed.
      -   Fixing jump issue in species adjustment table, now using Yes and No.
*   New Features: NA

# RMAVIS 0.98 (2024-03-06)

*   Modifications: NA
*   Fixes:
      -   Ensuring manual entry works.
      -   Ensuring single plot samples work.
*   New Features: NA

# RMAVIS 0.97 (2024-03-05)

*   Modifications: NA
*   Fixes:
      -   Updating documentation to reflect new MVA functionality.
      -   Fixing group survey plots option in MVA module.
*   New Features: NA

# RMAVIS 0.96 (2024-03-02)

*   Modifications:
    -   Changing name, MAVIS -> RMAVIS
    -   Removing site and group Jaccard similarity results.
    -   Refining code throughout.
    -   Trimming dependencies.
*   Fixes: NA
*   New Features:
    -   Adding pseudo-quadrat and sample quadrat centroids in MVA.
    -   Adding setupData module which allows modification of bundled datasets.
    -   Adding a bryophyte switch to include-exclude bryophyte taxa.
    -   Now loading dependencies in `load_packages.R`.

# RMAVIS 0.95 (2024-01-24)

*   Modifications:
    -   Removing unnecessary upload data checks.
    -   Making survey data availability table searchable.
    -   Changing nomenclature for releve to plot.
*   Fixes:
    -   Ensuring report renders correctly using new surveyDataSummary
        object data.
*   New Features: NA

# RMAVIS 0.94 (2024-01-18)

*   Modifications: NA
*   Fixes:
    -   Ensuring the number of quadrats per group is passed to the
        floristic tables module through the new survey data summary
        module.
*   New Features: NA

# RMAVIS 0.93 (2024-01-18)

*   Modifications: NA
*   Fixes: NA
*   New Features:
    -   Adding survey data summary module.
    -   Adding quadrat re-grouping tool.

# RMAVIS 0.92 (2024-01-18)

*   Modifications: NA
*   Fixes: NA
*   New Features:
    -   Adding Czekanowski index method for Site and Group NVC
        assignment calculations.

# RMAVIS 0.91 (2024-01-11)

*   Modifications: NA
*   Fixes: NA
*   New Features:
    -   Adding privacy policy section

# RMAVIS 0.9 (2024-01-09)

*   First release for user testing.
*   Modifications: NA
*   Fixes: NA
*   New Features: NA
