---
title: "Release notes for `RMAVIS`"
output: html_document
---

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
