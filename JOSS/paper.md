---
title: 'RMAVIS v1.0: a Shiny application for the analysis of vegetation survey data and assignment to GB NVC communities.'
tags:
  - Vegetation science
  - NVC
  - Ecology
  - Survey
authors:
  - name: Zeke Marshall
    orcid: 0000-0001-9260-7827
    affiliation: 1
  - name: Simon Mark Smart
    orcid: 0000-0003-2750-7832
    affiliation: 1
  - name: Colin Harrower
    orcid: 0000-0001-5070-5293
    affiliation: 2
  - name: Rob Marrs
    orcid: 0000-0002-0664-9420
    affiliation: 3
affiliations:
  - name: UK Centre for Ecology \& Hydrology, Lancaster Environment Centre, Library Avenue, Bailrigg, Lancaster, LA1 4AP, United Kingdom
    index: 1
  - name: UK Centre for Ecology \& Hydrology, Maclean Building, Benson Lane, Crowmarsh Gifford, Wallingford, Oxfordshire, OX10 8BB, United Kingdom
    index: 2
  - name: School of Environmental Sciences, University of Liverpool, Liverpool, L69 3GP, UK
    index: 3
date: "2024-08-07"
bibliography: paper.bib
output:
  html_document:
    keep_md: yes
---
  
# Summary

`RMAVIS` is a Shiny application for the assignment of vegetation sample
plot data to British National Vegetation Classification (NVC) 
communities [@rodwell1991;@rodwell1992;@rodwell1992a;@rodwell1995;@rodwell2000].

The assignment of vegetation sample plot data to established vegetation 
classification units using computational methods is a well established and 
recognised practice [@maciejewski2020]. 
The results of this assignment process are used in various ways, 
including assisting in the phase 2 habitat survey
(or NVC survey) process [@rodwell2006national]; 
establishing an ecological baseline and identifying important ecological 
features such as protected habitats [@cieem2022]; 
and in ecological restoration by providing a proxy for historical reference 
ecosystems to target and against which to measure restoration progress 
[@pywell2002; @gann2019; @sturbois2023].

In Great Britain (GB) the development of computational methods and programs 
for the assignment of vegetation survey data to NVC communities began with the 
development of TABLEFIT [@hill1989; @marrs2019] and was followed by MATCH 
[@malloch1998].
The most recent program, the Modular Analysis of Vegetation Information System 
(MAVIS), was developed as a windows application in 2000 and was a DEFRA-funded
output of the ECOFACT project [@bunce1999], with the latest version released in 
2016 [@smart2016mavis].


# Statement of need

The requirement for a new program in the form of a Shiny [@chang2024] 
application arises from several needs, namely to: 
1) accommodate updates to the NVC;
2) provide a means to easily reproduce and attribute the results of the NVC
assignment process;
3) broaden and simplify access through the provision of a web-based Graphical 
User Interface (GUI);
and
4) facilitate the continuous development of such NVC assignment software.
We developed `RMAVIS` considering these needs, with the view to providing
a reliable system for use by the GB ecology and conservation community, 
analogous to the Engine for Relevés to Irish Communities Assignment (ERICA)
tool [@perrin2018; @perrin2019a] developed for Ireland, and the EuroVegChecklist
Expert System (EVC-ES) [@mucina2016a] developed for Europe.

# Application structure

Inspired by the extensible structure of the species niche and distribution 
modelling Shiny application `wallace` [@kass2023], we constructed `RMAVIS` 
with a modular architecture, facilitating both the maintenance of existing 
modules and development of new modules with ease.

`RMAVIS` currently contains a total of twenty-one modules, with the fourteen main 
modules summarised in the following table.

|        **Module**       |                                                          **Description**                                                          |
|:-----------------------:|:---------------------------------------------------------------------------------------------------------------------------------:|
|         Sidebar         |                                Acts as the control module, containing the options for each module.                                |
|        Data Input       |                      Facilitates the entry of data, three methods are provided: manual, upload, and example.                      |
|     Data Validation     |                 Checks the format of the inputted data and provides means to adjust incorrectly formatted entries.                |
|      Data Structure     | Checks the structure of the inputted data, displaying the availability of data by species and number of plots per year and group. |
|      NVC Assignment     |                                        Displays the results of the NVC assignment process.                                        |
|  Habitat Correspondence |           Displays the habitats from alternative habitat classifications associated with the top-fitted NVC communities.          |
|     Floristic Tables    |             Allows the comparison of floristic tables composed from the inputted data with the NVC's floristic tables.            |
|        Frequency        |                         Summarises the frequency of occurrence of each species across all plots over time.                        |
|           EIVs          |         Displays the mean Hill-Ellenberg ecological indicator values (EIVs) for each plot, group of plots, and all plots.         |
|        Diversity        |                              Displays a number of diversity metrics for each plot and group of plots.                             |
|       MVA National      |                Constructs an ordination space using all NVC pseudo-quadrats, with sample plots added in passively.                |
|  MVA Local, restricted  |     Constructs an ordination space using the top-fitted NVC communities pseudo-quadrats, with sample plots added in passively.    |
| MVA Local, unrestricted |               Constructs an ordination space using the top-fitted NVC communities pseudo-quadrats and sample plots.               |
|          Report         |                Provides the user with a downloadable report, containing user-selected outputs from the app session.               |



![The MVA module of `RMAVIS`, showing the trajectory of all sample plots from the Leith Hill Wood example dataset in the ordination spaces over time.](joss_screenshot.png)

# Data sources and dependencies

`RMAVIS` depends on a number of individual datasets.
The list of accepted species was constructed using 
the vascular plant (*Tracheophyta* and *Pteridophyta*), moss (*Bryophyta*), 
liverwort (*Marchantiophyta*), and hornwort (*Anthocerotophyta*) taxa present in 
the UKSI [@raper2015], retrieved from the National Biodiversity Network (NBN), 
[@nbn2024Pteridophyta; @nbn2024Tracheophyta; @nbn2024Bryophyta; 
@nbn2024Marchantiophyta; @nbn2024Anthocerotophyta] filtered to 
include taxa at the species, species hybrid, species aggregate, subspecies, 
species sensu lato, and genus ranks; 
along with  the limited number of lichen (*Lecanoromycetes*), 
charophyte (*Charophyta*), and alga (*Algae*) taxa present in the NVC floristic 
tables.
The NVC communities present in `RMAVIS` are composed from @rodwell1991, 
@rodwell1992, @rodwell1992a, @rodwell1995, @rodwell2000, 
@wallace2017, @prosser2023, and @wallace2023.
Data for habitat correspondences is derived from: 
the Joint Nature Conservation Committee (JNCC) Spreadsheet of Habitat 
Correspondences [@jncc2008],
UKHab V1.1 [@butcher2020], and
the National Plant Monitoring Scheme (NPMS) habitat correspondences
[@pescott2019].
Data for the vascular plant Hill-Ellenberg values were retrieved from the
BSBI checklists, specifically the Nitrogen Score (N)
[@bsbi2017ellenbergN], Moisture Score (F) [@bsbi2017ellenbergF],
Reaction Score (R) [@bsbi2017ellenbergR], Salinity Score (S)
[@bsbi2017ellenbergS], and Light Score (L) [@bsbi2017ellenbergL] checklists. 
The corresponding data for bryophytes was taken from BRYOATT [@hill2007].
Four example datasets are bundled with `RMAVIS`: 
1) Parsonage Down [@ridding2020],
2) Leith Hill Place Wood [@wood2015; @smart2024],
3) Whitwell Common [@smart2000],
and
4) Newborough Warren [@wallace2018].

Version 1.0 of `RMAVIS` was built under R 4.4.0 [@rcoreteam] and depends on the 
`bookdown` [@xie2024a],
`bsicons` [@sievert2023a],
`bslib` [@sievert2024a],
`dplyr` [@wickham2019],
`ggplot2` [@wickham2019],
`htmltools` [@cheng2024],
`htmlwidgets` [@vaidyanathan2023],
`kableExtra` [@zhu2024a],
`knitr` [@xie2024c],
`magrittr` [@bache2022a],
`markdown` [@xie2024d],
`plotly` [@sievert2020],
`purrr` [@wickham2019],
`reactable` [@lin2023],
`readr` [@wickham2019],
`rhandsontable` [@owen2021a],
`rmarkdown` [@allaire2024],
`shiny` [@chang2024],
`shinybusy` [@meyer2024a],
`shinyjs` [@attali2021],
`shinyWidgets` [@perrier2024a], 
`stringr` [@wickham2019],
`tibble` [@wickham2019],
`tidyr` [@wickham2019],
`vegan` [@oksanen2024a], and
`writexl` [@ooms2024a] R packages.

# Conclusion

`RMAVIS` provides a web-based, easily accessible GUI for the assignment of
vegetation sample plot data to GB NVC communities. 
`RMAVIS` also provides a number of other exploratory analyses, 
which compliment the NVC assignment results.
We plan to maintain, optimise, and expand the functionality found in v1.0 of 
`RMAVIS` and hope that it acts as a useful tool for the GB ecology and 
conservation community.

# Acknowledgements

The development of `RMAVIS` was partly supported by the UK‐SCAPE program 
delivering National Capability (NE/R016429/1) funded by the Natural Environment 
Research Council.

We would like to thank
Lindsay Maskell,
Lucy Ridding,
Barry Jobson,
Colin Conroy,
Rob Marrs,
Andy McMullen,
John Handley,
Michael Tso,
Simon Rolph,
Cristina Martin Hernandez,
and
George Linney
for testing `RMAVIS`.

We would also like to thank Laurence Jones for providing the Newborough
Warren example dataset and Lucy Ridding for providing the Parsonage Down
example dataset.

# References
