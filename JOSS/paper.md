---
title: '`RMAVIS` v1.0: a R Shiny application for the analysis of vegetation survey data and assignment to UK NVC communities.'
tags:
  - Vegetation science
  - NVC
  - Ecology
  - Survey
authors:
  - name: Zeke Marshall
    orcid: 0000-0001-9260-7827
    affiliation: 1
  - name: Simon Smart
    orcid: 0000-0003-2750-7832
    affiliation: 1
  - name: Colin Harrower
    orcid: 0000-0001-5070-5293
    affiliation: 2
affiliations:
  - name: UK Centre for Ecology \& Hydrology, Lancaster Environment Centre, Library Avenue, Bailrigg,Lancaster, LA1 4AP, United Kingdom
    index: 1
  - name: UK Centre for Ecology \& Hydrology, Maclean Building, Benson Lane, Crowmarsh Gifford, Wallingford, Oxfordshire, OX10 8BB, United Kingdom
    index: 2
date: "2024-04-11"
bibliography: paper.bib
output:
  html_document:
    keep_md: yes
---
  
# Summary

`RMAVIS` is a R Shiny application for the assignment of vegetation sample
plot data to British National Vegetation Classification (NVC) 
communities [@rodwell2000].

The assignment of vegetation sample plot data to established vegetation 
classification units using computational methods is a well established and 
recognised practice [@maciejewski2020]. 
The results of this assignment process are used in various ways, 
including assisting in the phase 2 habitat survey
(or NVC survey) [@rodwell2006national] process; 
establishing an ecological baseline and identifying important ecological 
features such as protected habitats [@cieem2022]; 
and in ecological restoration by providing a proxy for historical reference 
ecosystems to target and against which to measure restoration progress 
[@pywell2002; @gann2019; @sturbois2023].

In the Great Britain (GB) the development of computational methods and programs 
for the assignment of vegetation survey data to NVC communities began with the 
development of TABLEFIT [@hill1989; @marrs2019] and was followed by MATCH 
[@malloch1998].
The most recent program, the Modular Analysis of Vegetation Information System 
(MAVIS), was developed as a windows application in 2000 and was a DEFRA-funded
output of the ECOFACT project [@bunce1999].
The latest version released in 2016 [@smart2016mavis].


# Statement of need

The requirement for a new program in the form of a R Shiny [@chang2024] 
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
tool [@perrin2018; @perrin2019a] developed for Ireland.

# Application structure

Inspired by the extensible structure of the species niche and distribution 
modelling R shiny application `wallace` [@kass2023], we constructed `RMAVIS` with 
a modular architecture, enabling both the easy maintenance of existing modules, 
and easy development of additional modules in the future.

`RMAVIS` currently contains a total of eighteen modules, with the fourteen main 
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



![The MVA module of `RMAVIS`, showing the trajectory of all sample plots from the Leith Hill Wood example dataset in the ordination spaces over time.](images/Screenshot from 2024-03-05 12-25-49.png)

# Data sources

`RMAVIS` depends on a number of individual datasets, 
the sources of which are as follows.
The list of accepted species was constructed using 
the vascular plant (*Tracheophyta*) taxa present in the 
Botanical Society of Britain and Ireland's (BSBI) database 
[@bsbi2022TaxonLists];
filtered to include taxa at the genus, species, hybrid species, and subspecies 
ranks which have an associated UK Species Inventory (UKSI) Taxon Version Key 
(TVK); the moss (*Bryophyta*), liverwort (*Marchantiophyta*), 
and hornwort (*Anthocerotophyta*) taxa present in the UKSI, 
retrieved from the National Biodiversity Network (NBN), filtered to include taxa 
at the species, species aggregate, subspecies, species sensu lato, 
and genus ranks; and the limited number of lichen (*Lecanoromycetes*) and 
charophyte (*Charophyta*) taxa, along with one unspecified 'algae' taxon present 
in the NVC floristic tables.
The NVC communities present in `RMAVIS` are composed from @rodwell2000, 
@wallace2017, and @prosser2023.
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
[@bsbi2017ellenbergS], and Light Score (L) [@bsbi2017ellenbergL]. 
The corresponding data for bryophytes was taken from BRYOATT [@hill2007].
Four example datasets are bundled with `RMAVIS`: 
1) Parsonage Down [@ridding2020],
2) Leith Hill Place Wood [@wood2015; @smart2024],
3) Whitwell Common [@smart2000],
and
4) Newborough Warren [@wallace2018].

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
