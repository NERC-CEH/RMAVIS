
# RMAVIS

<!-- badges: start -->

[![Generic badge](https://img.shields.io/badge/Version-1.0-green.svg)]()
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![DOI](https://zenodo.org/badge/722095560.svg)](https://zenodo.org/badge/latestdoi/722095560)
[![status](https://joss.theoj.org/papers/460c6f934a108fcf5a16d0f2ab77492e/status.svg)](https://joss.theoj.org/papers/460c6f934a108fcf5a16d0f2ab77492e)
[![License: CC BY
4.0](https://img.shields.io/badge/License-LGPL%203.0-lightgrey.svg)](https://opensource.org/license/lgpl-3-0)
<!-- badges: end -->

## Overview

The **R** **M**odular **A**nalysis of **V**egetation **I**nformation
**S**ystem (`RMAVIS`) R Shiny web application is a tool to assist in the
assignment of vegetation plot sample data to GB National Vegetation
Classification units, with additional exploratory analyses.

We will update and improve `RMAVIS` over time in response to user
feedback and hope it acts as a useful tool for the GB ecology and
conservation community.

## Structure

There are 5 main sections in `RMAVIS` at present.

**Home** - Provides outlining information.

**Application** - The core of `RMAVIS`, containing the actual
application.

**NVC Lookup** - Contains a searchable table, which can be used to
retrieve the full name of an NVC community using an NVC community or
sub-community code.

**Documentation** - Provides a more detailed overview of the underlying
methods, data sources, and usage instructions.

**News** - Contains the release log and additional news regarding the
development and maintenance of `RMAVIS`.

**Privacy** - A privacy notice.

## Running Locally

In addition to accessing `RMAVIS` via the web `RMAVIS` can be run on
your personal device and installed as an R package.

To run RMAVIS locally take the following steps.

1.  Ensure R \>= 4.4 is installed.
2.  Install `RMAVIS` using `remotes::install_github("NERC-CEH/RMAVIS")`
3.  Run `install.packages("tinytex")` then `tinytex::install_tinytex()`
    to install a minimal LaTeX distribution. This is required to
    generate a pdf report.
4.  From the R terminal run `RMAVIS::runApp()`.

If you wish to use the NVC-matching functions outside of the `RMAVIS`
app, these are also made available through the `RMAVIS` package.

## Reccomended Citation

To reference `RMAVIS` please cite the JOSS paper (in prep)…

Alternatively, use either the Zenodo DOI for all versions as follows:

Marshall, Z., Smart, S. M., Harrower, C., & Marrs, R. (2024). RMAVIS.
Zenodo. <https://zenodo.org/doi/10.5281/zenodo.10818640>

or the DOI associated with a particular release.

## Feedback

To report a bug please submit a Github issue
[here](https://github.com/NERC-CEH/RMAVIS/issues), fill out the feedback
form [here](https://forms.office.com/e/ByLgRPjT8J), or send an email to
[Zeke Marshall](mailto:zekmar@ceh.ac.uk?subject=RMAVIS).

## Acknowledgements

The development of this app was partly supported by the UK‐SCAPE
programme delivering National Capability (NE/R016429/1) funded by the
Natural Environment Research Council.

We would like to thank Lindsay Maskell, Lucy Ridding, Barry Jobson,
Colin Conroy, Andy McMullen, John Handley, Michael Tso, Simon Rolph,
Cristina Martin Hernandez, and George Linney for testing RMAVIS.

We would also like to thank Rob Marrs for his ongoing collaboration with
the development of NVC assignment methodologies and the University of
Liverpool for their ongoing support.
