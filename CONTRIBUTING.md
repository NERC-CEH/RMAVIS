# Contributing to `RMAVIS`
We welcome contributions to `RMAVIS` or suggestions for improvement. However,
we first ask that you email [Zeke Marshall](mailto:zekmar@ceh.ac.uk?subject=RMAVIS)
to discuss the the scope of the contribution to ensure that any contributions
are aligned with the development strategy for `RMAVIS`.

## Development
If you would like to collaborate on the development of `RMAVIS` please take the following steps:
1. Fork the repository and create a new branch.
2. Run `renv::restore()` to install the current dependencies of `RMAVIS`, along with development helper packages such as `{usethis}`.
3. Develop the code you wish to contribute.
4. Add tests for any new functions placed in the ./R folder.
5. Add provisional release notes in the NEWS.md file throughout development.
6. Update the DESCRIPTION file to include the following provisional information:
    -   An updated version number.
    -   Any changes to the dependencies.
    -   Your author details.
7. Add your author details to CITATION.cff.
8. Update the version number in the app Documentation.Rmd file, Report.Rmd file, README.Rmd file and re-render the documentation and README.
9. Update the renv.lock file to capture the dependencies used to make your contribution using `renv::snapshot()`.
10. Document any additional bundled data in ./R/data.R
11. Re-render the package documentation.
12. Check the package builds.
13. Check the package installs.
14. Ensure that the package checks pass (at present there are some notes and warnings that I need to clear up).
15. Source the ./data-raw/prepare_appfiles.R to ensure that the README.md and NEWS.md are copied to the app directory.
16. Tag @ZekeMarshall in any issues, or when you wish to make a pull request.

## Issues
To report bugs, suggest enhancements, request advice on usage, or ask a question please raise an issue [here](https://github.com/NERC-CEH/RMAVIS/issues).

## License
By contributing, you agree that your contributions will be licensed under the GNU Lesser General Public License.
