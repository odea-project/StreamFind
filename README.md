
<!-- README.md is generated from README.Rmd. Please edit that file -->

# streamFind

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The streamFind project, entitled “Flexible data analysis and workflow
designer to identify chemicals in the water cycle”, is funded by the
[Bundesministerium für Bildung und Forschung
(BMBF)](https://www.bmbf.de) and is a cooperation between the [Institut
für Energie- und Umwelttechnik e. V. (IUTA)](https://www.iuta.de), the
[Forschungszentrum Informatik (FZI)](https://www.fzi.de/) and supporting
partners. The goal of the streamFind project is the development and
assembly of data processing workflows for mass spectrometry and
spectroscopy and the application of the workflows in environmental and
quality studies of the water cycle. The streamFind aims to stimulate the
use of advanced data analysis (e.g., non-target screening, statistical
analysis, etc.) in routine studies, promoting standardization of data
processing and structure and easing the retrospective evaluation of
data. The streamFind platform is directed to academics but also
technicians, due to the aspired comprehensive documentation, well
categorized set of integrated modular functions and the graphical user
interface. The streamFind development is ongoing, please [contact
us](mailto:cunha@iuta.de) for questions or collaboration.

<img src="man/figures/logo_group.png" width="100%" style="display: block; margin: auto;" />

# streamFind R package

The back-end framework of
[streamFind](https://github.com/ricardobachertdacunha/streamFind) is an
R package.

## Installation

For installation of the streamFind R package, it is recommended to first
install the dependencies. Besides [R](https://cran.r-project.org/) and
[RTools](https://cran.r-project.org/bin/windows/Rtools/) (the latter is
only recommended for Windows users), the streamFind depends on the
[patRoon](https://github.com/rickhelmus/patRoon) R package and its
dependencies. The patRoon R package combines several tools for basic and
advanced data processing and can be used interchangeably with the
streamFind R package. Installation instructions for patRoon and its
dependencies can be found
[here](https://rickhelmus.github.io/patRoon/handbook_bd/manual-installation.html#r-prerequisites).

Then, the streamFind R package can be installed from the GitHub
repository.

``` r
remotes::install_github("ricardobachertdacunha/streamFind", dependencies = TRUE)
```

The supplementary
[streamFindData](https://github.com/ricardobachertdacunha/streamFindData)
R package holds the data used in examples and other documentation assets
of the streamFind R package and can also be installed from the GitHub
repository.

``` r
remotes::install_github("ricardobachertdacunha/streamFindData")
```

### Documentation

The documentation and usage examples of the streamFind R package can be
found in the [reference
page](https://ricardobachertdacunha.github.io/streamFind/reference/index.html)
and
[articles](https://ricardobachertdacunha.github.io/streamFind/articles/index.html)
of the
[webpage](https://ricardobachertdacunha.github.io/streamFind/index.html).
