
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
für Energie- und Umwelttechnik e. V. (IUTA)](https://www.iuta.de) and
[Forschungszentrum Informatik (FZI)](https://www.fzi.de/). The major
focus of streamFind is the development and assembly of workflows for
processing data from mass spectrometry and spectroscopy and for
application the workflows in environmental and quality studies of the
water cycle. The streamFind aims to stimulate the use of advanced data
analysis (e.g., non-target screening, statistical analysis, etc.) in
routine studies, promoting standardization of data processing and
structure and easing the retrospective evaluation of data. The
streamFind package can be used by academics but also by technicians due
to the comprehensive documentation and well categorized set of
integrated functions or modules (see vignettes for more information).

<img src="man/figures/logo_group.png" width="100%" style="display: block; margin: auto;" />

# streamFind R package

The R package
[streamFind](https://github.com/ricardobachertdacunha/streamFind) is the
back-end framework for the streamFind project and can be obtained with
this repository.

## Installation

For installation of the streamFind R package, it is recommended to first
install the dependencies. Besides [R](https://cran.r-project.org/) and
[RTools](https://cran.r-project.org/bin/windows/Rtools/) (the latter is
only recommended for Windows users), the streamFind depends on the
[patRoon](https://github.com/rickhelmus/patRoon) R package. The patRoon
R package combines several tools for basic and advanced data processing
and can be used interchangeably with streamFind. The
[handbook](https://rickhelmus.github.io/patRoon/handbook_bd/manual-installation.html#r-prerequisites)
of the patRoon R package offers detailed installation instructions,
including the list of required
[dependencies](https://rickhelmus.github.io/patRoon/handbook_bd/manual-installation.html#other-dependencies).

The streamFind R package can then be installed from the GitHub
repository.

``` r
remotes::install_github("ricardobachertdacunha/streamFind", dependencies = TRUE)
```

The
[streamFindData](https://github.com/ricardobachertdacunha/streamFindData)
R package holds the data used in examples and other documentation assets
of the streamFind R package and can be installed from the GitHub
repository.

``` r
remotes::install_github("ricardobachertdacunha/streamFindData")
```

### Documentation

The documentation and usage examples of the streamFind package can be
found in the [reference
page](https://ricardobachertdacunha.github.io/streamFind/reference/index.html)
and
[articles](https://ricardobachertdacunha.github.io/streamFind/articles/index.html)
of the
[webpage](https://ricardobachertdacunha.github.io/streamFind/index.html),
respectively.
