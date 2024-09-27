
<!-- README.md is generated from README.Rmd. Please edit that file -->

# StreamFind (R package)

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

<p align="center" width="100%">
<img width="60%" src="man/figures/logo_StreamFind.png" alt="Logo" />
</p>

The StreamFind is developed within the project [“Flexible data analysis
and workflow designer to identify chemicals in the water
cycle”](https://www.bildung-forschung.digital/digitalezukunft/de/wissen/Datenkompetenzen/datenkompetenzen_wissenschaftlichen_nachwuchs/Projekte/stream_find.html),
which is funded by the [Bundesministerium für Bildung und Forschung
(BMBF)](https://www.bmbf.de). The development is carried out by the
[Institut für Umwelt & Energie, Technik & Analytik e. V.
(IUTA)](https://www.iuta.de), the [Forschungszentrum Informatik
(FZI)](https://www.fzi.de/) and supporting partners. The goal of the
StreamFind is to develop and assemble data processing workflows for
different types of analytical data (e.g., mass spectrometry and
spectroscopy) and to apply the workflows in different fields (e.g.,
environmental and quality studies of the water cycle). StreamFind aims
to stimulate the use of advanced data analysis (e.g., non-target
screening, statistical analysis, etc.) in routine studies, to promote
standardization of data structure and processing, and to facilitate
retrospective data evaluation. The StreamFind platform is aimed at
scientists, but also at technicians, due to its comprehensive
documentation, its well categorized set of integrated modular functions
and its embedded graphical user interface.  
<br> The StreamFind development is ongoing, please [contact
us](mailto:cunha@iuta.de) for questions or collaboration.

<img src="man/figures/logo_group.png" width="100%" style="display: block; margin: auto;" />

## Installation

Pre-requisites for the StreamFind are the
[R](https://cran.r-project.org/) software and the
[RTools](https://cran.r-project.org/bin/windows/Rtools/) (only
applicable for Windows users). RTools is needed for compiling the C++
code used in the StreamFind R package. Assuming that R and RTools are
installed, the StreamFind R package can be installed from the GitHub
repository via the [BiocManager](https://www.bioconductor.org/install/).

``` r
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("odea-project/StreamFind", dependencies = TRUE)
```

## Docker Setup

Build the Docker Image

``` r
docker build -t my-r-app .
```
Start the Container

``` r
docker-compose up
```
Once the container is up, you'll be prompted to select the service you want to run:

Option 1: Run Shiny App
Starts the Shiny application, accessible at http://localhost:3838.

Option 2: Run RStudio Server
Starts the RStudio Server, accessible at http://localhost:8787.

Default credentials:

Username: rstudio

Password: rstudio

Option 3: Run Both Shiny App and RStudio Server

Stopping the Container
``` r
docker-compose down
```

## Other dependencies

The StreamFind depends on other open source software to process
different analytical data. For instance, for non-target screening using
mass spectrometry the StreamFind uses the
[patRoon](https://github.com/rickhelmus/patRoon) R package and its own
dependencies. Installation instructions for patRoon and its dependencies
can be found
[here](https://rickhelmus.github.io/patRoon/handbook_bd/manual-installation.html#r-prerequisites).
Consult the documentation for dependencies of other data types.

## Suplementary data

The supplementary
[StreamFindData](https://github.com/odea-project/StreamFindData) R
package holds the data used in examples and other documentation assets
of the StreamFind and can also be installed from the GitHub repository.

``` r
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("odea-project/StreamFindData")
```

## Documentation

The documentation and usage examples of the StreamFind R package can be
found in the [reference
page](https://odea-project.github.io/StreamFind/reference/index.html)
and
[articles](https://odea-project.github.io/StreamFind/articles/index.html)
of the [webpage](https://odea-project.github.io/StreamFind/index.html),
respectively.

## References

The StreamFind is open source due to public funding and the extensive
contribution from scientific literature as well as existing open source
software. Below, we reference the research and software that is used
within StreamFind. Please note that each open source software or
research that StreamFind uses relies on other contributions. Therefore,
we recommend to search within each citation for other contributions.  
<br>

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-xcms03" class="csl-entry">

Benton, H. Paul, Elizabeth J. Want, and Timothy M. D. Ebbels. 2010.
“Correction of Mass Calibration Gaps in Liquid Chromatography-Mass
Spectrometry Metabolomics Data.” *BIOINFORMATICS* 26: 2488.

</div>

<div id="ref-proteo02" class="csl-entry">

Chambers, M. C., B. Maclean, R. Burke, D Amodei, D. L. Ruderman, S.
Neumann, L. Gatto, et al. 2012a. “A Cross-Platform Toolkit for Mass
Spectrometry and Proteomics.” *Nature Biotechnology* 30 (10): 918–20.
<https://doi.org/10.1038/nbt.2377>.

</div>

<div id="ref-mzr04" class="csl-entry">

Chambers, Matthew C., Maclean, Brendan, Burke, Robert, Amodei, et al.
2012b. “<span class="nocase">A cross-platform toolkit for mass
spectrometry and proteomics</span>.” *Nat Biotech* 30 (10): 918–20.
<https://doi.org/10.1038/nbt.2377>.

</div>

<div id="ref-msnbase02" class="csl-entry">

Gatto, Laurent, Sebastian Gibb, and Johannes Rainer. 2020. “MSnbase,
Efficient and Elegant r-Based Processing and Visualisation of Raw Mass
Spectrometry Data.” *bioRxiv*.

</div>

<div id="ref-msnbase01" class="csl-entry">

Gatto, Laurent, and Kathryn Lilley. 2012. “MSnbase - an r/Bioconductor
Package for Isobaric Tagged Mass Spectrometry Data Visualization,
Processing and Quantitation.” *Bioinformatics* 28: 288–89.

</div>

<div id="ref-patroon01" class="csl-entry">

Helmus, Rick, Thomas L. ter Laak, Annemarie P. van Wezel, Pim de Voogt,
and Emma L. Schymanski. 2021. “patRoon: Open Source Software Platform
for Environmental Mass Spectrometry Based Non-Target Screening.”
*Journal of Cheminformatics* 13 (1).
<https://doi.org/10.1186/s13321-020-00477-w>.

</div>

<div id="ref-patroon02" class="csl-entry">

Helmus, Rick, Bas van de Velde, Andrea M. Brunner, Thomas L. ter Laak,
Annemarie P. van Wezel, and Emma L. Schymanski. 2022. “patRoon 2.0:
Improved Non-Target Analysis Workflows Including Automated
Transformation Product Screening.” *Journal of Open Source Software* 7
(71): 4029. <https://doi.org/10.21105/joss.04029>.

</div>

<div id="ref-kpic01" class="csl-entry">

Ji, Hongchao, Fanjuan Zeng, Yamei Xu, Hongmei Lu, and Zhimin Zhang.
2017. “KPIC2: An Effective Framework for Mass Spectrometry-Based
Metabolomics Using Pure Ion Chromatograms.” *Anal Chem.* 14 (89):
7631–40. <https://doi.org/10.1021/acs.analchem.7b01547>.

</div>

<div id="ref-pugixml01" class="csl-entry">

Kapoulkine, Arseny. 2022. “Pugixml 1.13: Light-Weight, Simple and Fast
XML Parser for c++ with XPath Support.” *Copyright (C) 2006-2018*.
<http://pugixml.org>.

</div>

<div id="ref-mzr02" class="csl-entry">

Keller, Andrew, Jimmy Eng, Ning Zhang, Xiao-jun Li, and Ruedi Aebersold.
2005. “A Uniform Proteomics MS/MS Analysis Platform Utilizing Open XML
File Formats.” *Mol Syst Biol*.

</div>

<div id="ref-proteo01" class="csl-entry">

Kessner, Darren, Matt Chambers, Robert Burke, David Agus, and Parag
Mallick. 2008. “ProteoWizard: Open Source Software for Rapid Proteomics
Tools Development.” *Bioinformatics* 24 (21): 2534–36.
<https://doi.org/10.1093/bioinformatics/btn323>.

</div>

<div id="ref-mdatools01" class="csl-entry">

Kucheryavskiy, Sergey. 2020. “Mdatools – r Package for Chemometrics.”
*Chemometrics and Intelligent Laboratory Systems* 198: 103937.
https://doi.org/<https://doi.org/10.1016/j.chemolab.2020.103937>.

</div>

<div id="ref-camera01" class="csl-entry">

Kuhl, C., R. Tautenhahn, C. Boettcher, T. R. Larson, and S. Neumann.
2012. “CAMERA: An Integrated Strategy for Compound Spectra Extraction
and Annotation of Liquid Chromatography/Mass Spectrometry Data Sets.”
*Analytical Chemistry* 84: 283–89.
<http://pubs.acs.org/doi/abs/10.1021/ac202450g>.

</div>

<div id="ref-mzr03" class="csl-entry">

Martens, Lennart, Matthew Chambers, Marc Sturm, Darren Kessner, Fredrik
Levander, Jim Shofstahl, Wilfred H Tang, et al. 2010. “MzML - a
Community Standard for Mass Spectrometry Data.” *Mol Cell Proteomics*.
<https://doi.org/10.1074/mcp.R110.000133>.

</div>

<div id="ref-genform" class="csl-entry">

Meringer, Markus, Stefan Reinker, Juan Zhang, and Alban Muller. 2011.
“MS/MS Data Improves Automated Determination of Molecular Formulas by
Mass Spectrometry.” *MATCH Commun. Math. Comput. Chem* 65 (2): 259–90.

</div>

<div id="ref-mzr01" class="csl-entry">

Pedrioli, Patrick G A, Jimmy K Eng, Robert Hubley, Mathijs Vogelzang,
Eric W Deutsch, Brian Raught, Brian Pratt, et al. 2004. “A Common Open
Representation of Mass Spectrometry Data and Its Application to
Proteomics Research.” *Nat Biotechnol* 22 (11): 1459–66.
<https://doi.org/10.1038/nbt1031>.

</div>

<div id="ref-qcentroids01" class="csl-entry">

Reuschenbach, Max, Lotta L. Hohrenk-Danzouma, Torsten C. Schmidt, and
Gerrit Renner. 2022. “Development of a Scoring Parameter to Characterize
Data Quality of Centroids in High-Resolution Mass Spectra.” *Analytical
and Bioanalytical Chemistry* 414 (July): 6635–45.
<https://doi.org/10.1007/s00216-022-04224-y>.

</div>

<div id="ref-openms01" class="csl-entry">

Röst, Hannes L., Timo Sachsenberg, Stephan Aiche, Chris Bielow, Hendrik
Weisser, Fabian Aicheler, Sandro Andreotti, et al. 2016. “OpenMS: A
Flexible Open-Source Software Platform for Mass Spectrometry Data
Analysis.” *Nature Methods* 13 (9): 741–48.
<https://doi.org/10.1038/nmeth.3959>.

</div>

<div id="ref-metfrag02" class="csl-entry">

Ruttkies, Christoph, Steffen Neumann, and Stefan Posch. 2019. “Improving
MetFrag with Statistical Learning of Fragment Annotations.” *BMC
Bioinformatics* 20 (1): 1–14.

</div>

<div id="ref-metfrag03" class="csl-entry">

Ruttkies, Christoph, Emma L Schymanski, Nadine Strehmel, Juliane
Hollender, Steffen Neumann, Antony J Williams, and Martin Krauss. 2019.
“Supporting Non-Target Identification by Adding Hydrogen Deuterium
Exchange MS/MS Capabilities to MetFrag.” *Analytical and Bioanalytical
Chemistry* 411: 4683–4700.

</div>

<div id="ref-metfrag01" class="csl-entry">

Ruttkies, Christoph, Emma L Schymanski, Sebastian Wolf, Juliane
Hollender, and Steffen Neumann. 2016. “MetFrag Relaunched: Incorporating
Strategies Beyond in Silico Fragmentation.” *Journal of Cheminformatics*
8 (1): 1–16.

</div>

<div id="ref-xcms01" class="csl-entry">

Smith, C.A., Want, E.J., O’Maille, G., Abagyan,R., Siuzdak, and G. 2006.
“XCMS: Processing Mass Spectrometry Data for Metabolite Profiling Using
Nonlinear Peak Alignment, Matching and Identification.” *Analytical
Chemistry* 78: 779–87.

</div>

<div id="ref-xcms02" class="csl-entry">

Tautenhahn, Ralf, Christoph Boettcher, and Steffen Neumann. 2008.
“Highly Sensitive Feature Detection for High Resolution LC/MS.” *BMC
Bioinformatics* 9: 504.

</div>

<div id="ref-class01" class="csl-entry">

Venables, W. N., and B. D. Ripley. 2002. *Modern Applied Statistics with
s*. Fourth. New York: Springer. <https://www.stats.ox.ac.uk/pub/MASS4/>.

</div>

<div id="ref-mdatools02" class="csl-entry">

Windig, Willem, Neal B. Gallagher, Jeremy M. Shaver, and Barry M. Wise.
2005. “A New Approach for Interactive Self-Modeling Mixture Analysis.”
*Chemometrics and Intelligent Laboratory Systems* 77 (1): 85–96.
https://doi.org/<https://doi.org/10.1016/j.chemolab.2004.06.009>.

</div>

<div id="ref-metfrag04" class="csl-entry">

Wolf, Sebastian, Stephan Schmidt, Matthias Müller-Hannemann, and Steffen
Neumann. 2010. “In Silico Fragmentation for Computer Assisted
Identification of Metabolite Mass Spectra.” *BMC Bioinformatics* 11:
1–12.

</div>

<div id="ref-airpls01" class="csl-entry">

Zhang, Zhi-Min, Shan Chen, and Yi-Zeng Liang. 2010. “Baseline Correction
Using Adaptive Iteratively Reweighted Penalized Least Squares.”
*Analyst* 135: 1138–46. <https://doi.org/10.1039/B922045C>.

</div>

</div>
