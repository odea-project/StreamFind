
# R pkgs for streamFind development
# note: apt pkgs should be run beforehand, see notes_R_install

### CRAN pkgs -----
install.packages(c(
  "bookdown",
  "kableExtra",
  "plotly",
  "cli", #needed for tests
  "shiny",
  "golem",
  "RaMS",
  "rJava",
  "igraph" #needed for annotation
))
# rJava needed for patRoon


### bioconductor pkgs -----
install.packages(c("BiocManager", "remotes"))
BiocManager::install(c(
  "mzR", "xcms", "CAMERA", "ropls",
  "InterpretMSSpectrum", "BiocStyle", "Rgraphviz"
))


### github pkgs -----
remotes::install_github("blosloos/enviPick")
remotes::install_github("rickhelmus/KPIC2")
remotes::install_github("cbroeckl/RAMClustR")
remotes::install_github("rickhelmus/cliqueMS")
remotes::install_github("KelseyChetnik/MetaClean")
remotes::install_github("blosloos/nontargetData")
remotes::install_github("blosloos/nontarget")

remotes::install_github("rickhelmus/patRoon",
  upgrade = "never", dependencies = TRUE
)

install.packages("plumber")

## Install local streamFind  ##
#updates documentation and installs the package
devtools::document()
devtools::document()
devtools::install(upgrade = "never")

print("streamFind package installed:")
print(require(streamFind))

