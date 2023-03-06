
### Development code ----------------------------------------------------------------------------------------

#updates documentation and installs the package
devtools::document()
devtools::document()
devtools::install(upgrade = "never", dependencies = TRUE, build_vignettes = TRUE)
library(streamFind)

#check the package
devtools::check()

# Add function files
golem::add_fct("setup_project", with_test = TRUE)

# Add utils files
golem::add_utils("helpers", with_test = TRUE)

#load package
devtools::document()
devtools::document()
devtools::load_all() #Ctrl+Shift+L

#show loaded packages
search()

#see todo list
todor::todor()

#show s4 methods
showMethods("a method")

#update the working directory to the package folder
setwd("C:/Users/Ricardo/Documents/CodeProjects/streamFind")

#streamFind system path
system.file(package = "streamFind", dir = "extdata")

#add folder to build_ignore
usethis::use_build_ignore("man-roxygen")






### Code lines strings --------------------------------------------------------------------------------------

#gsub(".*M(.*)\\].*", "\\1", adduct_ion)


# @slot workflows A list of objects inherent of downstream data processing steps, such as
# suspect screening, track of transformations and others.
