
# Development ------------------------------------------------------------------

# Updates documentation with roxygen
devtools::document()
devtools::document()

# Installs the package
devtools::install(upgrade = "never", dependencies = TRUE, build_vignettes = FALSE)

# Builds/Updates the GitHub page
pkgdown::build_site()

# Check the package for conformity
devtools::check()

# Loads all dependencies
devtools::load_all() #Ctrl+Shift+L

# Lists TODOs
todor::todor()


#chattr::chattr_use("copilot")
chattr::chattr_app()

zlibbioc::pkgconfig()

# Code lines -------------------------------------------------------------------

# \U1f5ab floppy disk (clear background)
# \U1f5aa floppy disk
# \U23f1 stopwatch for time tasks
# \U231b ample for time tasks
# \U2713 check mark for modified by reference
# \U1D48A italic i for info messages
# \U2139 i for information messages
# \U24d8 i from information messages with round circle
# \U2713 check for ready
# \U1f5f9 check in a box



#gsub(".*M(.*)\\].*", "\\1", adduct_ion)

file_path <- "R/fct_ProcessingSettings.R"
file_content <- readLines(file_path, encoding = "UTF-8")
non_ascii_lines <- grep("[^\x01-\x7F]", file_content, value = TRUE)
cat("Non-ASCII characters found in the following lines:\n")
print(non_ascii_lines)

# Others -----------------------------------------------------------------------

system.file(package = "StreamFind", dir = "extdata")

usethis::use_build_ignore("man-roxygen")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "my_dataset", open = FALSE)

## Tests ----
## Add one line by test you want to create
usethis::use_test("app")

# Documentation

## Vignette ----
usethis::use_vignette("StreamFind")
devtools::build_vignettes()

## Code Coverage ----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release()
usethis::use_github_action_check_standard()
usethis::use_github_action_check_full()
# Add action for PR
usethis::use_github_action_pr_commands()

# Travis CI
usethis::use_travis()
usethis::use_travis_badge()

# AppVeyor
usethis::use_appveyor()
usethis::use_appveyor_badge()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()
