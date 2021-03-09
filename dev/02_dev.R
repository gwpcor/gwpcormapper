# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package("shinydashboard")
usethis::use_package("sf")
usethis::use_package("dplyr")
usethis::use_package("geodist")
usethis::use_package("plotly")
usethis::use_package("crosstalk")
usethis::use_package("viridis")
usethis::use_package("leaflet")
usethis::use_package("tools")
usethis::use_package("shinyjs")
usethis::use_package("Rcpp")
usethis::use_package("corpcor")

## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct( "gwpcor" )
golem::add_utils( "server" )
golem::add_utils( "ui" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "tokyo2005", open = FALSE )

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("getting_started")
devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
usethis::use_github()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
