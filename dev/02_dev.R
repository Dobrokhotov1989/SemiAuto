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
usethis::use_package( "shinydashboard" )
usethis::use_package( "shinydashboardPlus" )
usethis::use_package( "shinyWidgets" )
usethis::use_package( "shinyBS" )
usethis::use_package( "shinyjs" )
usethis::use_package( "waiter" )
usethis::use_package( "europepmc" )
usethis::use_package( "ggplot2" )
usethis::use_package( "grid" )
usethis::use_package( "dplyr" )
usethis::use_package( "stringr" )
usethis::use_package( "xml2" )
usethis::use_package( "purrr" )
usethis::use_package( "rlang" )
usethis::use_package( "glue" )
usethis::use_package( "zip" )


## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "header" ) # Name of the module
golem::add_module( name = "sidebar" ) # Name of the module
golem::add_module( name = "footer" ) # Name of the module
golem::add_module( name = "body" ) # Name of the module

golem::add_module( name = "home_tab" ) # Name of the module
golem::add_module( name = "text_sieve_tab" ) # Name of the module
golem::add_module( name = "abbrevimate_tab" ) # Name of the module
golem::add_module( name = "about_tab" ) # Name of the module

golem::add_module( name = "home_motivation_box" ) # Name of the module
golem::add_module( name = "home_reading_time_box" ) # Name of the module

golem::add_module( name = "abbrevimate_settings_box" ) # Name of the module
golem::add_module( name = "abbrevimate_pos_hits_box" ) # Name of the module
golem::add_module( name = "abbrevimate_neg_hits_box" ) # Name of the module
golem::add_module( name = "abbrevimate_hits_box" ) # Name of the module
golem::add_module( name = "abbrevimate_dictionary_box" ) # Name of the module


golem::add_module( name = "text_sieve_settings_box" ) # Name of the module
golem::add_module( name = "text_sieve_results_box" ) # Name of the module

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct( "abbr_epmc_search" ) 
golem::add_fct( "abbr_term_to_pattern" ) 
golem::add_fct( "abbr_extract_pattern_from_paper" ) 
golem::add_fct( "abbr_abbreviation_to_pattern" ) 
golem::add_fct( "abbr_split_term_and_abbr" ) 

golem::add_fct( "abbr_find_coappearance_epmc" ) 
golem::add_fct( "abbr_dictionaries_to_patterns" ) 

golem::add_utils( "helpers" )


## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "pubs_trend_all_years", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("Semi_auto")
devtools::build_vignettes()

## Code Coverage----
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

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

