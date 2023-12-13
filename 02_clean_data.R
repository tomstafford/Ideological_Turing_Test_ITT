source("R/fct_clean_data.R")

argument_details <- yaml::read_yaml("config/cleaning_config.yaml")

purrr::walk(argument_details, load_dataset)

# Regression tests to compare with output of old scripts
# Not needed for analysis
#testthat::test_file("tests/test_clean_data_regression.R")
