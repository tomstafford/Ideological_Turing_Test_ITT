source("R/fct_split_data.R")

details <- yaml::read_yaml("config/split_config.yaml")

purrr::walk(details$topics, split_data)

# Regression tests to compare with output of old scripts
# Not needed for analysis
#testthat::test_file("tests/test_split_data_regression.R")
