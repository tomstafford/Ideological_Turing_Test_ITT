source("R/fct_load_data.R")

argument_details <- yaml::read_yaml("config/loading_config.yaml")

purrr::walk(argument_details, process_all_files)

# Regression tests to compare with output of old scripts
# Not needed for analysis
#testthat::test_file("tests/test_load_data_regression.R")


