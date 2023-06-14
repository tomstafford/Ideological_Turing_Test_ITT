source("analysis/R/fct_clean_data.R")

argument_details <- yaml::read_yaml("analysis/config/cleaning_config.yaml")

purrr::walk(argument_details, load_dataset)

testthat::test_file("analysis/tests/test_clean_data_regression.R")
