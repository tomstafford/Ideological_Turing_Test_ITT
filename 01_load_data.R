source("analysis/R/fct_load_data.R")

argument_details <- yaml::read_yaml("analysis/config/loading_config.yaml")

purrr::walk(argument_details, process_all_files)

testthat::test_file("analysis/tests/test_load_data_regression.R")

