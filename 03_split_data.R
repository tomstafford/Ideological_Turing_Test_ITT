source("analysis/R/fct_split_data.R")

details <- yaml::read_yaml("analysis/config/split_config.yaml")

purrr::walk(details$topics, split_data)

testthat::test_file("analysis/tests/test_split_data_regression.R")
