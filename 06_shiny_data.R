source("analysis/R/fct_shiny_data.R")

details <- yaml::read_yaml("analysis/config/split_config.yaml")

purrr::walk(details$topics, prep_shiny_data)
