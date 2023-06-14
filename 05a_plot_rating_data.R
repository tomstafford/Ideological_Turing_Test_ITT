source("analysis/R/fct_plot_rating_data.R")

details <- yaml::read_yaml("analysis/config/plot_rating_config.yaml")

purrr::walk(details$topics, plot_data, details$plot_config)

