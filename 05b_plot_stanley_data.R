source("R/fct_plot_stanley_data.R")

details <- yaml::read_yaml("config/plot_stanley_config.yaml")

purrr::walk(details$model_names, generate_stanley_plots, details$plot_config)
