source("R/utils.R")

load_data <- function(topic, in_dir = "data/split_data/") {
  
  filename <- paste(topic, "ITTDataFrame.csv", sep = "_")
  
  filepath <- file.path(in_dir, filename)
  readr::read_csv(filepath, show_col_types = FALSE)
}

lickert_to_numeric <- function(data_in, col_name_contains = "stan") {
  mutate(data_in, across(.cols = contains(col_name_contains), .fns = recode_lickert))
}


wide_to_long <- function(data_in, column_name, names_to, values_to){
  
  pivot_longer(data_in, starts_with(column_name), names_to = names_to, values_to = values_to)

}
