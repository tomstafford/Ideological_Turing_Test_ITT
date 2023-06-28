source("R/utils.R")
library(stringr)

load_data <- function(topic, in_dir = "data/split_data/") {
  
  filename <- paste(topic, "ITTDataFrame.csv", sep = "_")
  
  filepath <- file.path(in_dir, filename)
  readr::read_csv(filepath, show_col_types = FALSE)
}

lickert_to_numeric <- function(data_in, col_name_contains = "stan") {
  mutate(data_in, across(.cols = contains(col_name_contains), .fns = recode_lickert))
}

dichot_read_discuss <- function(data_in) {
  mutate(data_in,
         discuss = if_else(str_detect(tolower(discuss_1), "most"), 1, 0),
         read = if_else(str_detect(tolower(read_1), "most"), 1, 0))
}

wide_to_long <- function(data_in, column_name, names_to, values_to){
  
  pivot_longer(data_in, starts_with(column_name), names_to = names_to, values_to = values_to)

}
