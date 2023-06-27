source("R/utils.R")

split_data <- function(topic) {
  
  cleaned_data <- load_cleaned_data(topic)
  
  added_conditions <- assign_conditions(cleaned_data)
  
  purrr::walk(c("Baseline", "ITT"), filter_and_save,
              data_in = added_conditions, topic = topic)
  
}

load_cleaned_data <- function(topic,
                              filepath = "data/cleaned_data/") {
  
  fullpath <- gen_file_path(filepath, topic, "_ratings.csv")
  
  data_out <- read_csv(fullpath, show_col_types = FALSE)
}

assign_conditions <- function(data_in) {
  
  mutate(data_in, condition = case_when(
    Arguer_Position == "PRO" & argument_Position == "For" & rater_position == "Pro" ~ "Baseline",
    Arguer_Position == "ANTI" & argument_Position == "Against" & rater_position == "Anti" ~ "Baseline",
    Arguer_Position == "PRO" & argument_Position == "Against" & rater_position == "Anti"  ~ "ITT",
    Arguer_Position == "ANTI" & argument_Position == "For" & rater_position == "Pro" ~ "ITT",
    TRUE ~ NA_character_
  ))
  
}

filter_and_save <- function(filter_cond, data_in, topic,
                            output_dir = "data/split_data") {
  
  filtered_data <- filter(data_in, condition == filter_cond)
  
  add_ITT_col <- mutate(filtered_data,
                        ITT_cond = if_else(condition == "ITT", 1, 0))
  
  add_passed_col <- mutate(add_ITT_col,
                           passedITT = case_when(
                             ITT_cond == 0 ~ NA_real_,
                             response_ratings > 5 ~ 1,
                             response_ratings <= 5 ~ 0
                           ))
  
  outpath <- gen_file_path(output_dir,
                           paste(topic, filter_cond, sep = "_"),
                           "DataFrame.csv")
  
  if (!dir.exists(output_dir)) dir.create(output_dir)
  
  write_csv(add_passed_col, outpath)
  
}
