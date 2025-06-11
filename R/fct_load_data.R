library(stringr)
source("R/utils.R")

process_all_files <- function(details,
                              input_dir = "data/raw",
                              output_dir = "data/loaded_data"){

  # Separate out details  
  data_paths <- file.path(input_dir, details$path)
  raters <- details$raters
  arguer <- details$arguer
  
  # Generate output location
  output_path <- gen_file_path(output_dir, arguer, "_ratings.csv")
  
  # Run through each rater & input path to compile data
  argument_ratings <- purrr::map2_df(data_paths, raters, load_files_by_rater)
  
  # Drop rows that aren't responses to arguments (e.g. checks)
  argument_ratings <- keep_argument_responses(argument_ratings)
  
  # Re-code lickert responses and arg_ID to numerical
  argument_ratings <- mutate(argument_ratings,
                             response_ratings = recode_lickert(Response),
                             arg_ID = as.numeric(arg_ID))
  
  if (any(stringr::str_detect(colnames(argument_ratings), "_conf"))) {
    argument_ratings <- rename(argument_ratings, position_conf = ends_with("_conf"))
  } else {
    argument_ratings <- mutate(argument_ratings, position_conf = NA_character_)
  }
  
  argument_ratings <- mutate(argument_ratings,
                             position_conf_num = recode_conf_lickert(position_conf))
  
  # Create output_dir if it doesn't already exist
  if (!dir.exists(output_dir)) dir.create(output_dir)
  
  # Write loaded data
  write_csv(argument_ratings, output_path)
}


load_files_by_rater <- function(data_path, rater_pos, ext = "*.csv") {
  # Get all the csv files in the data path
  files <- list.files(data_path, ext, full.names = TRUE)
  
  # Use load file to load and join
  full_data <- purrr::map_df(files, load_file, rater_pos = rater_pos)
  
  return(full_data)
}

# Load individual file, select only specific cols 
load_file <- function(file_path, rater_pos){
  
  data <- read_csv(file_path,
                   col_types = col_types(),
                   col_select = col_selection(),
                   show_col_types = FALSE,
                   name_repair = make.names)

  # Add rater position
  data$rater_position <- rater_pos
  
  return(data)
}

# Remove rows which aren't related to responses to arguments
keep_argument_responses <- function(data) {
  data <- filter(data,
                 display == "statements",
                 str_detect(arg_ID, "check*", negate = T))
  
  return(data)
}

# Import only specific columns
col_selection <- function() {
  
  keep_cols <- c(
    all_of(c("Event.Index", "UTC.Timestamp", "UTC.Date", "Participant.Private.ID",
           "Task.Version", "Trial.Number", "Response", "display", "ID",
           "discuss_1", "read_1", "age", "gender", "edu", "arg_ID",
           "Arg_Position", "Arguments")),
    starts_with("stan"),
    starts_with("intellectual"),
    starts_with("morality"),
    ends_with("_conf"))
  
  return(keep_cols)
  
}
