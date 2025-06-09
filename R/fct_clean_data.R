source("R/utils.R")

stanley_regex <- c("^stan.*$",
                   "^int.*_1$", "^int.*_2$", "^int.*_3$",
                   "^mor.*_1$", "^mor.*_2$", "^mor.*_3$")

stanley_newname <- c("reasoning_stan",
                     "intellectual_stan_1",
                     "intellectual_stan_2",
                     "intellectual_stan_3",
                     "morality_stan_1",
                     "morality_stan_2",
                     "morality_stan_3")

# Load data
load_dataset <- function(details, output_dir = "data/cleaned_data/") {
  
  # Get details from list
  topic_name <- details$topic
  input_details <- details$filedetails
  clean_stanley <- details$stanley
  
  # Load and combine the two datasets
  full_data <- purrr::map_df(input_details, load_datafile)
  
  # Set Arguer_Position as a factor to allow it to be ordered
  full_data <- mutate(full_data,
                       Arguer_Position = factor(Arguer_Position,
                                                levels = c("PRO", "ANTI")))
  # Add an ID/index column for argument
  full_data <- add_id_col(full_data, argument_index,
                          c("Arguer_Position", "ARGUER_ID", "argument_ID"))
  
  # Add an ID/index column for rater
  full_data <- add_id_col(full_data, Rater_ID,
                          c("Arguer_Position", "Participant.Private.ID"))
  
  # Re-code the positions based on topic as they were originally coded
  # inconsistently
  full_data <- switch (topic_name,
    "brexit" = recode_brexit(full_data),
    "vaccine" = recode_vaccine(full_data),
    "veganism" = recode_vegan(full_data)
  )
  
  full_data <- mutate(full_data, arguer_id_full = sprintf('%03i_%s', ARGUER_ID, Arguer_Position))
  
  if (clean_stanley){
    for (idx in seq_along(stanley_regex)){
      full_data <- process_stanley(stanley_regex[idx],
                                   stanley_newname[idx],
                                   full_data)
    }
  }
  
  output_path = gen_file_path(output_dir, topic_name, "_ratings.csv")
  
  if (!dir.exists(output_dir)) dir.create(output_dir)
  
  write_csv(full_data, output_path)
  
}

load_datafile <- function(details, filepath = "data/loaded_data/") {
  
  # Extract details
  filename <- details$filename
  arguer_pos <- details$arguer_pos
  argument_pos <- details$argument_pos
  
  fullpath <- gen_file_path(filepath, filename, ".csv")
  
  # Load data
  # (set age as char because there is some odd data that causes problems)
  loaded_data <- read_csv(fullpath, col_types = cols(age = col_character()))
  
  # Update variable names
  loaded_data <- rename(loaded_data, ARGUER_ID = ID, argument_ID = arg_ID)
  # Add arguer position
  loaded_data <- mutate(loaded_data, Arguer_Position = arguer_pos)
  
  # Get largest value for argument ID for adjustment purposes (see below)
  adjustment_quant <- max(loaded_data$argument_ID)
  
  # Shift the IDs of specified argument positions so they run contiguously
  # rather than repeating
  # (e.g. Originally argument_ID for one arguer would be 123123, this alters it
  # to be 123456)
  loaded_data <- mutate(loaded_data,
                        argument_ID = ifelse(
                          Arg_Position == argument_pos,
                          argument_ID + adjustment_quant,
                          argument_ID))
}

# Add other ID variables 
add_id_col <- function(data_in, col_name, grouping_vars){
  
  data_grouped <- group_by(data_in, across(all_of(grouping_vars)))
  
  # generate IDs for each group
  data_out <- mutate(data_grouped, {{ col_name }} := cur_group_id())
}


recode_vegan <- function(data_in){

  mutate(data_in, 
         argument_Position = case_when(
           Arg_Position == "For" ~ "For",
           Arg_Position == "Against" ~ "Against"),
         rater_position = case_when(
           rater_position == "VEGAN" ~ "Pro",
           rater_position == "Vegan" ~ "Pro",
           rater_position == "Non_vegan" ~ "Anti",
           rater_position == "NONvegan" ~ "Anti"
         ))
}

recode_brexit  <- function(data_in) {
  
  mutate(data_in, 
         argument_Position = case_when(
           Arg_Position == "Leave" ~ "For",
           Arg_Position == "Remain" ~ "Against"),
         rater_position = case_when(
           rater_position == "LEAVE" ~ "Pro",
           rater_position == "REMAIN" ~ "Anti",
   ))
}

recode_vaccine <- function(data_in) {
  
  mutate(data_in, 
         argument_Position = case_when(
           Arg_Position == "For" ~ "For",
           Arg_Position == "Against" ~ "Against"),
         rater_position = case_when(
           rater_position == "Pro" ~ "Pro",
           rater_position == "Hesitant" ~ "Anti",
         ))
}

coacross <- function(...) {
  coalesce(!!!across(...))
}

process_stanley <- function(regex, new_name, data) {
    
  data_out <- mutate(data, !!new_name := coacross(matches(regex)), .keep = "unused")
  
  return(data_out)
}


