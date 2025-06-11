library(readr)
library(dplyr)

recode_lickert <- function(response) {
  case_when(response == "Strongly Agree" ~ 7,
            response == "Strongly agree" ~ 7,
            response == "Agree" ~ 6,
            response == "Somewhat Agree" ~ 5,
            response == "Neither Agree nor Disagree" ~ 4,
            response == "Somewhat Disagree" ~ 3,
            response == "Disagree" ~ 2,
            response == "Strongly Disagree" ~ 1)
}

recode_conf_lickert <- function(conf_response) {
  response <- stringr::str_to_lower(conf_response)
  
  case_when(response == "very confident" ~ 7,
            response == "confident" ~ 6,
            response == "somewhat confident" ~ 5,
            response == "undecided" ~ 4,
            response == "somewhat doubtful" ~ 3,
            response == "doubtful" ~ 2,
            response == "very doubtful" ~ 1)
}


# Specify types for specific cols to constrain importing
col_types <- function() {
  cols(age = col_character(),
       arg_ID = col_character())
}

gen_file_path <- function(path, name, suffix) {
  file.path(path, paste0(name, suffix))
}
