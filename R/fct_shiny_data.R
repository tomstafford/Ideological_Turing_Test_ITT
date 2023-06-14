source("analysis/R/utils.R")

prep_shiny_data <- function(label_in,
                           indir = "analysis/data/split_data/",
                           outdir = "analysis/data/shiny_data/") {
  
  files <- list.files(indir, label_in, full.names = TRUE)
  
  full_data <- purrr::list_rbind(
    purrr::map(files, read_csv, col_types = get_colspec())
  )
  
  wrangled_data <- wrangle_split_data(full_data)
  
  outpath <- gen_file_path(outdir, label_in, "_shiny.csv")
  
  write_csv(wrangled_data, outpath)
}

wrangle_split_data <- function(data_in) {
  
  cleaned_headers <- rename_with(data_in, stringr::str_to_lower)
  
  recoded_factors <- mutate(cleaned_headers,
                            arguer_position = forcats::fct_recode(
                              arguer_position,
                              Pro = "PRO",
                              Anti = "ANTI"))
  
  return(recoded_factors)
}


get_colspec <- function() {
  
  column_spec <- cols_only(
    Arguments = "c",
    argument_Position = "f",
    Arguer_Position = "f",
    rater_position = "f",
    response_ratings = "i",
    argument_ID = "i",
    argument_index = "i",
    condition = "f"
  )
  
  return(column_spec)
}
