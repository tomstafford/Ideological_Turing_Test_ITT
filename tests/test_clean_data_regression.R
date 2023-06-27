library(testthat)
library(readr)
library(dplyr)

new_dir <- "../data/cleaned_data/"
orig_dir <- "regression_datasets/"

import_cols <- function() {
  
  c(all_of(
    c("UTC.Timestamp", "Participant.Private.ID", "ARGUER_ID", "argument_ID",
      "argument_Position", "rater_position", "response_ratings", "Arguer_Position")))
  
}

test_that("Regression test for veganism_ratings DF", {
   
  filename <- "veganism_ratings.csv"
  
  orig_data <- read_csv(file.path(orig_dir, filename))
  new_data <- read_csv(file.path(new_dir, filename))
  
  expect_true(
    all_equal(orig_data[ , c("Rater_ID", "Arguer_Position")],
              new_data[ , c("Rater_ID", "Arguer_Position")])
  )
  
  expect_true(
    all_equal(orig_data[ , c("argument_index", "Arguer_Position")],
              new_data[ , c("argument_index", "Arguer_Position")])
  )
  
  orig_data <- select(orig_data, import_cols())
  new_data <- select(new_data, import_cols())
  
  expect_true(
    all_equal(orig_data,
              new_data,
              convert = T)
  )
})

test_that("Regression test for brexit_ratings DF", {
  
  filename <- "brexit_ratings.csv"
  
  orig_data <- read_csv(file.path(orig_dir, filename))
  new_data <- read_csv(file.path(new_dir, filename))
  
  expect_true(
    all_equal(orig_data[ , c("Rater_ID", "Arguer_Position")],
              new_data[ , c("Rater_ID", "Arguer_Position")])
  )
  
  expect_true(
    all_equal(orig_data[ , c("argument_index", "Arguer_Position")],
              new_data[ , c("argument_index", "Arguer_Position")])
  )
  
  orig_data <- select(orig_data, import_cols())
  new_data <- select(new_data, import_cols())
  
  expect_true(
    all_equal(orig_data,
              new_data,
              convert = T)
  )
})

test_that("Regression test for vaccine_ratings DF", {

  filename <- "vaccine_ratings.csv"

  orig_data <- read_csv(file.path(orig_dir, filename))
  new_data <- read_csv(file.path(new_dir, filename))
  
  expect_true(
    all_equal(orig_data[ , c("Rater_ID", "Arguer_Position")],
              new_data[ , c("Rater_ID", "Arguer_Position")])
  )
  
  expect_true(
    all_equal(orig_data[ , c("argument_index", "Arguer_Position")],
              new_data[ , c("argument_index", "Arguer_Position")])
  )
  
  orig_data <- select(orig_data, import_cols())
  new_data <- select(new_data, import_cols())
  
  expect_true(
    all_equal(orig_data,
              new_data,
              convert = T)
  )
})

test_that("Regression test for stanley transformations", {
  filename <- "brexit_ratings.csv"
  
  orig_data <- read_csv(file.path(orig_dir, filename))
  new_data <- read_csv(file.path(new_dir, filename))
  
  orig_data <- mutate(orig_data, reasoning_stan = coalesce(stanely_L,
                                                           stanley_R))
  expect_true(
    all_equal(orig_data[ ,"reasoning_stan"],
              new_data[ , "reasoning_stan"])
  )
  
  orig_data <- mutate(orig_data, intellectual_stan_1 = coalesce(intellectual_L_1,
                                                              intellectual_R_1))
  expect_true(
    all_equal(orig_data[ ,"intellectual_stan_1"],
              new_data[ , "intellectual_stan_1"])
  )
  
  orig_data <- mutate(orig_data, morality_stan_1 = coalesce(morality_L_1,
                                                          morality_R_1))
  expect_true(
    all_equal(orig_data[ ,"morality_stan_1"],
              new_data[ , "morality_stan_1"])
  )
})