library(testthat)
library(readr)
library(dplyr)

new_dir <- "../data/split_data/"
orig_dir <- "regression_datasets/"

import_cols <- function() {
  
  c(all_of(
    c("UTC.Timestamp", "Participant.Private.ID", "ARGUER_ID", "argument_ID",
      "argument_Position", "rater_position", "response_ratings",
      "Arguer_Position", "ITT_cond"
      )))
  
}

test_that("Regression test for veganism_ITTDataFrame", {
   
  filename <- "veganism_ITTDataFrame.csv"
  
  orig_data <- read_csv(file.path(orig_dir, filename))
  new_data <- read_csv(file.path(new_dir, filename))
  
  orig_data <- select(orig_data, import_cols())
  new_data <- select(new_data, import_cols())
  
  expect_true(
    all_equal(orig_data,
              new_data,
              convert = T)
  )
})

test_that("Regression test for veganism_BaselineDataFrame", {
  
  filename <- "veganism_BaselineDataFrame.csv"
  
  orig_data <- read_csv(file.path(orig_dir, filename))
  new_data <- read_csv(file.path(new_dir, filename))
  
  orig_data <- select(orig_data, import_cols())
  new_data <- select(new_data, import_cols())
  
  expect_true(
    all_equal(orig_data,
              new_data,
              convert = T)
  )
})

test_that("Regression test for brexit_ITTDataFrame", {
  
  filename <- "brexit_ITTDataFrame.csv"
  
  orig_data <- read_csv(file.path(orig_dir, filename))
  new_data <- read_csv(file.path(new_dir, filename))
  
  orig_data <- select(orig_data, import_cols())
  new_data <- select(new_data, import_cols())
  
  expect_true(
    all_equal(orig_data,
              new_data,
              convert = T)
  )
})

test_that("Regression test for brexit_BaselineDataFrame", {
  
  filename <- "brexit_BaselineDataFrame.csv"
  
  orig_data <- read_csv(file.path(orig_dir, filename))
  new_data <- read_csv(file.path(new_dir, filename))

  orig_data <- select(orig_data, import_cols())
  new_data <- select(new_data, import_cols())
  
  expect_true(
    all_equal(orig_data,
              new_data,
              convert = T)
  )
})

test_that("Regression test for vaccine_ITTDataFrame", {

  filename <- "vaccine_ITTDataFrame.csv"

  orig_data <- read_csv(file.path(orig_dir, filename))
  new_data <- read_csv(file.path(new_dir, filename))
  
  orig_data <- select(orig_data, import_cols())
  new_data <- select(new_data, import_cols())
  
  expect_true(
    all_equal(orig_data,
              new_data,
              convert = T)
  )
})

test_that("Regression test for vaccine_BaselineDataFrame", {
  
  filename <- "vaccine_BaselineDataFrame.csv"
  
  orig_data <- read_csv(file.path(orig_dir, filename))
  new_data <- read_csv(file.path(new_dir, filename))
  
  orig_data <- select(orig_data, import_cols())
  new_data <- select(new_data, import_cols())
  
  expect_true(
    all_equal(orig_data,
              new_data,
              convert = T)
  )
})