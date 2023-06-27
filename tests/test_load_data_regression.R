library(testthat)
library(readr)
library(dplyr)

new_dir <- "../data/loaded_data/"
orig_dir <- "regression_datasets/"

import_cols <- function() {
  
  c(all_of(
    c("UTC.Timestamp", "Participant.Private.ID", "ID", "arg_ID",
      "Arg_Position", "rater_position", "response_ratings")))
  
}


test_that("Regression test for vegan_ratings DF", {
  
  filename <- "vegan_ratings.csv"
  
  orig_data <- read_csv(file.path(orig_dir, filename),
                        col_select = import_cols())
  new_data <- read_csv(file.path(new_dir, filename), 
                       col_select = import_cols())
  
  result <- all_equal(new_data, orig_data)
  expect_true(result)
})


test_that("Regression test for nonVegan_ratings DF", {
  
  filename <- "nonVegan_ratings.csv"
  
  orig_data <- read_csv(file.path(orig_dir, filename),
                        col_select = import_cols())
  new_data <- read_csv(file.path(new_dir, filename), 
                       col_select = import_cols())
  
  result <- all_equal(new_data, orig_data)
  
  expect_true(result)
})


test_that("Regression test for leave_ratings DF", {
  
  filename <- "leave_ratings.csv"

  orig_data <- read_csv(file.path(orig_dir, filename),
                        col_select = import_cols())
  new_data <- read_csv(file.path(new_dir, filename), 
                       col_select = import_cols())
  
  result <- all_equal(new_data, orig_data)
  
  expect_true(result)
})

test_that("Regression test for remain_ratings DF", {
  
  filename <- "remain_ratings.csv"
  
  orig_data <- read_csv(file.path(orig_dir, filename),
                        col_select = import_cols())
  new_data <- read_csv(file.path(new_dir, filename), 
                       col_select = import_cols())
  
  result <- all_equal(new_data, orig_data)
  
  expect_true(result)
})

test_that("Regression test for pro_ratings DF", {
  
  filename <- "pro_ratings.csv"
  
  orig_data <- read_csv(file.path(orig_dir, filename),
                        col_select = import_cols())
  new_data <- read_csv(file.path(new_dir, filename), 
                       col_select = import_cols())
  
  result <- all_equal(new_data, orig_data)
  
  expect_true(result)
})

test_that("Regression test for hes_ratings DF", {
  
  filename <- "hes_ratings.csv"
  
  orig_data <- read_csv(file.path(orig_dir, filename),
                        col_select = import_cols())
  new_data <- read_csv(file.path(new_dir, filename), 
                       col_select = import_cols())
  
  result <- all_equal(new_data, orig_data)
  
  expect_true(result)
})
