##############################################################################################################
# test_create_database.R
library(KAROLA)
context("preper_data_to_ANNA")

arguments <- list(input = "input",
                  output_file = "output_file",
                  dictionary = "dictionary",
                  save_to_file = T)

#####

use_argument <- arguments
use_argument["input"] <- 3

test_that("test dictionary data frame param", {
    expect_error(do.call(preper_data_to_ANNA, use_argument),
                 "param class error")
})

#####

use_argument <- arguments
use_argument["output_file"] <- 3

test_that("test dictionary data frame param", {
    expect_error(do.call(preper_data_to_ANNA, use_argument),
                 "param class error")
})

#####

use_argument <- arguments
use_argument["dictionary"] <- 3

test_that("test dictionary data frame param", {
    expect_error(do.call(preper_data_to_ANNA, use_argument),
                 "param class error")
})

#####

use_argument <- arguments
use_argument["save_to_file"] <- 3

test_that("test dictionary data frame param", {
    expect_error(do.call(preper_data_to_ANNA, use_argument),
                 "param class error")
})
