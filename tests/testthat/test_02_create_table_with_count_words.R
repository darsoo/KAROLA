##############################################################################################################
# test_create_table_with_count_words.R

library(KAROLA)
context("create_table_with_count_words")

jsonFile <- file.path(system.file(package="KAROLA"),"extdata","test.json")

create_test_data_base(json = jsonFile)

#####
arguments <- list(word = "cancer_NN",
                  lemma_table = "test_table",
                  json = jsonFile)

do.call(create_database, arguments)
#####

arguments <- list(lemma_table = "test_table",
                  json = jsonFile,
                  first_analyzed_date = "2016-01-01",
                  last_analyzed_date = "2016-12-01")


function_output <- do.call(create_table_with_count_words, arguments)

test_that("background info", {
    expect_equal(function_output[2,1], "0")
    expect_equal(function_output[2,2], 175)
    expect_equal(nrow(function_output), 2057)
    expect_equal(ncol(function_output), 2)
})
#####
use_argument <- arguments
use_argument["first_analyzed_date"] <- "2015-12-01"
use_argument["last_analyzed_date"] <- "2016-02-01"
function_output <- ""
function_output <- do.call(create_table_with_count_words, use_argument)

test_that("test for fix bug - 1", {
    expect_is(function_output, "data.frame")
    expect_equal(nrow(function_output), 915)
    expect_equal(ncol(function_output), 3)
})
#####
use_argument <- arguments
use_argument["first_analyzed_date"] <- "2015-10-01"
use_argument["last_analyzed_date"] <- "2016-01-01"
function_output <- ""
function_output <- do.call(create_table_with_count_words, use_argument)

test_that("test for fix bug - 2", {
    expect_is(function_output, "data.frame")
    expect_equal(nrow(function_output), 1043)
    expect_equal(ncol(function_output), 3)
})
#####

# use_argument <- arguments
# use_argument["json"] <- "test.json-wou/wop"
# use_argument["first_analyzed_date"] <- "2015-12-01"
# use_argument["last_analyzed_date"] <- "2016-02-01"
# function_output <- ""
# function_output <- do.call(create_table_with_count_words, use_argument)
#
# test_that("test for fix bug - 1", {
#     expect_is(function_output, "data.frame")
# })
