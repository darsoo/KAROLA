##############################################################################################################
# test_create_table_with_count_words.R

library(KAROLA)
context("create_table_with_count_words")

json_file <- file.path(system.file(package = "KAROLA"), "extdata", "test.json")

#####

arguments <- list(word = "cancer_NN",
                  lemma_table = "test_table",
                  json = json_file)

try(do.call(create_database, arguments), silent = T)

#####

use_argument <- arguments
use_argument["word"] <- 3

test_that("test param class error - create_table_with_count_words(word)", {
    expect_error(do.call(create_database, use_argument), "param class error")
})

#####

use_argument <- arguments
use_argument["lemma_table"] <- 3

test_that("test param class error - create_table_with_c...(lemma_table)", {
    expect_error(do.call(create_database, use_argument), "param class error")
})

#####

arguments <- list(lemma_table = "test_table",
                  json = json_file,
                  first_analyzed_date = "2016-01-01",
                  last_analyzed_date = "2016-12-01")

test_that("background info", {
    check_docker()
    function_output <- do.call(create_table_with_count_words, arguments)
    expect_equal(function_output[2, 1], "0")
    expect_equal(function_output[2, 2], 175)
    expect_equal(nrow(function_output), 2057)
    expect_equal(ncol(function_output), 2)
})
#####
use_argument <- arguments
use_argument["first_analyzed_date"] <- "2015-12-01"
use_argument["last_analyzed_date"] <- "2016-02-01"
function_output <- ""

test_that("test for fix bug - 1", {
    check_docker()
    function_output <- do.call(create_table_with_count_words, use_argument)
    expect_is(function_output, "data.frame")
    expect_equal(nrow(function_output), 915)
    expect_equal(ncol(function_output), 3)
})
#####
use_argument <- arguments
use_argument["first_analyzed_date"] <- "2015-10-01"
use_argument["last_analyzed_date"] <- "2016-01-01"
function_output <- ""

test_that("test for fix bug - 2", {
    check_docker()
    function_output <- do.call(create_table_with_count_words, use_argument)
    expect_is(function_output, "data.frame")
    expect_equal(nrow(function_output), 1043)
    expect_equal(ncol(function_output), 3)
})
#####
