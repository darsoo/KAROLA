##############################################################################################################
# test_create_database.R
library(KAROLA)
context("create_database")

client <- docker::docker$from_env()
client$containers$run(image = "karola-test-database",
                      #name = "k-test-db",
                      ports=list('5432'='5430'),
                      detach = T)

#drop_test_data_base(json = "test.json")
create_test_data_base(json = "test.json")

#####
arguments <- list(
    word = "cancer_NN",
    lemma_table = "test_new_table_1",
    json = "test.json"
)
test_that("make table with good word",{
    expect_null(do.call(create_database, arguments))
})
#####
arguments <- list(
    word = "wrong_word",
    lemma_table = "test_new_table_2",
    json = "test.json"
)
test_that("make table with wrong word",{
    expect_error(do.call(create_database, arguments),paste0('given word: "' ,arguments$word ,'" does not exist in database'))
})
#####
drop_test_data_base(json = "test.json")
##############################################################################################################
# test_create_table_with_count_words.R

library(KAROLA)
context("create_table_with_count_words")

create_test_data_base(json = "test.json")

#####
arguments <- list(word = "cancer_NN",
                  lemma_table = "test_table",
                  json = "test.json")

do.call(create_database, arguments)
#####

arguments <- list(lemma_table = "test_table",
                  json = "test.json",
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

##############################################################################################################
# test_download_dictionary.R

dictionary <- download_dictionary(json = "test.json")

test_that("test dictionary data frame param", {
    expect_is(dictionary, "data.frame")
    expect_equal(nrow(dictionary), 10307)
    expect_equal(ncol(dictionary), 2)
    expect_equal(dictionary[2,1], 11817)
})

##############################################################################################################

drop_test_data_base(json = "test.json")

##############################################################################################################
