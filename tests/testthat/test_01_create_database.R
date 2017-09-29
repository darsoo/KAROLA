##############################################################################################################
# test_create_database.R
library(KAROLA)
context("create_database")

json_file <- file.path(system.file(package = "KAROLA"), "extdata", "test.json")

try(create_test_data_base(json = json_file), silent = T)


arguments <- list(word = "cancer_NN",
                  lemma_table = "new_table",
                  json = json_file)
#####

use_argument <- arguments
use_argument["word"] <- 3

test_that("test param class error - create_database(word)", {
    expect_error(do.call(create_database, use_argument), "param class error")
})

#####

use_argument <- arguments
use_argument["lemma_table"] <- 3

test_that("test param class error - create_database(lemma_table)", {
    expect_error(do.call(create_database, use_argument), "param class error")
})

#####
arguments <- list(
    word = "cancer_NN",
    lemma_table = "test_new_table_1",
    json = json_file
)
test_that("make table with good word", {
    check_docker()
    expect_null(do.call(create_database, arguments))
})
#####
arguments <- list(
    word = "wrong_word",
    lemma_table = "test_new_table_2",
    json = json_file
)
test_that("make table with wrong word", {
    check_docker()
    expect_error(
        do.call(create_database, arguments),
        paste0('given word: "', arguments$word, '" does not exist in database')
    )
})
#####
