dbname <- readline(prompt = "write database name: ")
port <- try(as.numeric(readline(prompt = "write postgres port: ")))
host <- readline(prompt = "write postgres host: " )
user <- readline(prompt = "write postgres username: ")
password <- readline(prompt = "write password for user: ")

# dbname <- "postgres"
# port <- 5432
# host <- "localhost"
# user <- "username"
# password <- "password"

##############################################################################################################
# test_create_database.R
library(KAROLA)
context("create_database")

drop_test_data_base(dbname = dbname, host = host, port = port, user = user, password = password)
create_test_data_base(dbname = dbname, host = host, port = port, user = user, password = password)

#####
arguments <- list(
    word = "cancer_NN",
    lemma_table = "test_new_table_1",
    dbname = "test_data_base",
    host = host,
    port = port,
    user = user,
    password = password
)
test_that("make table with good word",{
    expect_null(do.call(create_database, arguments))
})
#####
arguments <- list(
    word = "wrong_word",
    lemma_table = "test_new_table_2",
    dbname = "test_data_base",
    host = "localhost",
    port = 5432,
    user = "username",
    password = "password"
)
test_that("make table with wrong word",{
    expect_error(do.call(create_database, arguments))
})
#####
drop_test_data_base(dbname = dbname, host = host, port = port, user = user, password = password)
##############################################################################################################
# test_create_table_with_count_words.R

library(KAROLA)
context("create_table_with_count_words")

create_test_data_base(dbname = dbname, host = host, port = port, user = user, password = password)

#####
arguments <- list(lemma_table = "test_table",
                  dbname = "test_data_base",
                  host = "localhost",
                  port = 5432,
                  user = "username",
                  password = "password",
                  first_analyzed_date = "2016-01-01",
                  last_analyzed_date = "2016-12-01")

do.call(create_database, c("cancer_NN",arguments[c("lemma_table","dbname","host","port","user","password")]))
#####
function_output <- do.call(create_table_with_count_words, arguments)

test_that("background info", {
    expect_equal(function_output[2,1], "0")
    expect_equal(function_output[2,2], 175)
})
#####
use_argument <- arguments
use_argument["first_analyzed_date"] <- "2015-12-01"
use_argument["last_analyzed_date"] <- "2016-02-01"
function_output <- ""
function_output <- do.call(create_table_with_count_words, use_argument)

test_that("test for fix bug - 1", {
    expect_is(function_output, "data.frame")
})
#####
use_argument <- arguments
use_argument["first_analyzed_date"] <- "2015-10-01"
use_argument["last_analyzed_date"] <- "2016-01-01"
function_output <- ""
function_output <- do.call(create_table_with_count_words, use_argument)

test_that("test for fix bug - 2", {
    expect_is(function_output, "data.frame")
})
#####
drop_test_data_base(dbname = dbname, host = host, port = port, user = user, password = password)

##############################################################################################################
