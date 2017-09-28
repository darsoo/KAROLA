##############################################################################################################
# test_create_database.R
library(KAROLA)
context("create_database")

KAROLA_container_id <- system("sudo docker ps -q -f name=KarolaTestDatabase", intern = T)
if (length(KAROLA_container_id) > 0){
    try(system("docker stop KarolaTestDatabase"))
    try(system("docker rm KarolaTestDatabase"))
}

system("docker run --name KarolaTestDatabase -p 5430:5432 -d daroso/karola-test-database:0.9002")

for(time in 0:5){
    print(paste("Please wait ",6-time," sec."))
    Sys.sleep(1)
}

jsonFile <- file.path(system.file(package="KAROLA"),"extdata","test.json")

# client <- docker::docker$from_env()
# client$create_container(image = "karola-test-database",
#                       #name = "k-test-db",
#                       ports=list('5432'='5430'),
#                       detach = T)

#drop_test_data_base(json = jsonFile:)
create_test_data_base(json = jsonFile)

#####
arguments <- list(
    word = "cancer_NN",
    lemma_table = "test_new_table_1",
    json = jsonFile
)
test_that("make table with good word",{
    expect_null(do.call(create_database, arguments))
})
#####
arguments <- list(
    word = "wrong_word",
    lemma_table = "test_new_table_2",
    json = jsonFile
)
test_that("make table with wrong word",{
    expect_error(do.call(create_database, arguments),paste0('given word: "' ,arguments$word ,'" does not exist in database'))
})
#####
drop_test_data_base(json = jsonFile)
##############################################################################################################
# test_create_table_with_count_words.R

library(KAROLA)
context("create_table_with_count_words")

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

##############################################################################################################
# test_download_dictionary.R

dictionary <- download_dictionary(json = jsonFile)

test_that("test dictionary data frame param", {
    expect_is(dictionary, "data.frame")
    expect_equal(nrow(dictionary), 10307)
    expect_equal(ncol(dictionary), 2)
    expect_equal(dictionary[2,1], 11817)
})

##############################################################################################################

drop_test_data_base(json = jsonFile)

##############################################################################################################
system("docker stop KarolaTestDatabase")
system("docker rm KarolaTestDatabase")
