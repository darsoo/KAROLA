
##############################################################################################################

# test_download_dictionary.R
context("create_database")
library(KAROLA)

json_file <- file.path(system.file(package = "KAROLA"), "extdata", "test.json")

test_that("test dictionary data frame param", {
    check_docker()
    dictionary <- download_dictionary(json = json_file)
    expect_is(dictionary, "data.frame")
    expect_equal(nrow(dictionary), 10307)
    expect_equal(ncol(dictionary), 2)
    expect_equal(dictionary[2, 1], 11817)
})
