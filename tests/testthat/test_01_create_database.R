##############################################################################################################
# test_create_database.R
library(KAROLA)
context("create_database")

jsonFile <- file.path(system.file(package="KAROLA"),"extdata","test.json")

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
