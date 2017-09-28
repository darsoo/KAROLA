##############################################################################################################
# test_create_data_to_visualization.R
library(KAROLA)
context("create_data_to_visualization")

jsonFile <- file.path(system.file(package="KAROLA"),"extdata","test.json")

# dictionary <- download_dictionary(json = jsonFile)
#
# test_that("test dictionary data frame param", {
#     expect_is(dictionary, "data.frame")
#     expect_equal(nrow(dictionary), 10307)
#     expect_equal(ncol(dictionary), 2)
#     expect_equal(dictionary[2,1], 11817)
# })

##############################################################################################################

drop_test_data_base(json = jsonFile)

##############################################################################################################
# system("docker stop KarolaTestDatabase")
# system("docker rm KarolaTestDatabase")
