##############################################################################################################
# test_create_data_to_visualization.R
library(KAROLA)
context("create_data_to_visualization")

json_file <- file.path(system.file(package = "KAROLA"), "extdata", "test.json")

arguments <- list(
    word = "cancer_NN",
    lemma_table = "test_table",
    table_exist = F,
    output_file = "output_file",
    first_analyzed_date = "2000-01-01",
    last_analyzed_date = "2016-12-01",
    dictionary_exist = T,
    json = json_file,
    save_to_file = T
)

#####

use_argument <- arguments
use_argument["word"] <- 3

test_that("test param class error - create_data_to_vis...(word)", {
    expect_error(do.call(create_data_to_visualization, use_argument),
                 "param class error")
})

#####

use_argument <- arguments
use_argument["lemma_table"] <- 3

test_that("test param class error - create_data_to_vis...(lemma_table)", {
    expect_error(do.call(create_data_to_visualization, use_argument),
                 "param class error")
})

#####

use_argument <- arguments
use_argument["table_exist"] <- 3

test_that("test param class error - create_data_to_vis...(table_exist)", {
    expect_error(do.call(create_data_to_visualization, use_argument),
                 "param class error")
})

#####

use_argument <- arguments
use_argument["output_file"] <- 3

test_that("test param class error - create_data_to_vis...(output_file)", {
    expect_error(do.call(create_data_to_visualization, use_argument),
                 "param class error")
})

#####

use_argument <- arguments
use_argument["first_analyzed_date"] <- 3

test_that("test param class error - create_data_to_vis...(first_a...)", {
    expect_error(do.call(create_data_to_visualization, use_argument),
                 "param class error")
})

#####

use_argument <- arguments
use_argument["last_analyzed_date"] <- 3

test_that("test param class error - create_data_to_vis...(last_a...)", {
    expect_error(do.call(create_data_to_visualization, use_argument),
                 "param class error")
})

#####

use_argument <- arguments
use_argument["dictionary_exist"] <- 3

test_that("test param class error - create_data_to_vis...(dictionary_exist)", {
    expect_error(do.call(create_data_to_visualization, use_argument),
                 "param class error")
})

#####

use_argument <- arguments
use_argument["save_to_file"] <- 3

test_that("test param class error - create_data_to_vis...(save_to_file)", {
    expect_error(do.call(create_data_to_visualization, use_argument),
                 "param class error")
})


#####
##############################################################################################################

try(drop_test_data_base(json = json_file), silent = T)

##############################################################################################################
 system("docker stop KarolaTestDatabase")
 system("docker rm KarolaTestDatabase")
