##############################################################################################################
# test_create_database.R
library(KAROLA)
context("create_database")

KAROLA_container_id <- system("docker ps -q -f name=KarolaTestDatabase", intern = T)
if (length(KAROLA_container_id) > 0){
    try(system("docker stop KarolaTestDatabase"))
    try(system("docker rm KarolaTestDatabase"))
}

system("docker run --name KarolaTestDatabase -p 5430:5432 -d daroso/karola-test-database:0.9002")

log_from_karola_docker <- ''
time_from_ran <- 0
print("Please wait a moment, loading postgres database.")
while(log_from_karola_docker[length(log_from_karola_docker)] != "LOG:  autovacuum launcher started"){
    time_from_ran <- time_from_ran + 1
    print(paste0("Proces starts ",time_from_ran," second ago."))
    log_from_karola_docker <- system2("docker","logs KarolaTestDatabase",stdout = TRUE,stderr = TRUE)
    Sys.sleep(1)
}
print("Postgres database was loaded.")
Sys.sleep(3)

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
