##############################################################################################################
# test_00_prepere_to_test.R
library(KAROLA)
context("prepere_to_test")

KAROLA_container_id <- system("docker ps -q -f name=KarolaTestDatabase", intern = T)
if (length(KAROLA_container_id) > 0){
    try(system("docker stop KarolaTestDatabase"))
    try(system("docker rm KarolaTestDatabase"))
}

system("docker run --name KarolaTestDatabase -p 5430:5432 -d daroso/karola-test-database:0.9002")

log_from_karola_docker <- ""
time_from_ran <- 0
print("Please wait a moment, loading postgres database.")
while (log_from_karola_docker[length(log_from_karola_docker)] != "LOG:  autovacuum launcher started"){
    time_from_ran <- time_from_ran + 1
    print(paste0("Proces starts ", time_from_ran, " second ago."))
    log_from_karola_docker <- system2("docker", "logs KarolaTestDatabase", stdout = TRUE, stderr = TRUE)
    Sys.sleep(1)
    if (time_from_ran > 30) break
}
print("Postgres database was loaded.")
Sys.sleep(3)
