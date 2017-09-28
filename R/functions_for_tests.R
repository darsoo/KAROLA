create_test_data_base <- function(json){
    json <- fromJSON(json)
    dbname <- json$source_dbname
    host <- json$host
    port <- json$port
    user <- json$user
    password <- json$password

    drv <- DBI::dbDriver("PostgreSQL")
    connection <- RPostgreSQL::dbConnect(drv, dbname = dbname,
                                         host = host, port = port,
                                         user = user, password = password)
    query <- paste0("CREATE DATABASE test_data_base WITH TEMPLATE ",dbname,";")
    dbGetQuery(connection, query)
    RPostgreSQL::dbDisconnect(connection)
    DBI::dbUnloadDriver(drv)
}

drop_test_data_base <- function(json){
    json <- fromJSON(json)
    dbname <- json$source_dbname
    host <- json$host
    port <- json$port
    user <- json$user
    password <- json$password

    drv <- DBI::dbDriver("PostgreSQL")
    connection <- RPostgreSQL::dbConnect(drv, dbname = dbname,
                                         host = host, port = port,
                                         user = user, password = password)
    query <- "DROP DATABASE IF EXISTS test_data_base;"
    dbGetQuery(connection, query)
    RPostgreSQL::dbDisconnect(connection)
    DBI::dbUnloadDriver(drv)
}

check_docker <- function() {
KAROLA_container_id <- system("docker ps -q -f name=KarolaTestDatabase", intern = T)
    if (length(KAROLA_container_id) == 0) {
        testthat::skip("postgers database not available")
        }
    }

