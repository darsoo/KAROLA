create_test_data_base <- function(dbname = "medline",
                                  host = "localhost",
                                  port = 5432,
                                  user = "",
                                  password = ""){
    drv <- DBI::dbDriver("PostgreSQL")
    connection <- RPostgreSQL::dbConnect(drv, dbname = dbname,
                                         host = host, port = port,
                                         user = user, password = password)
    query <- paste0("CREATE DATABASE test_data_base WITH TEMPLATE ",dbname," OWNER ",user,";")
    dbGetQuery(connection, query)
    RPostgreSQL::dbDisconnect(connection)
    DBI::dbUnloadDriver(drv)
}

drop_test_data_base <- function(dbname = "medline",
                                host = "localhost",
                                port = 5432,
                                user = "",
                                password = ""){
    drv <- DBI::dbDriver("PostgreSQL")
    connection <- RPostgreSQL::dbConnect(drv, dbname = dbname,
                                         host = host, port = port,
                                         user = user, password = password)
    query <- "DROP DATABASE IF EXISTS test_data_base;"
    dbGetQuery(connection, query)
    RPostgreSQL::dbDisconnect(connection)
    DBI::dbUnloadDriver(drv)
}
