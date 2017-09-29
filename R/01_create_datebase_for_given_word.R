
#' create_database
#'
#' Create a new table with abstracts which include search word. To correct work
#' this function needs 2 tables in PostgreSQL : \cr
#' library - word_id:INTEGER, word:TEXT \cr
#' abstracts - pmid:INTEGER, words:INTEGER[] \cr
#'
#'
#' @param word        Character. Word which is analyzing.Its nesesary to add
#'   sufix after "_" with part of speech tag. List of all tag and their expands
#'   is available on site:
#'   \url{http://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html}.
#' @param lemma_table Character. Name of table which will be created.
#' @param json    "json varible" or json file. JSON need to indlude: \cr
#' dbname      Character. Database name. \cr
#' host        Character. Host. \cr
#' port        Numeric. Port number. \cr \cr
#' And could indlude: \cr \cr
#' user        Character. PostgreSQL username. \cr
#' password    Character. PostgreSQL user's password. \cr
#' @return There is no output. \cr
#' This function create table in postgres with abstracts which include \code{word}.
#' @importFrom DBI dbDriver
#' @import RPostgreSQL
#' @import getPass
#' @import jsonlite
#' @export
#' @examples
#' \dontrun{
#' create_database("cancer_NN")
#' }
create_database <- function(word,
                            lemma_table = "new_table",
                            json) {
    # check param class
    json <- fromJSON(json)
    dbname <- json$dbname
    host <- json$host
    port <- json$port
    user <- json$user
    password <- json$password
    if (is.null(user)) {
        user <- getPass(msg = "write postges username", noblank = T)
    }
    if (is.null(password)) {
        password <-
            getPass(msg = paste0("write password for ", user),
                    noblank = T)
    }

    # check param class
    if (class(word) != "character")
        stop("param class error")
    if (class(lemma_table) != "character")
        stop("param class error")
    if (class(dbname) != "character")
        stop("param class error")
    if (class(host) != "character")
        stop("param class error")
    if (class(port) != "integer")
        stop("param class error")
    if (user == "") {
        user <-
            getPass(msg = "write postges username", noblank = T)
    }
    if (password == "") {
        password <-
            getPass(msg = paste0("write password for ", user), noblank = T)
    }
    if (class(user) != "character")
        stop("param class error")
    if (class(password) != "character")
        stop("param class error")

    # create a connection
    drv <- DBI::dbDriver("PostgreSQL")
    connection <- RPostgreSQL::dbConnect(
        drv,
        dbname = dbname,
        host = host,
        port = port,
        user = user,
        password = password
    )
    # check that exist table with given name
    table_exist_check <-
        RPostgreSQL::dbExistsTable(connection, lemma_table)
    # break function if table exist
    if (table_exist_check == T) {
        output <-
            print(paste0(
                "Table ",
                lemma_table,
                " already exist. Please change table name"
            ))
        stop(output, call. = T)
    }
    if (table_exist_check == F) {
        # download word-id from db
        query_word_id_download <-
            paste0("SELECT word_id FROM library WHERE word = '", word, "';")
        word_id <-
            RPostgreSQL::dbGetQuery(connection, query_word_id_download)
        # try to change word_id's classfrom data.frame to numeric
        try (word_id <- as.numeric(word_id))
        # create new table with PMID with given word
        if (class(word_id) == "numeric" & length(word_id) != 0) {
            query_creat_table <-
                paste0("SELECT * INTO ",
                       lemma_table,
                       " FROM abstracts WHERE (",
                       word_id,
                       " = ANY(words));")
            df_postgres <-
                RPostgreSQL::dbGetQuery(connection, query_creat_table)
            print(paste0("Table ", lemma_table, " successfully"))
        } else{
            RPostgreSQL::dbDisconnect(connection)
            DBI::dbUnloadDriver(drv)
            stop(paste0('given word: "', word, '" does not exist in database'))
        }
    }
    RPostgreSQL::dbDisconnect(connection)
    DBI::dbUnloadDriver(drv)
    return()
}
