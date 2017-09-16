
#' create_database
#'
#' Create a new table with abstracts which indlude search word. To correct work
#' this function needs 2 tables in PostgreSQL : \cr
#' library - word_id:INTEGER, word:TEXT \cr
#' abstracts - pmid:INTEGER, words:INTEGER[] \cr
#'
#'
#' @param word        Character. Word which is analyzing.It's nesesary to add
#'   sufix after "_" with part of speech tag. List of all tag and thier expands
#'   is available on site:
#'   \url{http://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html}.
#' @param lemma_table Character. Name of table which will be created.
#' @param dbname      Character. Information about PostgreSQL database
#'   connection. Database name.
#' @param host        Character. Information about PostgreSQL database
#'   connection. Host.
#' @param port        Numeric. Information about PostgreSQL database connection.
#'   Port number.
#' @param user        Character. Information about PostgreSQL database
#'   connection. PostgreSQL username.
#' @param password    Character. Information about PostgreSQL database
#'   connection. PostgreSQL user's password.
#' @return There is no output. \cr
#' This function create table in postgres with abstracts which indlude \code{word}.
#' @importFrom DBI dbDriver
#' @import RPostgreSQL
#' @export
#' @examples
#' #create_database("cancer_NN")
create_database <- function(word,
                            lemma_table = "new_table",
                            dbname = "medline",
                            host = "localhost",
                            port = 5432,
                            user = "username",
                            password = "password") {
    # check param class
    if (class(word) != "character") stop("param class error")
    if (class(lemma_table) != "character") stop("param class error")
    if (class(dbname) != "character") stop("param class error")
    if (class(host) != "character") stop("param class error")
    if (class(port) != "numeric") stop("param class error")
    if (class(user) != "character") stop("param class error")
    if (class(password) != "character") stop("param class error")

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
        stop(output , call. = T)
    }
    if (table_exist_check == F) {
        # download word-id from db
        query_word_id_download <-
            paste0("SELECT word_id FROM library WHERE word = '", word, "';")
        word_id <-
            RPostgreSQL::dbGetQuery(connection, query_word_id_download)
        # create new table with PMID with given word
        query_creat_table <-
            paste0(
                "SELECT * INTO ",
                lemma_table,
                " FROM abstracts WHERE (",
                word_id,
                " = ANY(words));"
            )
        df_postgres <-
            RPostgreSQL::dbGetQuery(connection, query_creat_table)
        print(paste0("Table ", lemma_table, " successfully"))
    }
    RPostgreSQL::dbDisconnect(connection)
    DBI::dbUnloadDriver(drv)
    return()
}
