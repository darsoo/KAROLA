
#####

#' Create table with count all words in analyzing abstracts.
#'
#' @param lemma_table Character. Name of source table or table from \code{\link{create_database}}.
#' @param json    "json varible" or json file. JSON need to indlude: \cr
#' \code{dbname}      Character. Database name. \cr
#' \code{host}        Character. Host. \cr
#' \code{port}        Numeric. Port number. \cr \cr
#' And could indlude: \cr \cr
#' \code{user}        Character. PostgreSQL username. \cr
#' \code{password}    Character. PostgreSQL user's password. \cr
#' @param first_analyzed_date Date : YYYY-MM-DD. Month to start analysis
#' @param last_analyzed_date Date : YYYY-MM-DD. Month to stop analysis
#' @return Table with count words from \code{lemma_table}.
#' @importFrom DBI dbDriver
#' @import RPostgreSQL
#' @import jsonlite
#' @import getPass
#' @export
#' @examples
#' \dontrun{
#' create_table_with_count_words("new_table")
#' }


create_table_with_count_words <-
    function(lemma_table = "new_table",
             #table from '01_cerate_...'
             json,
             #date input format "YYYY-MM-01"
             first_analyzed_date = "2000-01-01",
             last_analyzed_date = "2016-12-01") {

    # check param class
    json <- jsonlite::fromJSON(json)
    dbname <- json$dbname
    host <- json$host
    port <- json$port
    user <- json$user
    password <- json$password
    if (is.null(user)) {
        user <-
            getPass::getPass(msg = "write postges username", noblank = T)
    }
    if (is.null(password)) {
        password <-
            getPass::getPass(msg = paste0("write password for ", user),
                    noblank = T)
    }

    if (class(lemma_table) != "character") stop("param class error")
    if (class(dbname) != "character") stop("param class error -db")
    if (class(host) != "character") stop("param class error -h")
    if (class(port) != "integer") stop("param class error-p")
    if (class(user) != "character") stop("param class error")
    if (class(password) != "character") stop("param class error")
    if (class(try(as.Date(first_analyzed_date), silent = T)
    ) != "Date")
        stop("param class error")
    if (class(try(as.Date(last_analyzed_date), silent = T)
    ) != "Date")
        stop("param class error")

    # create a connection
        drv <- DBI::dbDriver("PostgreSQL")
        connection <- RPostgreSQL::dbConnect(drv, dbname = dbname,
                         host = host, port = port,
                         user = user, password = password)
    # check that exist table with given name
        table_exist_check <- RPostgreSQL::dbExistsTable(connection, lemma_table)

    # break function if table doesn't exist
        if (table_exist_check == F) {
            output <-
                print(paste0(
                    "Table ",
                    lemma_table,
                    " doesn't exist. Please change table name"
                ))
            stop(output, call. = T)
        }

    # download date from postgreSQL
        date_query <- paste0("SELECT * FROM date_table")
        date_table <- RPostgreSQL::dbGetQuery(connection, date_query)
    # create new_date - month from 1800-01-01
        first_analyzed_new_date <-
            date_table[date_table["date"] == first_analyzed_date, "new_date"]
        last_analyzed_new_date <-
            date_table[date_table["date"] == last_analyzed_date, "new_date"]
    # create 2 empty df with given parameter
        df_for_month_in_year <-
            data.frame(word_id = "", stringsAsFactors = FALSE)
        output_df <-
            data.frame(word_id = "", stringsAsFactors = FALSE)
    # for loop month by month
        for (i in first_analyzed_new_date:last_analyzed_new_date){
        # load both data format
            date_id <- date_table[date_table["new_date"] == i, "date"]
        # count words in the given time interval
            query_words_count_in_month <-
                paste0(
                    "SELECT unnest(words) AS word_id, count(unnest(words)) AS c",
                    i, " FROM ", lemma_table,
                    " WHERE pmid IN (SELECT pmid FROM date WHERE (date = '",
                    date_id, "'))
                    GROUP by word_id
                    ORDER by word_id;")
            print(paste("Now ", date_id, " is being analyzed"))
            df_words_count_in_month <-
                RPostgreSQL::dbGetQuery(connection, query_words_count_in_month)
            # add month to yearly df
            df_for_month_in_year <-
                merge(
                    df_for_month_in_year,
                    df_words_count_in_month,
                    by = "word_id",
                    all = T,
                    stringsAsFactors = FALSE
                )
            # conditional instruction checking Year or end of data
            if (i %% 12 == 0 | i == last_analyzed_new_date){
                Year <- 1800 + (i %/% 12) - 1
            # if last date is December it's necessary to make correction
                if (i %% 12 != 0)
                    Year <- 1800 + (i %/% 12)
            # sum all month in year and add new column in output df
                Year_as_character <- as.character(Year)
                if ( (i %% 12 == 0 &
                      i == first_analyzed_new_date) |
                     (i %% 12 == 1 &
                      i == last_analyzed_new_date)) {
                     df_for_month_in_year[, Year_as_character] <-
                        df_for_month_in_year[, 2]
                } else {
                    df_for_month_in_year[, Year_as_character] <-
                        rowSums(df_for_month_in_year[, c(2:length(df_for_month_in_year))], na.rm = T)
                }
                df_year_summary <-
                    df_for_month_in_year[df_for_month_in_year[, Year_as_character] > 1, c(1, length(df_for_month_in_year))]
                output_df <-
                    merge(
                        output_df,
                        df_year_summary,
                        by = "word_id",
                        all = T,
                        stringsAsFactors = FALSE
                    )
                # empty yearly df
                df_for_month_in_year <-
                    data.frame(word_id = "", stringsAsFactors = FALSE)
            }
        }
    RPostgreSQL::dbDisconnect(connection)
    DBI::dbUnloadDriver(drv)
    return(output_df)
}


#####

#' Create table with dictionary of all words.
#'
#' @param json    "json varible" or json file. JSON need to indlude: \cr
#' dbname      Character. Database name. \cr
#' host        Character. Host. \cr
#' port        Numeric. Port number. \cr \cr
#' And could indlude: \cr \cr
#' user        Character. PostgreSQL username. \cr
#' password    Character. PostgreSQL user's password. \cr
#' @return Table with all words existing in source data base.
#' @importFrom DBI dbDriver
#' @import RPostgreSQL
#' @import getPass
#' @import jsonlite
#' @export
#' @examples
#' \dontrun{
#' download_dictionary()
#' }
download_dictionary <- function(json){

    json <- jsonlite::fromJSON(json)
    dbname <- json$dbname
    host <- json$host
    port <- json$port
    user <- json$user
    password <- json$password

    # check param class
    if (class(dbname) != "character")
        stop("param class error")
    if (class(host) != "character")
        stop("param class error")
    if (class(port) != "integer")
        stop("param class error")
    if (user == "") {
        user <-
            getPass::getPass(msg = "write postges username", noblank = T)
    }
    if (password == "") {
        password <-
            getPass::getPass(msg = paste0("write password for ", user),
                    noblank = T)
    }
    if (class(user) != "character")
        stop("param class error")
    if (class(password) != "character")
        stop("param class error")

    # create a connection
    drv <- DBI::dbDriver("PostgreSQL")
    connection <- RPostgreSQL::dbConnect(drv, dbname = dbname,
                            host = host, port = port,
                            user = user, password = password)
    query <- ("SELECT * FROM library")
    dictionary <- RPostgreSQL::dbGetQuery(connection, query)
    RPostgreSQL::dbDisconnect(connection)
    DBI::dbUnloadDriver(drv)
    return(dictionary)
}

#####

#' Parse and clearing data to visualisation in shiny-app ANNA.
#'
#' @param input         Table. Table with count words from \code{\link{create_table_with_count_words}}.
#' @param output_file   character. Opis.
#' @param dictionary    character. Location and name of dictionary file.
#' @param save_to_file  Logical. If TRUE create a output file with name:
#'   \code{output_file}
#' @return Table with count words in every time interval (default - year)
#' @export
#' @examples
#' \dontrun{
#' prepere_data_to_ANNA()
#' }
prepere_data_to_ANNA <-
    function(input = "input",
             output_file = "output_file",
             dictionary = "dictionary",
             save_to_file = T
             ){
        # check param class
        if (class(input) != "character") stop("param class error")
        if (class(output_file) != "character") stop("param class error")
        if (class(dictionary) != "character") stop("param class error")
        if (class(save_to_file) != "logical") stop("param class error")
        #
        output <- merge(input, dictionary, by = "word_id", all.x = T)
        output <- output[, c(1, length(output), 2:(length(output) - 1))]
        output <- output[2:nrow(output), ]
        output[is.na(output)] <- 0
        if (save_to_file == T){
            utils::write.table(output, file = output_file, sep = "\t")
        }
        return(output)
    }

#####

#' Opis funkcji
#'
#' @param word        Character. Word which is analyzing.Its nesesary to add
#'   sufix after "_" with part of speech tag. List of all tag and their expands
#'   is available on site:
#'   \url{http://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html}.
#' @param lemma_table         Character. Name of table which will be created or
#'   existing table from \code{\link{create_database}}
#' @param table_exist         Logical. If FALSE, function automatically  create new
#'   table with name: \code{lemma_table}
#' @param json    "json varible" or json file. JSON need to indlude: \cr
#' dbname      Character. Database name. \cr
#' host        Character. Host. \cr
#' port        Numeric. Port number. \cr \cr
#' And could indlude: \cr \cr
#' user        Character. PostgreSQL username. \cr
#' password    Character. PostgreSQL user's password. \cr
#' @param output_file         Character. Name of output file.
#' @param first_analyzed_date Date : YYYY-MM-DD. Month to start analysis
#' @param last_analyzed_date  Date : YYYY-MM-DD. Month to stop analysis
#' @param dictionary_exist    Logical. If you have a dictionary in file it
#'   should TRUE. Otherwise function prepare this file, but this take a lot of
#'   time.
#' @param save_to_file  Logical. If TRUE create a output file with name:
#'   \code{output_file}
#' @return Table with count words in every time interval (default - year) in
#'   abstracts with \code{word}.
#' @import getPass
#' @import jsonlite
#' @export

create_data_to_visualization <- function(word,
                                         lemma_table,
                                         table_exist = F,
                                         output_file = "output_file",
                                         first_analyzed_date = "2000-01-01",
                                         last_analyzed_date = "2016-12-01",
                                         dictionary_exist = T,
                                         json,
                                         save_to_file = T){
    json <- jsonlite::fromJSON(json)
    user <- json$user
    password <- json$password
    if (is.null(user)) {
        user <-
            getPass::getPass(msg = "write postges username", noblank = T)
    }
    if (is.null(password)) {
        password <-
            getPass::getPass(msg = paste0("write password for ", user),
                    noblank = T)
    }
    json <- jsonlite::toJSON(json)

    # check param class
    if (class(word) != "character")
        stop("param class error")
    if (class(lemma_table) != "character")
        stop("param class error")
    if (class(table_exist) != "logical")
        stop("param class error")
    if (class(output_file) != "character")
        stop("param class error")
    if (class(try(as.Date(first_analyzed_date), silent = T)
    ) != "Date")
        stop("param class error")
    if (class(try(as.Date(last_analyzed_date), silent = T)
    ) != "Date")
        stop("param class error")
    if (class(dictionary_exist) != "logical")
        stop("param class error")
    if (class(save_to_file) != "logical")
        stop("param class error")

    #
    if (table_exist == F) {
        create_database(
            word = word,
            lemma_table = lemma_table,
            json = json
        )
    }
    table_with_count_words <-
        create_table_with_count_words(
            lemma_table = lemma_table,
            first_analyzed_date = first_analyzed_date,
            last_analyzed_date = last_analyzed_date,
            json = json
        )
    if (dictionary_exist == F) {
        dictionary <- download_dictionary(
            json = json
        )
    } else{

        dictionary <-
            utils::read.table(
                file = "dictionary",
                header = T,
                sep = "\t",
                stringsAsFactors = F
            )
    }
    prepere_data_to_ANNA(input = table_with_count_words,
                        dictionary = dictionary,
                        save_to_file = save_to_file,
                        output_file = output_file)
}

#####
