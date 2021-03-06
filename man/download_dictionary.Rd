% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02_datebase_to_table.R
\name{download_dictionary}
\alias{download_dictionary}
\title{Create table with dictionary of all words.}
\usage{
download_dictionary(json)
}
\arguments{
\item{json}{"json varible" or json file. JSON need to indlude: \cr
dbname      Character. Database name. \cr
host        Character. Host. \cr
port        Numeric. Port number. \cr \cr
And could indlude: \cr \cr
user        Character. PostgreSQL username. \cr
password    Character. PostgreSQL user's password. \cr}
}
\value{
Table with all words existing in source data base.
}
\description{
Create table with dictionary of all words.
}
\examples{
\dontrun{
download_dictionary()
}
}
