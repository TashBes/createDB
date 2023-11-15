
#' connect to a Postgresql database
#'
#' This function will connect to a postgresql database
#' given a database name. It requires that you have saved the
#' database port, username, and password in your .Renviron file.
#' This function works for both windows and linux systems.
#'
#' To save variables in your .Renviron file open a text file, add
#' DB_PORT, DB_USER, and DB_PASSWORD with an = after each and the
#' relevant value. Then run the code, save the file as .Renviron,
#' and exit and restart R.
#'
#' @param DBNAME is the name of the database you wish to connect to.
#' It should be a charater with inverted commas.
#' @return a connection object to the sql database provided.
#' @example con <- sql_con("occur_stage")
#' @export
sql_con <- function(DBNAME) {


  ## connect to sql database
  if (Sys.info()["sysname"] == "Windows") {
    con <- DBI::dbConnect(RPostgres::Postgres(),
                          dbname = DBNAME,
                          port = Sys.getenv("DB_PORT"),
                          user = Sys.getenv("DB_USER"),
                          password = Sys.getenv("DB_PASSWORD"))
  } else {
    con <- DBI::dbConnect(
      drv = dbDriver("PostgreSQL"),
      dbname=DBNAME,
      host = Sys.getenv("DB_HOST"),
      port = Sys.getenv("DB_PORT"),
      user = Sys.getenv("DB_USER"),
      password = Sys.getenv("DB_PASSWORD")
    )
  }
  # assign("con", con, envir=.GlobalEnv)
}
