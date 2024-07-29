
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
#'
#'@importFrom DBI  dbConnect
#'@importFrom RPostgres  Postgres
#'
#' @examples
#' ## example code
#' # con <- sql_con("occur_stage")
#' @export
sql_con <- function(DBNAME) {


  ## connect to sql database

    con <- DBI::dbConnect(RPostgres::Postgres(),
                          dbname = DBNAME,
                          port = Sys.getenv("DB_PORT"),
                          user = Sys.getenv("DB_USER"),
                          password = Sys.getenv("DB_PASSWORD"))
}
  # assign("con", con, envir=.GlobalEnv)


#' extract all tables
#'
#' This function extracts all tables in a postgres database that
#' match the string provided
#'
#' @param DBNAME is the name of the database you wish to connect to.
#' It should be a charater with inverted commas.
#' @param STR is the character string that should occur in all tables that
#' you wish to retrieve from the database.
#' @return All tables with the matching string as dataframes with the same
#' names as those in the database. It should be a character in inverted
#' commas
#'
#'@importFrom DBI  dbConnect
#'@importFrom DBI  dbListTables
#'@importFrom DBI  dbReadTable
#'@importFrom RPostgres  Postgres
#'@importFrom dplyr  filter
#'@importFrom stringr  str_detect
#'@importFrom RODBC  odbcCloseAll
#'
#' @examples
#' ## example code
#' # sql_tbl_ext("occur_stage", "fm")
#' @export
sql_tbl_ext <- function(DBNAME , STR) {

    con <- DBI::dbConnect(RPostgres::Postgres(),
                          dbname = DBNAME,
                          port = Sys.getenv("DB_PORT"),
                          user = Sys.getenv("DB_USER"),
                          password = Sys.getenv("DB_PASSWORD"))

  # con <- sql_con(DBNAME) #could use my other function, but this may break it.
  ## the first method is for windows and the second for linux


  #create list of tables in the database
  TABLES <- DBI::dbListTables(con) %>%
    as.data.frame() %>%
    dplyr::filter(stringr::str_detect(.,STR))

  ## Iterate through list of tables to get a vector of their names
  for (tbl_names in TABLES) {
  }

  for(tbl in tbl_names) {

    df <- DBI::dbReadTable(con,tbl)

    new_tbl_name <- (tbl)

    ## Assign dataframe to environment
    assign(new_tbl_name, df, envir=.GlobalEnv)
  }

  ## close connection
  RODBC::odbcCloseAll()

}


