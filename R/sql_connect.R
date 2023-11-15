libraries <- function (){

  #definitely used
  library(RODBC)##for all databases
  library(DBI)## for sql
  library(tidyverse)
  if (Sys.info()["sysname"] != "Windows") { # load the appropriate packages depending on OS and approach; Windows seems to work with RPostgres but not with RPostgreSQL? and vice versa. this may need some further investigation
    library(RPostgreSQL)
    library(Hmisc)
  } else
    library(RPostgres)
  ## the approach used is different for different operating systems
  ## due to the ease of using the dbmtools library and Hmisc package
  ## on Linux, whereas installing dbmtools on Windows seems messy,
  ## therefore recommended to stick with the RODBC query tools on Windows
  ## at the moment, the approach assumes a Mac OS will be the same as
  ## Linux (and would require installation of dbmtools), but not yet
  ## tested

}


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


sql_tbl_ext <- function(DBNAME , STR) {


  if (Sys.info()["sysname"] == "Windows") {
    con <- DBI::dbConnect(RPostgres::Postgres(),
                          dbname = DBNAME,
                          port = Sys.getenv("DB_PORT"),
                          user = Sys.getenv("DB_USER"),
                          password = Sys.getenv("DB_PASSWORD"))
  } else {
    con <- DBI::dbConnect(
      drv = dbDriver("PostgreSQL"),
      dbname = DBNAME,
      host = Sys.getenv("DB_HOST"),
      port = Sys.getenv("DB_PORT"),
      user = Sys.getenv("DB_USER"),
      password = Sys.getenv("DB_PASSWORD"))
  }

  # con <- sql_con(DBNAME) #could use my other function, but this may break it.
  ## the first method is for windows and the second for linux


  #create list of tables in the database
  TABLES <- DBI::dbListTables(con) %>%
    as.data.frame() %>%
    dplyr::filter(str_detect(.,ABBR))

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
  odbcCloseAll()

}
