#' Load tables from access
#'
#' This function loads tables in an access database into r as
#' dataframes with the same name as the access tables. If there are multiple
#' files with the same name it will take the first instance found, which
#' will be the file closest to the data root folder.
#'
#' @param MDBFILE File name of the access database with the extention.
#' @return Returns all tables in the access database as r dataframes
#' with the same names.
#'
#' @export
#'
#'@importFrom RODBC odbcDriverConnect
#'@importFrom RODBC sqlTables
#'@importFrom RODBC sqlFetch
#'@importFrom RODBC odbcCloseAll
#'@importFrom dplyr  filter
#'@importFrom dplyr  select
#'@importFrom magrittr  %>%
#'
#' @examples
#' ## example code - this won't run but is for you to see the correct formatting
#' # extract_accs("FishMaster.accdb")
#'
#'
#'
extract_accs <- function(MDBFILE) {

  #requireNamespace(magrittr) #to be able to pipe

  #define path
  pth <- (list.files(path="data", recursive = TRUE, pattern = MDBFILE, full.names = T)[1])

  # Define connection strings
  dbq_string <- paste0("DBQ=", pth)
  driver_string <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
  db_connect_string <- paste0(driver_string, dbq_string)

  # Create .accdb connection
  conAcc <- RODBC::odbcDriverConnect(db_connect_string)

  #create list of tables in the database
  TABLES <- RODBC::sqlTables(conAcc) %>%
    dplyr::filter(TABLE_TYPE == "TABLE") %>%
    dplyr::select(TABLE_NAME)

  ## Iterate through list of tables to get a vector of their names
  for (tbl_names in TABLES) {
  }

  for(tbl in tbl_names) {

    df <- RODBC::sqlFetch(conAcc,tbl)

    new_tbl_name <- (tbl)

    ## Assign dataframe to environment
    assign(new_tbl_name, df, envir=.GlobalEnv)
  }

  ## close connection
  RODBC::odbcCloseAll()

  #dfList <- lapply(tables, function(t) dbReadTable(db, t)) (improved method?)

}
