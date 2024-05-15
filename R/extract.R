#' Load tables from access
#'
#' This function loads tables in an access database into r as
#' dataframes with the same name as the access tables.
#'
#' @param MDBPATH Path to the access database. Remeber to ass the back slash
#' before the first folder name. If you are working in a project the file path
#' should only be the part from the project onwards.
#' @return Returns all tables in the access database as r dataframes
#' with the same name.
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
#' # example code - this won't run but is for you to see the correct formatting
#'  extract_accs("/data/FishMaster.accdb")
#'
#'
#'
extract_accs <- function(MDBPATH) {

  #requireNamespace(magrittr) #to be able to pipe

  #define path
  pth <- list.files(path="data", recursive = TRUE, pattern = MDBPATH, full.names = T)

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
