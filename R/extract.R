extract_accs <- function(MDBPATH) {

  requireNamespace() #to be able to pipe

  # Define connection strings
  dbq_string <- paste0("DBQ=", getwd(),MDBPATH)
  driver_string <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
  db_connect_string <- paste0(driver_string, dbq_string)

  # Create .accdb connection
  conAcc <- RODBC::odbcDriverConnect(db_connect_string)

  #create list of tables in the database
  TABLES <- RODBC::sqlTables(conAcc)

  TABLE_NAMES <-TABLES %>%
    dplyr::filter(TABLES$TABLE_TYPE == "TABLE") %>%
    dplyr::select(TABLES$TABLE_NAME)

  ## Iterate through list of tables to get a vector of their names
  for (tbl_names in TABLE_NAMES) {
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