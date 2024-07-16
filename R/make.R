

##
#'make the date table
#'
#'To make the data table, instead of adding dates from the data we will
#'create a table with every date over a date range that our data is
#'expected to cover. This may need to be extended as this work extends passed
#'2025, but extra rows can be added easily enough.
#'It is easier to make the table in R so that we can use the lubridate functions
#'
#' @return Returns ta table with every date at daily, yearly, and monthly intervals from 1890/01/01
#' to 2025/01/01.
#'
#'@importFrom dplyr mutate
#'@importFrom dplyr if_else
#'@importFrom magrittr  %>%
#'@importFrom lubridate month
#'@importFrom lubridate year
#'@importFrom lubridate wday
#'@importFrom lubridate day
#'@importFrom lubridate yday
#'@importFrom lubridate isoweek
#'@importFrom lubridate ceiling_date
#'@importFrom lubridate quarter
#'@importFrom chron is.weekend
#'
#' @examples
#' # example code
#'  date_tbl <- make_dim_date()
#'
#' @export
make_dim_date <- function()  {

  ##Make a date range

  ## defining start date
  start_date <- as.Date("1890/01/01")

  ## defining end date
  end_date <- as.Date("2025/01/01")

  ## generating range of dates
  date_full_date <- seq(start_date, end_date,"days")

  #add range to table
  day_tbl <- data.frame(date_full_date)

  ## add dim_date table columns and populate them using the date range values.
  ## important to known that for date_weekday_weekend true indicates a weekend
  ## and for date_last_date_of_month_indicator true indicates it is the last
  ## day of the month.

  dim_date <-day_tbl %>%
    dplyr::mutate(date_name = format(date_full_date, "%d%b%Y")) %>%
    dplyr::mutate(date_month_number = lubridate::month(date_full_date)) %>%
    dplyr::mutate(date_month_name = lubridate::month(date_full_date, label = TRUE)) %>%
    dplyr::mutate(date_calendar_year_number = lubridate::year(date_full_date)) %>%
    dplyr::mutate(date_day_of_week_number = lubridate::wday(date_full_date))%>%
    dplyr::mutate(date_day_of_week_name = lubridate::wday(date_full_date, label=TRUE))%>%
    dplyr::mutate(date_day_of_month_number = lubridate::day(date_full_date))%>%
    dplyr::mutate(date_day_of_year_number = lubridate::yday(date_full_date))%>%
    dplyr::mutate(date_is_weekend = as.character(chron::is.weekend(date_full_date)))%>% #true for weekend
    dplyr::mutate(date_week_of_year_number = lubridate::isoweek(date_full_date) )%>%
    dplyr::mutate(date_last_day_of_month = lubridate::ceiling_date(date_full_date, "month") - lubridate::days(1))%>%
    dplyr::mutate(date_last_day_of_month =
                    dplyr::if_else(date_last_day_of_month == date_full_date, "TRUE", "FALSE"))%>% #True if it is the last day of the month
    dplyr::mutate(date_calendar_quarter_number = lubridate::quarter(date_full_date)) %>%
    dplyr::mutate(date_calendar_year_season_name = dplyr::if_else(date_calendar_quarter_number == 1, "summer",
                                                                  dplyr::if_else(date_calendar_quarter_number == 2, "autumn",
                                                                                 dplyr::if_else(date_calendar_quarter_number == 3, "winter", "spring")), "spring"))


  dim_date <- dim_date %>%
    select(date_calendar_year_number) %>%
    distinct() %>%
    bind_rows(dim_date)

  dim_date <- dim_date %>%
    select(date_calendar_year_number, date_month_number) %>%
    distinct() %>%
    bind_rows(dim_date)

}





##
##
#'create the project table
#'
#'
#' @param PROJ_NAME The name of the project where the records where created
#' @param PROJ_ABBR The abbreviation for the project, usually 3 letters
#' @param PROJ_MAN The manager responsible for maintaining the records, or who can be contacted for help with any queries
#' @param PROJ_DESC A short description of the project.
#' @return Returns a table of one row with the previouse information
#'
#' @examples
#' # example code
#'  fm_project <- make_dim_project("FishMaster", "FM", "Colin Attwood", "Species records from scientifi publications")
#'
#' @export
make_dim_project<- function(PROJ_NAME, PROJ_ABBR, PROJ_MAN, PROJ_DESC) {

  project_name <- c(PROJ_NAME)

  project_abbreviation <- c(PROJ_ABBR)

  project_manager <- c(PROJ_MAN)

  project_description <- c(PROJ_DESC)


  fm_project <- data.frame(project_name,
                           project_abbreviation,
                           project_manager,
                           project_description)


}



#'get a list of distinct species names
#'
#' Gets a list of distinct species names form the
#' database, and renames the species column
#'
#' @param DF The dataframe to run the function on
#' @param SP_COL The column with the species names
#' @return Returns a dataframe with distinct species names
#'
#'@importFrom dplyr  distinct
#'@importFrom magrittr  %>%
#'@importFrom dplyr  rename
#'
#' @examples
#' # example code
#'  taxon <- make_dis_spp(fm_fish_abundance_clean, Species)
#'
#' @export
make_dis_spp <- function(DF, SP_COL)  {

  taxon <- DF %>%
    dplyr::distinct(aphiaid, {{SP_COL}}) %>%
    dplyr::rename(catch_taxon = {{SP_COL}})

}


##
##
##
#'Take out duplicates for loactions, and taxons
#'
#'
#' @param DF The dataframe to run the function on
#' @param SQL_TBL The sql table that matches the dataframe
#' @return Returns the dataframe only records that are not currently in the sql table.
#'
#'@importFrom magrittr  %>%
#'@importFrom DBI dbReadTable
#'@importFrom DBI dbDisconnect
#'@importFrom dplyr anti_join
#'
#' @examples
#' # example code
#'  taxon_new <- make_rmv_dups(taxon, "dim_taxon")
#'
#' @export
make_rmv_dups <- function(DF, SQL_TBL) {

  con <- sql_con("occurrence_dwh")

  sql_tlb<-DBI::dbReadTable(con, SQL_TBL)

  DBI::dbDisconnect(con)

  ##check for duplicates

  newDF <- DF %>%
    dplyr::anti_join(sql_tlb)

}


#'make taxon table
#'
#' To make the taxon table use classification function to retrieve the taxonomic information for each aphia
#' ID from WORMS.
#' Then use rbind to take the information from a list format to a database.
#' Then remove the id column as it is not needed.
#' The data is in long format, to get it into wide format first make sure that each aphiaID (query), and
#' rank (kingdom/family ect) only has one name (eg agryrosomus) associated with it. Basically remove any
#' duplicates.
#' Then convert it to wide format and rename query as aphiaid.
#' Get a dataframe of the unique aphiaid's with their associated taxon names from the original dataset, removing
#' any species that don't have an aphiaid.
#' Join the WORMS taxon table to the dataframe.
#'
#' @param DF The dataframe to run the function on
#' @return Returns a table of taxon information for each taxon records in the dataset
#'
#'@importFrom taxize  classification
#'@importFrom magrittr  %>%
#'@importFrom dplyr select
#'@importFrom dplyr group_by
#'@importFrom dplyr distinct
#'@importFrom tidyr pivot_wider
#'@importFrom dplyr rename
#'@importFrom dplyr left_join
#'@importFrom dplyr rename_all
#'
#' @examples
#' # example code
#'  fm_taxon <- make_dim_taxon(taxon_new)
#'
#' @export
make_dim_taxon <- function(DF){

  dta_tax <- taxize::classification(unique(DF$aphiaid), db = "worms") %>%
    rbind() %>%
    dplyr::select(-id) %>%
    dplyr::group_by(query, rank) %>%
    dplyr::distinct(name) %>%
    tidyr::pivot_wider(id_cols = query, names_from = rank, values_from = name) %>%
    dplyr::rename(aphiaid = query) %>%
    dplyr::left_join(taxon_new) %>%
    dplyr::rename_all(., .funs = tolower)%>%
    dplyr::distinct() # not essential, just a precaution


}




