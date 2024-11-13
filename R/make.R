

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
#' ## example code
#' # date_tbl <- make_dim_date
#'
#' @export
make_dim_date <- function()  {

  ##Make a date range

  ## defining start date
  start_date <- as.Date("1750/01/01")

  ## defining end date
  end_date <- as.Date("2050/01/01")

  ## generating range of dates
  date_full_date <- seq(start_date, end_date,"days")

  #add range to table
  day_tbl <- data.frame(date_full_date)

  ## add dim_date table columns and populate them using the date range values.
  ## important to known that for date_weekday_weekend true indicates a weekend
  ## and for date_last_date_of_month_indicator true indicates it is the last
  ## day of the month.

  dim_date_day <- day_tbl %>%
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


  dim_date_year <- dim_date_day %>%
    dplyr::select(date_calendar_year_number) %>%
    dplyr::distinct() %>%
    dplyr::bind_rows(dim_date_day)

  dim_date_month <- dim_date_day %>%
    dplyr::select(date_month_number) %>%
    dplyr::distinct() %>%
    dplyr::bind_rows(dim_date_year)

  dim_date <- dim_date_day %>%
    dplyr::select(date_calendar_year_number, date_month_number) %>%
    dplyr::distinct() %>%
    dplyr::bind_rows(dim_date_month)

  # rm(dim_date_day)
  # rm(dim_date_month)
  # rm(dim_date_year)

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
#' ## example code
#' # fm_project <- make_dim_project("FishMaster", "FM", "Colin Attwood", "Species records from scientifi publications")
#'
#' @export
make_dim_project<- function(PROJ_NAME, PROJ_SOURCE, PROJ_MAN, PROJ_DESC, PROJ_TIME, PROJ_RECRD, PROJ_CIT, PROJ_URL, PROJ_DR, PROJ_GUIDNC, PROJ_RECEIV) {

  project_title <- c(PROJ_NAME)

  project_source <- c(PROJ_ABBR)

  project_manager <- c(PROJ_MAN)

  project_description <- c(PROJ_DESC)

  project_time_period <- c(PROJ_TIME)

  project_records <- c(PROJ_RECRD)

  project_citation <- c(PROJ_CIT)

  project_online_repository <- c(PROJ_URL)

  project_data_restriction <- c(PROJ_DR)

  project_data_guidance <- c(PROJ_GUIDNC)

  project_date_recieved <- c(PROJ_RECEIV)



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
#' ## example code
#' # taxon <- make_dis_spp(fm_fish_abundance_clean, Species)
#'
#' @export
make_dis_spp <- function(DF, SP_COL)  {

  taxon <- DF %>%
    dplyr::distinct({{SP_COL}}) %>%
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
#' ## example code
#' # taxon_new <- make_rmv_dups(taxon, "dim_taxon")
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


###
#'get aphia ID's
#'
#' This function matches species names in a dataframe to worms id numbers
#'
#' @param DF The dataframe to run the function on
#' @param SP_COL The column with the species names
#' @param DF_ABBR Dataset abbreviation. Must be a character in inverted commas
#' @return Returns the dataframe with the aphia ID's as a new column
#'
#'@importFrom dplyr  pull
#'@importFrom dplyr bind_rows
#'@importFrom magrittr  %>%
#'@importFrom jsonlite fromJSON
#'
#' @examples
#' ## example code
#' # clean_aphiaid(species_names, scientific_name, "FM")
#' @export
make_aphiaid <- function (DF, SP_COL) {

  sp_name <- dplyr::pull(DF, {{SP_COL}})

  ## Get the matching ahpia ID's for each taxon in the dataset from the
  ## online worms directory.

  worms = data.frame()

  for (i in sp_name) {
    #Convert the sp_name to a valid REST-url
    urlNamesPart <- ""
    for (index in 1:length(i)) {
      urlNamesPart <- sprintf("%s&scientificnames[]=%s", urlNamesPart, i[index]);
    }

    #The url can contain special characters that need to be converted
    urlNamesPart <- URLencode(urlNamesPart)

    #The dyanmic build of the URL causes an obsolete '&' at the beginning of the string, so remove it
    urlNamesPart <- substring(urlNamesPart, 2)

    #Build the final REST-url
    url <- sprintf("http://www.marinespecies.org/rest/AphiaRecordsByMatchNames?%s", urlNamesPart);

    #Get the actual data from the URL
    matches <- jsonlite::fromJSON(url)

    #Handle the data (each requested name has an list of results)
    for (matchesindex in 1:length(i)) {
      #Get the results for the current index
      currentResultList = matches[[matchesindex]]

      #Get the number of list entries for the first column
      numberOfResults <- length(currentResultList[[1]])

      #Handle empty data due to no matches found
      if (is.na(currentResultList[[1]][[1]])) {
        numberOfResults <- 0
      }
      print(sprintf("%d Result(s) found for %s", numberOfResults, i[matchesindex]))
      if (numberOfResults > 0) {
        for (listentry in 1:numberOfResults) {
          print(sprintf("ID: %d, SCIENTIFICNAME: %s, MATCH_TYPE: %s",
                        currentResultList[["AphiaID"]][listentry],
                        currentResultList[["scientificname"]][listentry],
                        currentResultList[["match_type"]][listentry]
          ));
        }
      }
    }

    output <- matches %>%
      dplyr::bind_rows()

    worms = rbind(worms, output)
    assign("worms", worms, envir=.GlobalEnv)

  }
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
#'@importFrom magrittr  %>%
#'@importFrom dplyr bind_rows
#'@importFrom tidyr pivot_wider
#'@importFrom worrms wm_classification
#'
#' @examples
#' ## example code
#' # fm_taxon <- make_dim_taxon(taxon_new)
#'
#' @export
make_dim_taxon <- function(AphiaID){

  valid_aphia <- data.frame()

  for (i in AphiaID){
    test <- worrms::wm_classification(i) %>%
      mutate(AphiaID = i) %>%
      tidyr::pivot_wider(names_from = rank, values_from = scientificname)

    valid_aphia = dplyr::bind_rows(valid_aphia, test)

  }

}
