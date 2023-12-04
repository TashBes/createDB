##
#'Clean the most generic errors in species names
#'
#' This function fixes the most common errors found in species names,
#' mainly being:
#' removing spaces at the begining or ends of the words,
#' removing any numbers or dots,
#' removing any instances of spp or sp,
#' removing any punctuation,
#' capitalising the first word.
#'
#' @param DF The dataframe to run the function on
#' @param SP_COL The column with the species names
#' @return Returns the dataframe with the cleans species name
#' in the original column
#'
#'@importFrom dplyr  filter
#'@importFrom magrittr  %>%
#'@importFrom rlang :=
#'
#' @examples
#' # example code
#'  clean_names(species_names, scientific_name)
#'
#' @export
clean_names <- function(DF, SP_COL) {

  DF %>%   #remove any spaces at the beginning of words
    dplyr::mutate({{SP_COL}} := stringr::str_squish({{SP_COL}})) %>%          #remove spaces in the words
    dplyr::mutate({{SP_COL}} := gsub('[.]|[0-9]','',{{SP_COL}})) %>% #remove numbers and dots
    dplyr::mutate({{SP_COL}} := stringr::str_squish({{SP_COL}})) %>%          #remove spaces in the words
    dplyr::mutate({{SP_COL}} := gsub('spp$','', {{SP_COL}})) %>%    #remove any spp
    dplyr::mutate({{SP_COL}} := stringr::str_squish({{SP_COL}})) %>%          #remove spaces in the words
    dplyr::mutate({{SP_COL}} := gsub('sp$','', {{SP_COL}})) %>%     #remove any sp
    dplyr::mutate({{SP_COL}} := stringr::str_squish({{SP_COL}})) %>%          #remove spaces in the words
    dplyr::mutate({{SP_COL}} := stringr::str_replace_all({{SP_COL}},"[[:punct:]]", "")) %>% #remove any punctuation
    dplyr::mutate({{SP_COL}} := stringr::str_squish({{SP_COL}})) %>%          #remove spaces in the words
    dplyr::mutate({{SP_COL}} := gsub('d$', "dae", {{SP_COL}}))%>%
    dplyr::mutate({{SP_COL}} := Hmisc::capitalize({{SP_COL}})) %>%          #capitalise the first word
    dplyr::filter(!is.na({{SP_COL}})) %>%
    dplyr::filter(!stringr::str_detect({{SP_COL}},"Teleoste|unknown|Unknown|fish|Fish"))
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
#'@importFrom dplyr  filter
#'@importFrom dplyr  ungroup
#'@importFrom dplyr  distinct
#'@importFrom dplyr  arrange
#'@importFrom dplyr  pull
#'@importFrom dplyr  mutate
#'@importFrom dplyr  left_join
#'@importFrom taxize  get_wormsid
#'@importFrom magrittr  %>%
#'
#' @examples
#' # example code
#'  clean_aphiaid(species_names, scientific_name, "FM")
#' @export
clean_aphiaid <- function (DF, SP_COL, DF_ABBR) {

  ##get a list of distinct species names
  taxon <- DF %>%
    dplyr::ungroup() %>%
    dplyr::distinct({{SP_COL}}) %>%
    dplyr::arrange(dplyr::desc({{SP_COL}}))

  sp_name <- dplyr::pull(taxon, {{SP_COL}})

  ## Get the matching ahpia ID's for each taxon in the dataset from the
  ## online worms directory. This function only returns an ID if the name
  ## is correct and classified as marine. If there is ambiguity the user
  ## is prompted to chose the WORMS name to take. As a rule chose the name
  ## that says accepted. If neither are accepted or both are choose the
  ## first name.

  ##retrieve the aphia ID's from the WORMS website
  worms <- taxize::get_wormsid(sp_name, ask = T, marine_only = T)

  ## isolate the species not found so that we have a record of them
  missing <- taxon %>%
    dplyr::filter(is.na(worms)) %>%
    dplyr::mutate(dataset = {{DF_ABBR}})

  assign("missing", missing, envir=.GlobalEnv)

  aphiaid <- taxon %>%
    dplyr::mutate(aphiaid = worms) %>%
    dplyr::mutate(aphiaid = as.character(aphiaid))

  DF <- DF %>%
    dplyr::left_join(aphiaid) %>%
    dplyr::filter(!is.na(aphiaid))

}


##
#'Restrict locations to with the eez
#'
#' This function test if locations fall within the marine EEZ with a buffer
#' of 2km, and drops all other values.
#'
#' @param DF The dataframe to run the function on
#' @param LON The column with the longitude values (as decimals). Should be a
#' character in inverted commas.
#' @param LAT The column with the latitude values (as decimals). Should be a
#' character in inverted commas.
#' @return Returns the dataframe with only the values that fall with the eez
#'
#'@importFrom sf  st_read
#'@importFrom sf  st_as_sf
#'@importFrom sf  st_set_crs
#'@importFrom sf  st_transform
#'@importFrom sf  st_crs
#'@importFrom sf  st_buffer
#'@importFrom sf  st_intersection
#'@importFrom sf  st_drop_geometry
#'@importFrom sf  st_coordinates
#'@importFrom dplyr  mutate
#'@importFrom dplyr  left_join
#'@importFrom magrittr  %>%
#'
#' @examples
#' # example code
#'  clean_area(species_locations, "longitude", "Latitude")
#' @export
clean_area <- function(DF,LON, LAT) {


  eez <- sf::st_read("data/EEZ_South_Africa_buffered_beyond_allEEZversions1.shp")

  map <- DF %>%
    sf::st_as_sf(coords = c({{LON}},{{LAT}}))%>%
    sf::st_set_crs(4326) %>%
    sf::st_transform(sf::st_crs(eez))

  ##buffer the eez by 20km
  buff <- sf::st_buffer(eez, 2000)
  int <- sf::st_intersection(buff, map)

  ##Remove inland locations

  inland_loc <- int %>%
    dplyr::select(-c(OBJECTID, PROVINCE, Source, Shape_Leng, Shape_Area))%>%
    sf::st_transform(sf::st_crs(4326)) %>%
    dplyr::mutate({{LON}} := sf::st_coordinates(.)[,1],
           {{LAT}} := sf::st_coordinates(.)[,2]) %>%
    sf::st_drop_geometry()%>%  ## Drop the geometry column to make it a normal dataframe again
    dplyr::left_join({{DF}})

}



