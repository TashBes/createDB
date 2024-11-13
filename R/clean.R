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
#'@importFrom dplyr  mutate
#'@importFrom stringr  str_squish
#'@importFrom stringr  str_replace_all
#'@importFrom stringr  str_detect
#'@importFrom Hmisc  capitalize
#'@importFrom magrittr  %>%
#'@importFrom rlang :=
#'
#' @examples
#' ## example code
#' # clean_names(species_names, scientific_name)
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




##
#'Restrict locations to with the eez
#'
#' This function test if locations fall within an area, with an optional buffer
#' and drops all other values.
#'
#' @param DF The dataframe to run the function on
#' @param LON The column with the longitude values (as decimals). Should be a
#' character in inverted commas.
#' @param LAT The column with the latitude values (as decimals). Should be a
#' character in inverted commas.
#' @param AREA The shape file you want to crop your data to. Should be the name
#' of the shape file plus the file extension.
#' @param BUFFER A numeric variable of the buffer zone in meters to be used
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
#' ## example code
#' # clean_area(species_locations, "longitude", "Latitude", "EEZ_South_Africa_buffered_beyond_allEEZversions1.shp", 2000)
#' @export
clean_area <- function(DF,LON, LAT, AREA, BUFFER) {


  eez <- st_read((dir(path = ".", AREA, full.names = T, recursive = T)[1]))

  map <- DF %>%
    sf::st_as_sf(coords = c({{LON}},{{LAT}}))%>%
    sf::st_set_crs(4326) %>%
    sf::st_transform(sf::st_crs(eez))

  ##buffer the eez
  buff <- sf::st_buffer(eez, BUFFER)
  int <- sf::st_intersection(buff, map)

  ##Remove inland locations

  inland_loc <- int %>%
#    dplyr::select(-c(OBJECTID, PROVINCE, Source, Shape_Leng, Shape_Area))%>%
    sf::st_transform(sf::st_crs(4326)) %>%
    dplyr::mutate({{LON}} := sf::st_coordinates(.)[,1],
           {{LAT}} := sf::st_coordinates(.)[,2]) %>%
    sf::st_drop_geometry()%>%  ## Drop the geometry column to make it a normal dataframe again
    dplyr::left_join({{DF}})

}



