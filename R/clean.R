##
#'Clean the most generic errors in species names
#'
#' This function fixs the most common errors found in species names,
#' mainly being:
#' removing spaces at the begining or ends of the words,
#' removing any numbers or dots,
#' removing any instances of spp or sp
#' removing any punctuation
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
#' @example clean_names(species_names, scientific_name)
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
