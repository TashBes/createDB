clean_names <- function(DF, SP_COL) {

  DF %>%   #remove any spaces at the beginning of words
    mutate({{SP_COL}} := str_squish({{SP_COL}})) %>%          #remove spaces in the words
    mutate({{SP_COL}} := gsub('[.]|[0-9]','',{{SP_COL}})) %>% #remove numbers and dots
    mutate({{SP_COL}} := str_squish({{SP_COL}})) %>%          #remove spaces in the words
    mutate({{SP_COL}} := gsub('spp$','', {{SP_COL}})) %>%    #remove any spp
    mutate({{SP_COL}} := str_squish({{SP_COL}})) %>%          #remove spaces in the words
    mutate({{SP_COL}} := gsub('sp$','', {{SP_COL}})) %>%     #remove any sp
    mutate({{SP_COL}} := str_squish({{SP_COL}})) %>%          #remove spaces in the words
    mutate({{SP_COL}} := str_replace_all({{SP_COL}},"[[:punct:]]", "")) %>% #remove any punctuation
    mutate({{SP_COL}} := str_squish({{SP_COL}})) %>%          #remove spaces in the words
    mutate({{SP_COL}} := gsub('d$', "dae", {{SP_COL}}))%>%
    mutate({{SP_COL}} := Hmisc::capitalize({{SP_COL}})) %>%          #capitalise the first word
    filter(!is.na({{SP_COL}})) %>%
    filter(!str_detect({{SP_COL}},"Teleoste|unknown|Unknown|fish|Fish"))
}


clean_aphiaid <- function (DF, SP_COL, DF_ABBR) {

  ##get a list of distinct species names
  taxon <- DF %>%
    ungroup() %>%
    distinct({{SP_COL}}) %>%
    arrange(desc({{SP_COL}}))

  sp_name <- pull(taxon, {{SP_COL}})

  ## Get the matching ahpia ID's for each taxon in the dataset from the
  ## online worms directory. This function only returns an ID if the name
  ## is correct and classified as marine. If there is ambiguity the user
  ## is prompted to chose the WORMS name to take. As a rule chose the name
  ## that says accepted. If neither are accepted or both are choose the
  ## first name.

  ##retrieve the aphia ID's from the WORMS website
  worms <- get_wormsid(sp_name, ask = T, marine_only = T)

  ## isolate the species not found so that we have a record of them
  missing <- taxon %>%
    filter(is.na(worms)) %>%
    mutate(dataset = {{DF_ABBR}})

  assign("missing", missing, envir=.GlobalEnv)

  aphiaid <- taxon %>%
    mutate(aphiaid = worms) %>%
    mutate(aphiaid = as.character(aphiaid))

  DF <- DF %>%
    left_join(aphiaid) %>%
    filter(!is.na(aphiaid))

}



clean_area <- function(DF,LON, LAT) {


  eez <- st_read("data/EEZ_South_Africa_buffered_beyond_allEEZversions1.shp")

  map <- DF %>%
    st_as_sf(coords = c({{LON}},{{LAT}}))%>%
    st_set_crs(4326) %>%
    st_transform(st_crs(eez))

  ##buffer the eez by 20km
  buff <- st_buffer(eez, 2000)
  int <- st_intersection(buff, map)

  ##Remove inland locations

  inland_loc <- int %>%
    select(-c(OBJECTID, PROVINCE, Source, Shape_Leng, Shape_Area))%>%
    st_transform(st_crs(4326)) %>%
    mutate({{LON}} := sf::st_coordinates(.)[,1],
           {{LAT}} := sf::st_coordinates(.)[,2]) %>%
    st_drop_geometry()%>%  ## Drop the geometry column to make it a normal dataframe again
    left_join({{DF}})

}

