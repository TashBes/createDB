#' Retrieve the taxonomic hierarchy for a given taxon ID.
#'
#' @export
#' @param sci_id Vector of taxa names (character) or IDs (character or numeric)
#' to query. For `db = "eol"`, EOL expects you to pass it a taxon id, called
#' `eolid` in the output of [get_eolid()].
#' @param db character; database to query. either `ncbi`, `itis`, `eol`,
#' `tropicos`, `gbif`, `nbn`, `worms`, `natserv`, `bold`, `wiki`, or `pow`.
#' Note that each taxonomic data source has, their own identifiers, so that
#' if you provide the wrong `db` value for the identifier you could get a
#' result, but it will likely be wrong (not what you were expecting). If using
#' ncbi, and/or tropicos, we recommend getting an API key; see
#' [taxize-authentication]
#' @param id character; identifiers, returned by [get_tsn()], [get_uid()],
#' [get_eolid()], [get_tpsid()], [get_gbifid()], [get_tolid()],
#' [get_wormsid()], [get_natservid()], [get_wormsid()], [get_wiki()],
#' [get_pow()]
#' @param callopts Curl options passed on to [crul::verb-GET]
#' @param ... For `classification`: other arguments passed to [get_tsn()],
#' [get_uid()], [get_eolid()], [get_tpsid()], [get_gbifid()],
#' [get_wormsid()], [get_natservid()], [get_wormsid()], [get_wiki()],
#' [get_pow()]. For `rbind.classification` and `cbind.classification`: one or
#' more objects of class `classification`
#' @param return_id (logical) If `TRUE` (default), return the taxon id
#' as well as the name and rank of taxa in the lineage returned.
#' Ignored for natserv as they don't return IDs in their taxonomic
#' classification data.
#' @param rows (numeric) Any number from 1 to infinity. If the default NA,
#' all rows are considered. Note that this parameter is ignored if you pass
#' in a taxonomic id instead of a name of class character.
#' @param batch_size (numeric) For NCBI queries, specify the number of IDs to
#'   lookup for each query.
#' @param max_tries (numeric) For NCBI queries, the number of times a particular
#'   query will be attempted, assuming the first does not work.
#' @param x Deprecated, see `sci_id`
#'
#' @return A named list of data.frames with the taxonomic classification of
#'    every supplied taxa.
#' @details If IDs are supplied directly (not from the `get_*` functions)
#' you must specify the type of ID. There is a timeout of 1/3 seconds between
#' queries to NCBI.
#'
#' BEWARE: Right now, NBN doesn't return the queried taxon in the
#' classification. But you can attach it yourself quite easily of course.
#' This behavior is different from the other data sources.
#'
#' @seealso [get_tsn()], [get_uid()], [get_eolid()],
#'    [get_tpsid()], [get_gbifid()], [get_wormsid()], [get_natservid()],
#'    [get_boldid()], [get_wiki()], [get_pow()]
#'
#' @section Lots of results:
#' It may happen sometimes that you get more results back from your query
#' than will show in the data.frame on screen. Our advice is to refine your
#' query in those cases. On a data source basis we can attempt to help
#' make it easier to refine queries, whether it be with the data provider
#' (unlikely to happen), or in the code in this package (more likely) -
#' let us know if you run into too many results problem and we'll see what
#' we can do.
#'
#' @section Authentication:
#' See [taxize-authentication]
#'
#' @section EOL:
#' EOL does not have very good failure behavior. For example, if you submit
#' an ID that does not exist they'll return a 500 HTTP error, which is
#' not an appropriate error; it's probably that that ID does not exist
#' in their database, but we can't know for sure. Isn't that fun?
#'
#' @section NCBI Rate limits:
#' In case you run into NCBI errors due to your rate limit being exceeded, see
#' [taxize_options()], where you can set `ncbi_sleep`.
#'
#' @section HTTP version for NCBI requests:
#' We hard code `http_version = 2L` to use HTTP/1.1 in HTTP requests to
#' the Entrez API. See `curl::curl_symbols('CURL_HTTP_VERSION')`
#'
#' @examples \dontrun{
#' # Plug in taxon IDs
#' classification(9606, db = 'ncbi')
#' classification(c(9606, 55062), db = 'ncbi')
#' classification(129313, db = 'itis')
#' classification(6985636, db = 'eol')
#' classification(126436, db = 'worms')
#' classification('Helianthus annuus', db = 'pow')
#' classification('Helianthus', db = 'pow')
#' classification('Asteraceae', db = 'pow')
#' classification("134717", db = 'natserv')
#' classification(c(2704179, 6162875, 8286319, 2441175, 731), db = 'gbif')
#' classification(25509881, db = 'tropicos')
#' classification("NBNSYS0000004786", db = 'nbn')
#' classification(as.nbnid("NBNSYS0000004786"), db = 'nbn')
#' classification(3930798, db = 'tol')
#'
#' ## works the same if IDs are in class character
#' classification(c("2704179", "2441176"), db = 'gbif')
#' classification("Agapostemon", db = "bold")
#'
#' # wikispecies
#' classification("Malus domestica", db = "wiki")
#' classification("Pinus contorta", db = "wiki")
#' classification("Pinus contorta", db = "wiki", wiki_site = "commons")
#' classification("Pinus contorta", db = "wiki", wiki_site = "pedia")
#' classification("Pinus contorta", db = "wiki", wiki_site = "pedia",
#'   wiki = "fr")
#'
#' classification(get_wiki("Malus domestica", "commons"))
#' classification(get_wiki("Malus domestica", "species"))
#' classification(c("Pinus contorta", "Malus domestica"), db = "wiki")
#'
#' # Plug in taxon names
#' ## in this case, we use get_*() fxns internally to first get taxon IDs
#' classification("Oncorhynchus mykiss", db = "eol")
#' classification(c("Chironomus riparius", "aaa vva"), db = 'ncbi')
#' classification(c("Chironomus riparius", "aaa vva"), db = 'ncbi',
#'   messages=FALSE)
#' classification(c("Chironomus riparius", "aaa vva"), db = 'itis')
#' classification(c("Chironomus riparius", "aaa vva"), db = 'itis',
#'   messages=FALSE)
#' classification(c("Chironomus riparius", "aaa vva"), db = 'eol')
#' classification("Alopias vulpinus", db = 'nbn')
#' classification('Gadus morhua', db = 'worms')
#' classification('Aquila chrysaetos', db = 'natserv')
#' classification('Gadus morhua', db = 'natserv')
#' classification('Pomatomus saltatrix', db = 'natserv')
#' classification('Aquila chrysaetos', db = 'natserv')
#' classification(c("Chironomus riparius", "asdfasdfsfdfsd"), db = 'gbif')
#' classification("Chironomus", db = 'tol')
#' classification("Poa annua", db = 'tropicos')
#'
#' # Use methods for get_uid, get_tsn, get_eolid, get_tpsid
#' classification(get_uid(c("Chironomus riparius", "Puma concolor")))
#'
#' classification(get_uid(c("Chironomus riparius", "aaa vva")))
#' classification(get_tsn(c("Chironomus riparius", "aaa vva")))
#' classification(get_tsn(c("Chironomus riparius", "aaa vva"),
#'   messages = FALSE))
#' classification(get_eolid(c("Chironomus riparius", "aaa vva")))
#' classification(get_tpsid(c("Poa annua", "aaa vva")))
#' classification(get_gbifid(c("Poa annua", "Bison bison")))
#'
#' # Pass many ids from class "ids"
#' (out <- get_ids("Puma concolor", db = c('ncbi','gbif')))
#' (cl <- classification(out))
#'
#' # Bind width-wise from class classification_ids
#' cbind(cl)
#'
#' # Bind length-wise
#' rbind(cl)
#'
#' # Many names to get_ids
#' (out <- get_ids(c("Puma concolor","Accipiter striatus"),
#'   db = c('ncbi','itis')))
#' (cl <- classification(out))
#' rbind(cl)
#' ## cbind with so many names results in some messy data
#' cbind(cl)
#' ## so you can turn off return_id
#' cbind( classification(out, return_id=FALSE) )
#'
#' (cl_uid <- classification(get_uid(c("Puma concolor",
#'   "Accipiter striatus")), return_id=FALSE))
#' rbind(cl_uid)
#' cbind(cl_uid)
#' ## cbind works a bit odd when there are lots of ranks without names
#' (cl_uid <- classification(get_uid(c("Puma concolor","Accipiter striatus")),
#'   return_id=TRUE))
#' cbind(cl_uid)
#'
#' (cl_tsn <- classification(get_tsn(c("Puma concolor","Accipiter striatus"))))
#' rbind(cl_tsn)
#' cbind(cl_tsn)
#'
#' (tsns <- get_tsn(c("Puma concolor","Accipiter striatus")))
#' (cl_tsns <- classification(tsns))
#' cbind(cl_tsns)
#'
#' # NBN data
#' (res <- classification(c("Alopias vulpinus","Pinus sylvestris"),
#'   db = 'nbn'))
#' rbind(res)
#' cbind(res)
#'
#' # Return taxonomic IDs
#' ## the return_id parameter is logical, and you can turn it on or off.
#' ## It's TRUE by default
#' classification(c("Alopias vulpinus","Pinus sylvestris"), db = 'ncbi',
#'   return_id = TRUE)
#' classification(c("Alopias vulpinus","Pinus sylvestris"), db = 'ncbi',
#'   return_id = FALSE)
#'
#' # Use rows parameter to select certain
#' classification('Poa annua', db = 'tropicos')
#' classification('Poa annua', db = 'tropicos', rows=1:4)
#' classification('Poa annua', db = 'tropicos', rows=1)
#' classification('Poa annua', db = 'tropicos', rows=6)
#'
#' # Queries of many IDs are processed in batches for NCBI
#' ids <- c("13083", "2650392", "1547764", "230054", "353934", "656984",
#' "271789", "126272", "184644", "73213", "662816", "1161803", "1239353",
#' "59420", "665675", "866969", "1091219", "1431218", "1471898",
#' "864321", "251768", "2486276", "2068772", "1825808", "2006532",
#' "128287", "1195738", "1084683", "1886461", "508296", "377247",
#' "1489665", "329325", "219243", "1176946", "339893", "197933",
#' "174510", "1704048", "212897", "154842", "1239280", "260135",
#' "405735", "1566412", "2083462", "651348", "983204", "165380",
#' "2338856", "2068760", "167262", "34229", "1213340", "478939",
#' "1933585", "49951", "1277794", "1671089", "1502538", "362355",
#' "746473", "242879", "158219", "313664", "2093188", "1541232",
#' "584742", "1331091", "147639", "284492", "75642", "1412882",
#' "391782", "1406855", "434506", "2053357", "217315", "1444328",
#' "329249", "2294004", "84942", "324458", "538247", "69452", "49170",
#' "1993845", "261646", "127633", "228146", "1420004", "1629772",
#' "577055", "697062", "231660", "648380", "554953", "746496", "2602969")
#' result <- classification(ids, db = 'ncbi')
#' }
#'
#' @examples \dontrun{
#' # Fails without db param set
#' # classification(315576)
#' }
classification <- function(...){
  UseMethod("classification")
}

#' @export
#' @rdname classification
classification.default <- function(sci_id, db = NULL, callopts = list(),
                                   return_id = TRUE, rows = NA, x = NULL, ...) {
  nstop(db)
  if (!is.null(x)) {
    lifecycle::deprecate_warn(when = "v0.9.97", what = "classification(x)", with = "classification(sci_id)")
    sci_id <- x
  }

  switch(
    db,
    itis = {
      id <- process_ids(sci_id, db, get_tsn, rows = rows, ...)
      stats::setNames(classification(id, return_id = return_id, ...), sci_id)
    },
    ncbi = {
      id <- process_ids(sci_id, db, get_uid, rows = rows, ...)
      stats::setNames(classification(id, callopts = callopts,
                                     return_id = return_id, ...), sci_id)
    },
    eol = {
      id <- process_ids(sci_id, db, get_eolid, rows = rows, ...)
      stats::setNames(classification(id, callopts = callopts,
                                     return_id = return_id, ...), sci_id)
    },
    tropicos = {
      id <- process_ids(sci_id, db, get_tpsid, rows = rows, ...)
      stats::setNames(classification(id, callopts = callopts,
                                     return_id = return_id, ...), sci_id)
    },
    gbif = {
      id <- process_ids(sci_id, db, get_gbifid, rows = rows, ...)
      stats::setNames(classification(id, callopts = callopts,
                                     return_id = return_id, ...), sci_id)
    },
    nbn = {
      id <- process_ids(sci_id, db, get_nbnid, rows = rows, ...)
      stats::setNames(classification(id, callopts = callopts,
                                     return_id = return_id, ...), sci_id)
    },
    tol = {
      id <- process_ids(sci_id, db, get_tolid, rows = rows, ...)
      stats::setNames(classification(id, callopts = callopts,
                                     return_id = return_id, ...), sci_id)
    },
    worms = {
      id <- process_ids(sci_id, db, get_wormsid, rows = rows, ...)
      stats::setNames(classification(id, callopts = callopts,
                                     return_id = return_id, ...), sci_id)
    },
    natserv = {
      id <- process_ids(sci_id, db, get_natservid, rows = rows, ...)
      stats::setNames(classification(id, callopts = callopts,
                                     return_id = return_id, ...), sci_id)
    },
    bold = {
      id <- process_ids(sci_id, db, get_boldid, rows = rows, ...)
      stats::setNames(classification(id, callopts = callopts,
                                     return_id = return_id, ...), sci_id)
    },
    wiki = {
      id <- process_ids(sci_id, db, get_wiki, rows = rows, ...)
      stats::setNames(classification(id, callopts = callopts,
                                     return_id = return_id, ...), sci_id)
    },
    pow = {
      id <- process_ids(sci_id, db, get_pow, rows = rows, ...)
      stats::setNames(classification(id, callopts = callopts,
                                     return_id = return_id, ...), sci_id)
    },
    stop("the provided db value was not recognised", call. = FALSE)
  )
}

process_ids <- function(input, db, fxn, ...){
  g <- tryCatch(as.numeric(as.character(input)), warning = function(e) e)
  if (
    inherits(g, "numeric") || # all others
    is.character(input) && all(grepl("N[HB]", input)) || # NBN
    is.character(input) && all(grepl("urn:lsid", input)) # POW
  ) {
    as_fxn <- switch(db,
                     itis = as.tsn,
                     ncbi = as.uid,
                     eol = as.eolid,
                     tropicos = as.tpsid,
                     gbif = as.gbifid,
                     nbn = as.nbnid,
                     tol = as.tolid,
                     worms = as.wormsid,
                     natserv = as.natservid,
                     bold = as.boldid,
                     wiki = as.wiki,
                     pow = as.pow)
    as_fxn(input, check = FALSE)
  } else {
    eval(fxn)(input, ...)
  }
}

#' @export
#' @rdname classification
classification.tsn <- function(id, return_id = TRUE, ...) {
  warn_db(list(...), "itis")
  fun <- function(x) {
    # return NA if NA is supplied
    if (is.na(x)) {
      out <- NA
    } else {
      out <- ritis::hierarchy_full(as.character(x), wt = "json", raw = FALSE)
      if (NROW(out) < 1) return(NA)
      # make normal data.frame
      out <- data.frame(out, stringsAsFactors = FALSE)
      # remove overhang
      out <- out[1:which(out$tsn == x), c('taxonname', 'rankname', 'tsn')]
      names(out) <- c('name', 'rank', 'id')
      # Optionally return tsn of lineage
      if (!return_id) out <- out[, c('name', 'rank')]
      out$rank <- tolower(out$rank)
    }
    return(out)
  }
  out <- lapply(id, fun)
  names(out) <- id
  structure(out, class = 'classification', db = 'itis')
}

#' @export
#' @rdname classification
classification.uid <- function(id, callopts = list(), return_id = TRUE,
                               batch_size = 50, max_tries = 3, ...) {

  warn_db(list(...), "ncbi")
  fun <- function(x, callopts) {
    key <- getkey(NULL, service="entrez")

    query_ncbi <- function(ids) {
      query <- tc(list(db = "taxonomy", ID = paste0(ids, collapse = ','),
                       api_key = key))
      cli <- crul::HttpClient$new(url = ncbi_base(),
                                  opts = c(http_version = 2L, callopts))
      success <- FALSE
      tries <- 1
      while (success == FALSE && tries <= max_tries) {
        res <- cli$get("entrez/eutils/efetch.fcgi", query = query)
        try({
          res$raise_for_status()
          tt <- res$parse("UTF-8")
          ttp <- xml2::read_xml(tt)
          out <- lapply(xml2::xml_find_all(ttp, '//TaxaSet/Taxon'),
                        function(tax_node) {
                          lin <- data.frame(
                            name = xml_text_all(tax_node,
                                                ".//LineageEx/Taxon/ScientificName"),
                            rank = xml_text_all(tax_node, ".//LineageEx/Taxon/Rank"),
                            id = xml_text_all(tax_node, ".//LineageEx/Taxon/TaxId"),
                            stringsAsFactors = FALSE)
                          targ_tax <- data.frame(
                            name = xml_text_all(tax_node, "./ScientificName"),
                            rank = xml_text_all(tax_node, "./Rank"),
                            id = xml_text_all(tax_node, "./TaxId"),
                            stringsAsFactors = FALSE)
                          rbind(lin, targ_tax)
                        })
          # Is not directly below root and no lineage info
          parent_id <- xml_text_all(ttp, "//TaxaSet/Taxon/ParentTaxId") %||% ""
          out[vapply(out, NROW, numeric(1)) == 0 & parent_id != "1"] <- NA
          # Add NA where the taxon ID was not found
          names(out) <- xml_text(xml2::xml_find_all(ttp,
                                                    '//TaxaSet/Taxon/TaxId'))
          success <- ! grepl(tt, pattern = 'error', ignore.case = TRUE)
        }, silent=FALSE)
        tries <- tries + 1
        # NCBI limits requests to three per second without key or 10 per
        # second with key
        ncbi_rate_limit_pause(key)
        # Wait longer if query failed
        if (success == FALSE) {
          Sys.sleep(1)
        }
      }
      # Return NA if cannot get information
      if (!success) {
        out <- rep(list(NA), length(ids))
        warning(call. = FALSE, 'Giving up on query after ',
                max_tries, ' tries. NAs will be returned.')
      }
      return(out)
    }
    # return NA if NA is supplied
    out <- rep(list(NA), length(x))
    out[! is.na(x)] <- query_ncbi(x[! is.na(x)])
    # Optionally return taxon id of lineage taxa
    if (!return_id) {
      out[! is.na(out)] <- lapply(out[! is.na(out)],
                                  function(o) o[, c('name', 'rank')])
    }
    # Return ranks in all lower case
    out[! is.na(out)] <- lapply(out[! is.na(out)], function(o) {
      o$rank <- tolower(o$rank)
      return(o)
    })
    return(out)
  }
  id <- as.character(id) # force to character
  id_chunks <- split(id, ceiling(seq_along(id)/batch_size))
  out <- lapply(id_chunks, fun, callopts = callopts)
  out <- unlist(out, recursive = FALSE)
  names(out) <- id
  structure(out, class = 'classification', db = 'ncbi')
}

#' @export
#' @rdname classification
classification.eolid <- function(id, callopts = list(), return_id = TRUE, ...) {
  warn_db(list(...), "eol")
  common_names = synonyms = NULL
  fun <- function(x){
    if (is.na(x)) {
      out <- NA
    } else {
      args <- tc(list(common_names = common_names, synonyms = synonyms))
      cli <- crul::HttpClient$new(url = 'https://eol.org', opts = callopts)
      tt <- cli$get(file.path('api/hierarchy_entries/1.0', paste0(x, ".json")),
                    query = args)
      tt$raise_for_status()
      res <- jsonlite::fromJSON(tt$parse("UTF-8"), FALSE)
      if (length(res$ancestors) == 0) {
        return(sprintf("No hierarchy information for %s", x))
      } else {
        fff <- lapply(res$ancestors, function(z) {
          z[sapply(z, is.null)] <- NA_character_
          data.frame(z, stringsAsFactors = FALSE)
        })
        out <- dt2df(fff, idcol = FALSE)[,c('scientificName',
                                            'taxonRank', 'taxonID')]
        # add querried taxon
        tr <- res$taxonRank
        out <- rbind(out,
                     c(res$scientificName, if ( is.null(tr) ) NA else tr, x))
        names(out) <- c('name', 'rank', 'id')
        # Optionally return id of lineage
        if (!return_id) out <- out[, c('name', 'rank')]
        out$rank <- tolower(out$rank)
        return(out)
      }
    }
  }
  out <- lapply(id, fun)
  names(out) <- id
  structure(out, class = 'classification', db = 'eol')
}

#' @export
#' @rdname classification
classification.tpsid <- function(id, callopts = list(), return_id = TRUE, ...) {
  warn_db(list(...), "tropicos")
  fun <- function(x, callopts){
    if (is.na(x)) {
      out <- NA
    } else {
      url <- sprintf('http://services.tropicos.org/Name/%s/HigherTaxa', x)
      key <- getkey(NULL, "TROPICOS_KEY")
      args <- tc(list(format = 'json', apikey = key))
      tt <- tax_GET(url, query = args, opts = callopts)
      if ("Error" %in% names(tt)) {
        out <- data.frame(ScientificName = NA, Rank = NA)
      } else {
        out <- tt[,c('ScientificName','Rank', 'NameId')]
      }
      names(out) <- c('name', 'rank', 'id')
      # Optionally return id of lineage
      if (!return_id) out <- out[, c('name', 'rank')]
      out$rank <- tolower(out$rank)
    }
    return(out)
  }
  out <- lapply(id, fun, callopts = callopts)
  names(out) <- id
  structure(out, class = 'classification', db = 'tropicos')
}

#' @export
#' @rdname classification
classification.gbifid <- function(id, callopts = list(),
                                  return_id = TRUE, ...) {

  warn_db(list(...), "gbif")
  fun <- function(x, callopts){
    if (is.na(x)) {
      out <- NA
    } else {
      out <- suppressWarnings(tryCatch(
        gbif_name_usage(key = x, callopts = callopts),
        error = function(e) e))
      if (inherits(out, "simpleError")) {
        NA
      } else {
        cls = c('kingdom','phylum','class', 'order','family','genus','species')
        df1 <- stats::setNames(nmdlst2df(out[names(out) %in% cls]),
                               c("rank", "name"))
        df2 <- stats::setNames(
          nmdlst2df(out[names(out) %in% paste0(cls, "Key")]),
          c(".id", "id"))
        df2$rank <- sub("Key", "", df2$.id)
        df2$.id <- NULL
        df <- merge(df1, df2, by = "rank")
        df$rank <- tolower(df$rank)
        # sort to make sure ranks are in correct descending order
        df <- df[order(vapply(df$rank, which_rank, 1)), ]
        # column order
        df <- data.frame(name = df$name, rank = df$rank, id = df$id,
                         stringsAsFactors = FALSE)
        # check if target taxon is sub-specific
        if (which_rank(tolower(out$rank)) > 37) {
          df <- rbind(df,
                      c(out$canonicalName, tolower(out$rank), out$key))
        }
        df$id <- as.integer(df$id)
        # Optionally return id of lineage
        if (!return_id) df[, c('name', 'rank')] else df
      }
    }
  }
  out <- lapply(id, fun, callopts = callopts)
  names(out) <- id
  structure(out, class = 'classification', db = 'gbif')
}

#' @export
#' @rdname classification
classification.nbnid <- function(id, callopts = list(),
                                 return_id = TRUE, ...) {

  warn_db(list(...), "nbn")
  fun <- function(x, callopts){
    if (is.na(x)) {
      out <- NA
    } else {
      out <- suppressWarnings(tryCatch(nbn_classification(id = x, callopts),
                                       error = function(e) e))
      if (inherits(out, "simpleError")) {
        NA
      } else {
        out <- out[ , c('scientificname', 'rank', 'guid')]
        names(out) <- c('name', 'rank', 'id')
        # Optionally return id of lineage
        if (!return_id) out <- out[, c('name', 'rank')]
        out$rank <- tolower(out$rank)
        return(out)
      }
    }
  }
  out <- lapply(id, fun, callopts = callopts)
  names(out) <- id
  structure(out, class = 'classification', db = 'nbn')
}

#' @export
#' @rdname classification
classification.tolid <- function(id, callopts = list(),
                                 return_id = TRUE, ...) {

  warn_db(list(...), "tol")
  fun <- function(x, callopts) {
    if (is.na(x)) {
      out <- NA
    } else {
      x <- as.numeric(x)
      out <- tryCatch(rotl::taxonomy_taxon_info(x,
                                                include_lineage = TRUE), error = function(e) e)
      if (inherits(out, "error")) {
        NA
      } else {
        outdf <- rotl::tax_lineage(out)[[1]]
        # we have to reverse row order
        outdf <- outdf[NROW(outdf):1, ]
        # tack on species searched for
        orig <- c(out[[1]]$rank, out[[1]]$name, out[[1]]$unique_name,
                  out[[1]]$ott_id)
        outdf <- rbind(outdf, orig)
        row.names(outdf) <- NULL
        outdf <- outdf[ , c('name','rank', 'ott_id')]
        names(outdf) <- c('name', 'rank', 'id')
        if (!return_id) outdf <- outdf[, c('name', 'rank')]
        return(outdf)
      }
    }
  }
  out <- lapply(id, fun, callopts = callopts)
  names(out) <- id
  structure(out, class = 'classification', db = 'tol')
}

#' @export
#' @rdname classification
classification.wormsid <- function(id, callopts = list(),
                                   return_id = TRUE, ...) {

  warn_db(list(...), "worms")
  fun <- function(x, ...){
    if (is.na(x)) {
      out <- NA
    } else {
      out <- tryCatch(worrms::wm_classification(as.numeric(x), ...),
                      error = function(e) e)
      if (inherits(out, "error")) {
        NA
      } else {
        row.names(out) <- NULL
        out <- out[ , c('scientificname','rank', 'AphiaID')]
        names(out) <- c('name', 'rank', 'id')
        if (!return_id) out <- out[, c('name', 'rank')]
        return(out)
      }
    }
  }
  out <- lapply(id, fun, ...)
  names(out) <- id
  structure(out, class = 'classification', db = 'worms')
}

#' @export
#' @rdname classification
classification.natservid <- function(id, callopts = list(),
                                     return_id = TRUE, ...) {

  warn_db(list(...), "natserv")
  fun <- function(x, callopts) {
    if (is.na(x)) {
      out <- NA
    } else {
      out <- tryCatch(natserv::ns_id(paste0("ELEMENT_GLOBAL.2.", x)), error = function(e) e)
      # out <- tryCatch(natserv::ns_data(x), error = function(e) e)
      if (inherits(out, "error")) {
        NA
      } else {
        zz <- c("kingdom", "phylum", "taxclass", "taxorder", "family", "genus")
        if (is.null(out$speciesGlobal)) return(NA)
        res <- tc(out$speciesGlobal[zz])
        if (length(res) == 0) return(NA)
        df <- data.frame(scientificname = unname(unlist(res)),
                         rank = gsub("tax", "", names(res)), stringsAsFactors = FALSE)
        df <- rbind(df, c(out$scientificName, "species"))
        # tmp <- out[[1]]$classification
        # if (is.null(tmp)) return(NA)
        # tmp <- tmp$taxonomy$formalTaxonomy
        # if (is.null(tmp)) return(NA)
        # tmp <- tmp[names(tmp) %in% c('kingdom', 'phylum',
        #   'class', 'order', 'family', 'genus')]
        # df <- data.frame(scientificname = unname(unlist(tmp)),
        #   rank = names(tmp), stringsAsFactors = FALSE)
        # rks <- c('kingdom', 'phylum', 'class', 'order',
        #   'family', 'genus', 'species')
        # targ_taxon <- c(
        #   out[[1]]$classification$names$scientificName$unformattedName[[1]],
        #   rks[which(df$rank[length(df$rank)] == rks) + 1]
        # )
        # df <- rbind(df, targ_taxon)
        return(df)
      }
    }
  }
  out <- lapply(id, fun, callopts = callopts)
  names(out) <- id
  structure(out, class = 'classification', db = 'natserv')
}

#' @export
#' @rdname classification
classification.boldid <- function(id, callopts = list(),
                                  return_id = TRUE, ...) {

  warn_db(list(...), "bold")
  fun <- function(x, callopts) {
    if (is.na(x)) {
      out <- NA
    } else {
      out <- tryCatch(bold_search(id = x, includeTree = TRUE),
                      error = function(e) e)
      if (inherits(out, "error")) {
        NA
      } else {
        if (is.null(out)) return(NA)
        tmp <- out[names(out) %in% c('taxid', 'taxon', 'tax_rank')]
        df <- data.frame(name = tmp$taxon, rank = tmp$tax_rank,
                         id = tmp$taxid, stringsAsFactors = FALSE)
        return(df)
      }
    }
  }
  out <- lapply(id, fun, callopts = callopts)
  names(out) <- id
  structure(out, class = 'classification', db = 'bold')
}

#' @export
#' @rdname classification
classification.wiki <- function(id, callopts = list(), return_id = TRUE, ...) {
  warn_db(list(...), "wiki")
  fun <- function(x, wiki_site = "species", wiki = "en", callopts) {
    if (is.na(x)) {
      out <- NA
    } else {
      fxn <- switch(
        wiki_site,
        species = wikitaxa::wt_wikispecies,
        commons = wikitaxa::wt_wikicommons,
        pedia = wikitaxa::wt_wikipedia
      )
      out <- tryCatch(fxn(x)$classification, error = function(e) e)
      if (inherits(out, "error")) {
        NA
      } else {
        if (is.null(out) || NROW(out) == 0) return(NA)
        df <- data.frame(name = out$name, rank = out$rank,
                         stringsAsFactors = FALSE)
        return(df)
      }
    }
  }
  out <- list()
  for (i in seq_along(id)) {
    out[[i]] <-
      fun(id[i], attr(id, "wiki_site"), attr(id, "wiki_lang"))
  }
  names(out) <- id
  structure(out, class = 'classification', db = 'wiki',
            wiki_site = attr(id, "wiki_site"), wiki = attr(id, "wiki_lang"))
}

#' @export
#' @rdname classification
classification.pow <- function(id, callopts = list(), return_id = TRUE, ...) {
  warn_db(list(...), "pow")
  fun <- function(x, callopts) {
    if (is.na(x)) {
      out <- NA
    } else {
      out <- tryCatch(pow_lookup(x), error = function(e) e)
      if (inherits(out, "error")) {
        NA
      } else {
        if (is.null(out)) return(NA)
        tmp <- out$meta$classification[,c('name', 'rank', 'fqId')]
        df <- data.frame(name = tmp$name, rank = tolower(tmp$rank),
                         id = tmp$fqId, stringsAsFactors = FALSE)
        return(df)
      }
    }
  }
  out <- lapply(id, fun, callopts = callopts)
  names(out) <- id
  structure(out, class = 'classification', db = 'pow')
}

# ---------
#' @export
#' @rdname classification
classification.ids <- function(id, ...) {
  fun <- function(x, ...){
    # return NA if NA is supplied
    if (all(is.na(x))) {
      out <- NA
    } else {
      out <- classification(x, ...)
    }
    return(out)
  }
  structure(lapply(id, fun, ...), class = 'classification_ids')
}

#' @export
#' @rdname classification
cbind.classification <- function(...) {
  gethiernames <- function(x) {
    x <- data.frame(x)
    x$name <- as.character(x$name)
    x$rank <- as.character(x$rank)
    values <- setNames(data.frame(t(x[,'name']), stringsAsFactors = FALSE),
                       tolower(x[,'rank']))
    if ("id" %in% names(x)) {
      x$id <- as.character(x$id)
      ids <- setNames(data.frame(t(x[,'id']), stringsAsFactors = FALSE),
                      paste0(tolower(x[,'rank']),"_id") )
      data.frame(values, ids)
    } else {
      values
    }
  }
  input <- x <- c(...)
  input <- input[vapply(x, function(z) inherits(z, "data.frame"), logical(1))]
  tmp <- dt2df(lapply(input, gethiernames), idcol = FALSE)
  tmp$query <- names(x)
  tmp$db <- attr(x, "db")
  tmp
}

#' @export
#' @rdname classification
rbind.classification <- function(...) {
  input <- x <- c(...)
  db <- attr(input, "db")
  x <- input[vapply(x, function(z) inherits(z, "data.frame"), logical(1))]
  for (i in seq_along(x)) {
    x[[i]]$query <- names(x[i])
  }
  df <- dt2df(x, idcol = FALSE)
  df$db <- db
  return( df )
}

#' @export
#' @rdname classification
cbind.classification_ids <- function(...) {
  input <- c(...)

  # remove non-data.frames
  input <- input[vapply(input, function(x)
    class(x[[1]])[1], "") %in% c("data.frame", "tbl_df")]

  gethiernames <- function(x){
    x$name <- as.character(x$name)
    x$rank <- as.character(x$rank)
    values <- setNames(data.frame(t(x$name), stringsAsFactors = FALSE),
                       tolower(x$rank))
    if ("id" %in% names(x)) {
      x$id <- as.character(x$id)
      ids <- setNames(data.frame(t(x$id), stringsAsFactors = FALSE),
                      paste0(tolower(x$rank), "_id") )
      data.frame(values, ids)
    } else {
      values
    }
  }
  dat <- dt2df(lapply(input, function(h){
    tmp <- lapply(h, gethiernames)
    tmp <- dt2df(tmp, idcol = FALSE)
    tmp$query <- names(h)
    tmp$db <- attr(h, "db")
    tmp
  }), idcol = FALSE)
  move_col(tt = dat, y = c('query','db'))
}

#' @export
#' @rdname classification
rbind.classification_ids <- function(...) {
  input <- c(...)

  # remove non-data.frames
  input <- input[vapply(input, function(x)
    class(x[[1]])[1], "") %in% c("data.frame", "tbl_df")]

  df <- lapply(input, function(x){
    coll <- list()
    for (i in seq_along(x)) {
      coll[[i]] <- data.frame(names(x[i]), x[i][[1]], stringsAsFactors = FALSE)
    }
    coll
  })

  get <- list()
  for (i in seq_along(df[[1]])) {
    tmp <- do.call(rbind, lapply(df, "[[", i))
    source2 <- gsub("\\.[0-9]+", "", row.names(tmp))
    row.names(tmp) <- NULL
    names(tmp)[1] <- "query"
    tmp <- data.frame(db = source2, tmp, stringsAsFactors = FALSE)
    get[[i]] <- tmp
  }

  tt <- if (length(get) == 1) get[[1]] else dt2df(get, idcol = FALSE)
  move_col(tt, c('query', 'db'))
}


##########################################################################################
#' Get Worms ID for a taxon name
#'
#' Retrieve Worms ID of a taxon from World Register of Marine
#' Species (WORMS).
#'
#' @export
#' @param sci_com character; A vector of common or scientific names. Or, a
#' `taxon_state` object (see [taxon-state])
#' @param searchtype character; One of 'scientific' or 'common', or any unique
#' abbreviation
#' @param marine_only logical; marine only? default: `TRUE` (only used
#' when `searchtype="scientific"`); passed on to [worrms::wm_records_name()]
#' @param fuzzy logical; fuzzy search. default: `NULL` (`TRUE` for
#' `searchtype="scientific"` and `FALSE` for `searchtype="common"` to match
#' the default values for those parameters in \pkg{worrms} package); passed on
#' to [worrms::wm_records_name()] or [worrms::wm_records_common()]
#' @param accepted logical; If TRUE, removes names that are not accepted valid
#' names by WORMS. Set to `FALSE` (default) to give back both accepted
#' and unaccepted names.
#' @param ask logical; should get_wormsid be run in interactive mode?
#' If `TRUE` and more than one wormsid is found for the species, the
#' user is asked for input. If `FALSE` NA is returned for
#' multiple matches.
#' @param messages logical; should progress be printed?
#' @param rows numeric; Any number from 1 to infinity. If the default NaN, all
#' rows are considered. Note that this function still only gives back a wormsid
#' class object with one to many identifiers. See [get_wormsid_()] to get back
#' all, or a subset, of the raw data that you are presented during the ask
#' process.
#' @param query Deprecated, see `sci_com`
#' @param x Input to `as.wormsid`
#' @param ... Ignored
#' @param check logical; Check if ID matches any existing on the DB, only
#' used in [as.wormsid()]
#@template getreturn
#'
#' @family taxonomic-ids
#' @seealso [classification()]
#'
#' @examples \dontrun{
#' (x <- get_wormsid('Gadus morhua'))
#' attributes(x)
#' attr(x, "match")
#' attr(x, "multiple_matches")
#' attr(x, "pattern_match")
#' attr(x, "uri")
#'
#' get_wormsid('Pomatomus saltatrix')
#' get_wormsid(c("Gadus morhua", "Lichenopora neapolitana"))
#'
#' # marine_only
#' get_wormsid("Apedinella", marine_only=TRUE)
#' get_wormsid("Apedinella", marine_only=FALSE)
#'
#' # fuzzy
#' ## searchtype="scientific": fuzzy is TRUE by default
#' get_wormsid("Platypro", searchtype="scientific", fuzzy=TRUE)
#' get_wormsid("Platypro", searchtype="scientific", fuzzy=FALSE)
#' ## searchtype="common": fuzzy is FALSE by default
#' get_wormsid("clam", searchtype="common", fuzzy=FALSE)
#' get_wormsid("clam", searchtype="common", fuzzy=TRUE)
#'
#' # by common name
#' get_wormsid("dolphin", 'common')
#' get_wormsid("clam", 'common')
#'
#' # specify rows to limit choices available
#' get_wormsid('Plat')
#' get_wormsid('Plat', rows=1)
#' get_wormsid('Plat', rows=1:2)
#'
#' # When not found
#' get_wormsid("howdy")
#' get_wormsid(c('Gadus morhua', "howdy"))
#'
#' # Convert a wormsid without class information to a wormsid class
#' # already a wormsid, returns the same
#' as.wormsid(get_wormsid('Gadus morhua'))
#' # same
#' as.wormsid(get_wormsid(c('Gadus morhua', 'Pomatomus saltatrix')))
#' # numeric
#' as.wormsid(126436)
#' # numeric vector, length > 1
#' as.wormsid(c(126436,151482))
#' # character
#' as.wormsid("126436")
#' # character vector, length > 1
#' as.wormsid(c("126436","151482"))
#' # list, either numeric or character
#' as.wormsid(list("126436","151482"))
#' ## dont check, much faster
#' as.wormsid("126436", check=FALSE)
#' as.wormsid(126436, check=FALSE)
#' as.wormsid(c("126436","151482"), check=FALSE)
#' as.wormsid(list("126436","151482"), check=FALSE)
#'
#' (out <- as.wormsid(c(126436,151482)))
#' data.frame(out)
#' as.wormsid( data.frame(out) )
#'
#' # Get all data back
#' get_wormsid_("Plat")
#' get_wormsid_("Plat", rows=1)
#' get_wormsid_("Plat", rows=1:2)
#' get_wormsid_("Plat", rows=1:75)
#' # get_wormsid_(c("asdfadfasd","Plat"), rows=1:5)
#' }
get_wormsid <- function(sci_com, searchtype = "scientific", marine_only = TRUE,
                        fuzzy = NULL, accepted = FALSE, ask = TRUE, messages = TRUE,
                        rows = NA, query = NULL, ...) {

  checkmate::assert(sci_com, c("character", "taxon_state"))
  checkmate::assert(searchtype, "character")
  checkmate::assert(marine_only, "logical")
  checkmate::assert(fuzzy, "logical")
  checkmate::assert(accepted, "logical")
  checkmate::assert(ask, "logical")
  assert_rows(rows)
  if (!is.null(query)) {
    lifecycle::deprecate_warn(when = "v0.9.97", what = "get_wormsid(query)", with = "get_wormsid(sci_com)")
    sci_com <- query
  }


  if (inherits(sci_com, "character")) {
    tstate <- taxon_state$new(class = "wormsid", names = sci_com)
    items <- sci_com
  } else {
    assert_state(sci_com, "wormsid")
    tstate <- sci_com
    sci_com <- tstate$taxa_remaining()
    items <- c(sci_com, tstate$taxa_completed())
  }

  prog <- progressor$new(items = items, suppress = !messages)
  done <- tstate$get()
  for (i in seq_along(done)) prog$completed(names(done)[i], done[[i]]$att)
  prog$prog_start()

  for (i in seq_along(sci_com)) {
    direct <- FALSE
    mssg(messages, "\nRetrieving data for taxon '", sci_com[i], "'\n")

    if (!searchtype %in% c("scientific", "common")) {
      stop("'searchtype' must be one of 'scientific' or 'common'", call. = FALSE)
    }
    # NOTE: space replacement needed for curl problem (issue #888)
    query <- gsub(sci_com[i], pattern = ' ', replacement = '+', fixed = TRUE)
    wmdf <- switch(
      searchtype,
      scientific = worms_worker(query, worrms::wm_records_name, rows,
                                marine_only, fuzzy %||% TRUE, ...),
      common = worms_worker(query, worrms::wm_records_common, rows,
                            marine_only, fuzzy %||% FALSE, ...)
    )
    mm <- NROW(wmdf) > 1

    if (!inherits(wmdf, "tbl_df") || NROW(wmdf) == 0) {
      wmid <- NA_character_
      att <- "not found"
    } else {
      wmdf <- suppressWarnings(data.frame(wmdf))
      wmdf <- wmdf[, c("AphiaID", "scientificname", "authority", "status")]
      names(wmdf)[1] <- "id"

      if (accepted) {
        wmdf <- wmdf[ wmdf$status %in% 'accepted', ]
      }

      wmdf <- sub_rows(wmdf, rows)

      # should return NA if spec not found
      if (nrow(wmdf) == 0) {
        mssg(messages, m_not_found_sp_altclass)
        wmid <- NA_character_
        att <- "not found"
      }

      # take the one wmid from data.frame
      if (nrow(wmdf) == 1) {
        wmid <- wmdf$id
        att <- "found"
      }

      # check for direct match
      if (nrow(wmdf) > 1) {
        names(wmdf)[grep("scientificname", names(wmdf))] <- "target"
        matchtmp <- wmdf[tolower(wmdf$target) %in% tolower(sci_com[i]), "id"]
        if (length(matchtmp) == 1) {
          wmid <- matchtmp
          direct <- TRUE
          att <- "found"
        } else {
          wmid <- NA_character_
          att <- "not found"
        }
      }

      # multiple matches
      if (any(
        nrow(wmdf) > 1 && is.na(wmid) |
        nrow(wmdf) > 1 && att == "found" & length(wmid) > 1
      )) {
        if (ask) {
          names(wmdf)[grep("scientificname", names(wmdf))] <- "target"

          # user prompt
          wmdf <- wmdf[order(wmdf$target), ]

          # prompt
          message("\n\n")
          print(wmdf)
          message("\nMore than one WORMS ID found for taxon '", sci_com[i], "'!\n
                  Enter rownumber of taxon (other inputs will return 'NA'):\n") # prompt
          take <- scan(n = 1, quiet = TRUE, what = 'raw')

          if (length(take) == 0) {
            take <- 'notake'
            att <- 'nothing chosen'
          }
          if (take %in% seq_len(nrow(wmdf))) {
            take <- as.numeric(take)
            message("Input accepted, took taxon '", as.character(wmdf$target[take]), "'.\n")
            wmid <-  wmdf$id[take]
            att <- "found"
          } else {
            wmid <- NA_character_
            mssg(messages, "\nReturned 'NA'!\n\n")
            att <- "not found"
          }
        } else {
          if (length(wmid) != 1) {
            warning(sprintf(m_more_than_one_found, "Worms ID", sci_com[i]),
                    call. = FALSE)
            wmid <- NA_character_
            att <- m_na_ask_false
          }
        }
      }

    }
    res <- list(id = as.character(wmid), att = att, multiple = mm,
                direct = direct)
    prog$completed(sci_com[i], att)
    prog$prog(att)
    tstate$add(sci_com[i], res)
  }
  out <- tstate$get()
  ids <- structure(pluck_un(out, "id", ""), class = "wormsid",
                   match = pluck_un(out, "att", ""),
                   multiple_matches = pluck_un(out, "multiple", logical(1)),
                   pattern_match = pluck_un(out, "direct", logical(1))
  )
  on.exit(prog$prog_summary(), add = TRUE)
  on.exit(tstate$exit, add = TRUE)
  add_uri(ids, get_url_templates$worms)
}

try_df <- function(expr) {
  res <- tryCatch(expr, error = function(e) e)
  if (inherits(res, "error")) {
    data.frame(NULL)
  } else {
    res
  }
}

#' @export
#' @rdname get_wormsid
as.wormsid <- function(x, check=TRUE) UseMethod("as.wormsid")

#' @export
#' @rdname get_wormsid
as.wormsid.wormsid <- function(x, check=TRUE) x

#' @export
#' @rdname get_wormsid
as.wormsid.character <- function(x, check=TRUE) if (length(x) == 1) make_worms(x, check) else collapse(x, make_worms, "wormsid", check = check)

#' @export
#' @rdname get_wormsid
as.wormsid.list <- function(x, check=TRUE) if (length(x) == 1) make_worms(x, check) else collapse(x, make_worms, "wormsid", check = check)

#' @export
#' @rdname get_wormsid
as.wormsid.numeric <- function(x, check=TRUE) as.wormsid(as.character(x), check)

#' @export
#' @rdname get_wormsid
as.wormsid.data.frame <- function(x, check=TRUE) {
  structure(x$ids, class = "wormsid", match = x$match,
            multiple_matches = x$multiple_matches,
            pattern_match = x$pattern_match, uri = x$uri)
}

#' @export
#' @rdname get_wormsid
as.data.frame.wormsid <- function(x, ...){
  data.frame(ids = as.character(unclass(x)),
             class = "wormsid",
             match = attr(x, "match"),
             multiple_matches = attr(x, "multiple_matches"),
             pattern_match = attr(x, "pattern_match"),
             uri = attr(x, "uri"),
             stringsAsFactors = FALSE)
}

make_worms <- function(x, check=TRUE) {
  make_generic(x, 'https://www.marinespecies.org/aphia.php?p=taxdetails&id=%s',
               "wormsid", check)
}

check_wormsid <- function(x){
  tt <- worrms::wm_record(as.numeric(x))
  identical(as.character(tt$AphiaID), as.character(x))
}

#' @export
#' @rdname get_wormsid
get_wormsid_ <- function(sci_com, messages = TRUE, searchtype = "scientific",
                         marine_only = TRUE, fuzzy = NULL, accepted = TRUE, rows = NA, query = NULL,
                         ...) {

  if (!is.null(query)) {
    lifecycle::deprecate_warn(when = "v0.9.97", what = "get_wormsid_(query)", with = "get_wormsid_(sci_com)")
    sci_com <- query
  }
  stats::setNames(
    lapply(sci_com, get_wormsid_help, messages = messages,
           searchtype = searchtype, marine_only = marine_only, fuzzy = fuzzy,
           accepted = accepted, rows = rows, ...),
    sci_com
  )
}

get_wormsid_help <- function(query, messages, searchtype, marine_only,
                             fuzzy, accepted, rows, ...) {

  mssg(messages, "\nRetrieving data for taxon '", query, "'\n")
  searchtype <- match.arg(searchtype, c("scientific", "common"))
  df <- switch(
    searchtype,
    scientific = worms_worker(query, worrms::wm_records_name, rows = rows,
                              marine_only = marine_only, fuzzy = fuzzy, ...),
    common = worms_worker(query, worrms::wm_records_common, rows = rows,
                          marine_only = marine_only, fuzzy = fuzzy, ...)
  )
  if (!inherits(df, "tbl_df") || NROW(df) == 0) {
    NULL
  } else {
    df <- df[, c("AphiaID","scientificname","authority","status")]
    if (accepted) df <- df[ df$status %in% 'accepted', ]
    sub_rows(df, rows)
  }
}

# WORMS WORKER
# worms_worker(x = "Plat", expr = worrms::wm_records_name)
worms_worker <- function(x, expr, rows, marine_only, fuzzy, ...) {
  if (
    all(!is.na(rows)) &&
    class(rows) %in% c('numeric', 'integer') &&
    rows[length(rows)] <= 50
  ) {
    try_df(expr(x, marine_only = marine_only, fuzzy = fuzzy, ...))
  } else if (
    all(!is.na(rows)) &&
    class(rows) %in% c('numeric', 'integer') &&
    rows[length(rows)] > 50
  ) {
    out <- try_df(expr(x, marine_only = marine_only, fuzzy = fuzzy, ...))
    out <- list(out)
    i <- 1
    total <- 0
    while (NROW(out[[length(out)]]) == 50 && total < rows[length(rows)]) {
      i <- i + 1
      out[[i]] <- try_df(expr(x, marine_only = marine_only, fuzzy = fuzzy,
                              offset = sum(unlist(sapply(out, NROW))), ...))
      total <- sum(unlist(sapply(out, NROW)))
    }
    df2dt2tbl(out)[rows,]
  } else {
    out <- try_df(expr(x, marine_only = marine_only, fuzzy = fuzzy, ...))
    out <- list(out)
    i <- 1
    while (NROW(out[[length(out)]]) == 50) {
      i <- i + 1
      out[[i]] <- try_df(expr(x, marine_only = marine_only, fuzzy = fuzzy,
                              offset = sum(unlist(sapply(out, NROW))), ...))
    }
    df2dt2tbl(out)
  }
}
