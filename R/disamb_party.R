
#' Disambiguate political party names + match to Party Facts ID
#'
#' @param x Data frame containing party
#' @param party_ref A string identifying the variable in the data, \code{x}, containing political party names or other party identifiers
#' @param country A string identifying the country, if single-country data, or vector of strings identifying the country of each observation if multi-country data.
#' @param year A four-digit numeric identifying the year of the data, or a string identifying the variable in \code{x} containing the year variable (four-digit numeric). If \code{year = NULL}, assumes the current calendar year.
#' @param origin A string identifying the origin format of country names (see \code{countrycode} package). If \code{origin = NULL}, attempts to resolve the origin format automatically.
#'
#' @return A tibble, \code{x}, with new columns for Party Facts id and information on the matching process
#' @export
#'
#' @importFrom magrittr `%>%`
#'
#' @examples
#'
#'


disamb_party <- function(x, party_ref, country, year = NULL, origin = NULL) {

  #############################################################
  ## SANITY CHECK INPUT: 'party_ref'

  # party_ref needs to be an internal-to-data-environment reference!!!
  if (!(is.atomic(party_ref) &&
        length(party_ref) == 1L) &&
      is.character(party_ref) &&
      (party_ref %in% names(x))) stop("party_ref must be a string identifying the variable in the data containing the political party names or other party identifiers")


  #############################################################
  ## SANITY CHECK INPUT: country

  # country is a single string or vector of strings (length = nrow(x))
  if (!(is.atomic(country) &&
        is.character(country))) top("country must be a string identifying the country of the data or a vector of strings identifying the country of each observation in the data.")

  if (!xor(length(country) == 1L,
           length(country) == nrow(x))) stop("country must be a string identifying the country of the data or a vector of strings identifying the country of each observation in the data.")


  #############################################################
  ## SANITY CHECK INPUT: year (length only)

  # CASE: vector
  # if year is numeric vector, catch incorrect length (i.e. length != nrow(x))
  if (length(year) > 1 && (length(year) != nrow(x))) {
    stop("If year is a vector, it must be the same length as the number of rows in the data")
  }


  #############################################################
  ## DEAL WITH NAs IN ANY KEY VARIABLES
  pna <- is.na(x[[party_ref]])
  cna <- is.na(country)
  if (cna & (length(cna) == 1)) { stop("country must be a string identifying the country of the data or a vector of strings identifying the country of each observation in the data.") }
  if (is.character(year)) { yna <- is.na(x[[year]]) }
  if (is.numeric(year)) { yna <- is.na(year) }
  if ((length(yna) == 1) && yna) { stop("year not provided correctly. If a four-digit numeric scalar, year will be applied to all observations. If a numeric vector, year will be applied to each observation. If a character object, year must refer to a variable in the data. If NULL, year is assumed to be the current year.") }
  lna <- list(pna, cna, yna)

  if (any(purrr::map_lgl(lna, any))) {
    lna <- lna[purrr::map_int(lna, length) > 1]

    tna <- lna %>%
      unlist %>%
      matrix(nrow = nrow(x)) %>%
      apply(1, any)

    if (any(tna)) {
      quar.x <- x[tna, ]
      x <- x[!tna, ]
    }
  }


  #############################################################
  ## SANITY CHECK INPUT: 'x'

  # x must be data frame / tibble
  if (!("data.frame" %in% class(x))) stop("x must be a data frame or tibble.")


  #############################################################
  ## SANITY CHECK INPUT: 'origin'
  if (!is.null(origin)) { # CASE: 'origin' *not* NULL
    # reject non-valid 'origin' specifications
    if (!(origin %in% c("cctld", "country.name",
                        "country.name.de", "cowc", "cown", "dhs", "ecb",
                        "eurostat", "fao", "fips", "gaul", "genc2c", "genc3c",
                        "genc3n", "gwc", "gwn", "imf", "ioc", "iso2c", "iso3c",
                        "iso3n", "p4c", "p4n", "un", "un_m49", "unicode.symbol",
                        "unhcr", "unpd", "vdem", "wb", "wb_api2c", "wb_api3c",
                        "wvs", "country.name.en.regex", "country.name.de.regex"))) {
      stop("origin must be a valid origin for countrycode function. See help(countrycode). If origin is not specified, this function will try to resolve a valid input for 'origin'.")
      # In future: change this to attempt to resolve valid origin if
      # provided origin is non-valid -- warn user when this is done
    }

    clean_origin <- origin
  }

  # if 'origin' is NULL, attempt to resolve country format
  if (is.null(origin)) {
    origin_hits <- purrr::map_lgl(countrycode::codelist, function(xx) all(unique(country) %in% xx))
    origin_hits <- names(origin_hits[origin_hits]) %>%
      stringr::str_replace_all("country.name.en", "country.name") %>%
      stringr::str_replace_all("cow.name", "cown") %>%
      stringr::str_replace_all("vdem.name", "vdem") %>%
      stringr::str_replace_all("un\\..+", "un")

    origin_hits <- origin_hits[origin_hits %in% c("cctld", "country.name",
                                                  "country.name.de", "cowc", "cown", "dhs", "ecb",
                                                  "eurostat", "fao", "fips", "gaul", "genc2c", "genc3c",
                                                  "genc3n", "gwc", "gwn", "imf", "ioc", "iso2c", "iso3c",
                                                  "iso3n", "p4c", "p4n", "un", "un_m49", "unicode.symbol",
                                                  "unhcr", "unpd", "vdem", "wb", "wb_api2c", "wb_api3c",
                                                  "wvs", "country.name.en.regex", "country.name.de.regex")]

    if (length(origin_hits) > 0) {
      clean_origin <- origin_hits[1]

      # warn that origin has been assumed based on origin_hits
      cat(paste0("\nCountrycode origin format not provided. Assuming: ", "\"", clean_origin, "\" (See countrycode package)\n"))

      # get jnry_country var
      x <- x %>%
        dplyr::mutate(jnry_country = countrycode::countrycode(
          sourcevar = country,
          origin = clean_origin,
          destination = "iso3c"
        )) %>%
        tibble::as_tibble()

    } else { # CASE: NO COL HAS MATCHES FOR ALL COUNTRY DESIGNATIONS
      # which cols in 'codelist' have any hits?
      origin_hits <- purrr::map_int(countrycode::codelist, function(xx) sum(unique(country) %in% xx))

      # if these are English country names
      if ("country.name.en" %in% names(origin_hits[origin_hits == max(origin_hits)])) {
        # # change Northern Ireland to UK
        # fixed_country <- ifelse(country %>% str_detect(regex("Northern Ireland", ignore_case = TRUE)),
        #                         "United Kingdom",
        #                         country)

        # separate resolved + unresolved
        resolved.cn <- country[(country %in% countrycode::codelist$country.name.en)] %>% unique()
        toresolve.cn <- country[!(country %in% countrycode::codelist$country.name.en)] %>% unique()


        # if no easy hits, move to English regex (consider adding fallbacks/options to this?)
        ctry_mtchs <- countrycode::codelist$country.name.en.regex %>%
          purrr::map(
            function(xx) {
              stringr::str_which(string = toresolve.cn,
                                 pattern = stringr::regex(xx, ignore_case = TRUE))
            }
          )

        # remap country matches
        x[["mrg_country"]] <- country

        x <- x %>%
          dplyr::left_join(
            tibble::tibble(
              cc_row = which(ctry_mtchs %>% purrr::map_lgl(function(xx) length(xx) > 0)),
              mrg_country = toresolve.cn[unlist(ctry_mtchs[ctry_mtchs %>% purrr::map_lgl(function(xx) length(xx) > 0)])]
            ) %>%
              dplyr::add_row(
                cc_row = purrr::map_int(resolved.cn, function(xx) which(countrycode::codelist$country.name.en %in% xx)),
                mrg_country = resolved.cn
              ) %>%
              dplyr::mutate(jnry_country = countrycode::codelist$iso3c[cc_row]) %>%
              dplyr::select(-cc_row),
            by = "mrg_country"
          ) %>%
          # fix dumb N.Ireland issue
          dplyr::mutate(
            jnry_country = ifelse(
              mrg_country %>% stringr::str_detect(stringr::regex("northern ireland", ignore_case = TRUE)),
              "GBR",
              jnry_country
            ),
            # fix stupid East Germany issue... (maybe this shouldn't be hard-coded?)
            jnry_country = ifelse(
              mrg_country %>% stringr::str_detect(stringr::regex("german", ignore_case = TRUE)),
              "DEU",
              jnry_country
            )) %>%
          dplyr::select(-mrg_country)

      } else if (all(origin_hits == 0)) {


        #############################################################
        ## NO MATCHES FOR COUNTRY NAMES IN THE countrycode 'codelist'
        ## NOTHING MORE CAN BE DONE, SO RETURN AN ERROR
        ## ADVISE USER TO FIX THEIR COUNTRY NAMES
        stop("Cannot resolve origin format of country name or identifier. Try explicitly specifying origin format & checking country identifiers for errors/misspellings.")

      } else {
        # there are hits in 'codelist' but the most hits aren't happening in
        # the English-language names
        # have to proceed carefully and match as possible

        # find best match in 'codelist'
        best_origin_mtch <- names(origin_hits[origin_hits == max(origin_hits)][1])

        # if no easy hits, move to English regex (consider adding fallbacks/options to this?)
        ctry_mtchs <- unique(country) %>%
          purrr::map(
            function(xx) {
              which(countrycode::codelist[[best_origin_mtch]] %in% xx)
            }
          ) %>%
          purrr::map(
            function(xx) {
              xx <- ifelse(length(xx) == 0, NA, xx)
            }
          )

        # remap country matches
        x[["mrg_country"]] <- country

        # get best match for 'country' that can be managed
        x <- x %>%
          dplyr::left_join(
            tibble::tibble(
              cc_row = unlist(ctry_mtchs),
              mrg_country = unique(country)
            ),
            by = "mrg_country"
          ) %>%
          dplyr::mutate(jnry_country = countrycode::codelist$iso3c[cc_row]) %>%
          dplyr::select(-cc_row, -mrg_country)

        # this solution leaves NAs in the 'jnry_country' field
        # does this create problems later?

        if (sum(is.na(x$jnry_country)) == 0) {
          # NB: warn the user that this is the case, and that the data will be incomplete
          cat("\nDifficulty matching country names. All country identifies successfully matched, but recommend checking integrity of matches.")
        } else {
          # NB: warn the user that this is the case, and that the data will be incomplete
          cat(paste0("\nDifficulty matching country names.\n", sum(is.na(x$jnry_country)), " observations had no match in the countrycode codelist\nand therefore have no assigned country name.\nRecommend checking country identifiers for errors/misspellings and trying again.\n"))
        }
      }
    }
  }




  #############################################################
  ## SANITY CHECK INPUT: year

  # year can take the following allowed formats:
  #   - scalar 4-digit #
  #   - vector of 4-digit #s
  #   - ref to a data frame col (of 4-digit #s)
  #   - NULL [what to do in this case??]
  #     currently, NULL --> current year (NB: make explicit in documentation)
  #
  # REJECT CONDITIONS:
  #   - year not atomic
  #   - any year not four digits
  #   - year vector not the right length (i.e. not as long as target data)
  #   (should we also reject if any missing values?)
  #   (This should at least throw a warning...)

  # catch cases
  if (is.null(year)) { # CASE: NULL

    # if year NULL --> set year to current year
    x <- x %>%
      dplyr::mutate(jnry_year = Sys.Date() %>% stringr::str_extract("^\\d{4}"))

    # warn user that year --> current year
    warning("No year provided. Defaulting to current calendar year.\n")

  } else if (!is.atomic(year) || length(year) == 0) {

    # if year is provided, but it is not the correct class or is empty --> BREAK
    stop("year not provided correctly. If a four-digit numeric scalar, year will be applied to all observations. If a numeric vector, year will be applied to each observation. If a character object, year must refer to a variable in the data. If NULL, year is assumed to be the current year.")

  } else if (is.numeric(year)) { # CASE: numeric

    # catch if any numeric year is not four digits (i.e. somebody mistyped?)
    if (!all(stringr::str_length(year) == 4)) stop("year not provided correctly. If a four-digit numeric scalar, year will be applied to all observations. If a numeric vector, year will be applied to each observation. If a character object, year must refer to a variable in the data. If NULL, year is assumed to be the current year.")

    # if no other errors, put the year into the data (x)
    x <- x %>% dplyr::mutate(jnry_year = year)

  } else if (is.character(year) && (length(year) == 1)) { # CASE: character

    # check character year is a variable in the data
    if (!(year %in% names(x))) stop("year not provided correctly -- if provided as a string, must refer to a variable in the data.")

    # if year is character scalar, retrieve that column in the data
    year <- x[[year]]

    # catch years not four digits
    if (!is.numeric(year) || !all(stringr::str_length(year) == 4)) stop("year not provided correctly -- must be numeric and consist of only four-digit numbers")

    # create a new column for jnry_year -- wasteful, but... efficient and doesn't rename user variables
    x <- x %>% dplyr::mutate(jnry_year = year)

  } else {

    # any other cases throw an error
    stop("year not provided correctly. If a four-digit numeric scalar, year will be applied to all observations in the input data. If a numeric vector, year will be mapped to observations in the data. If a character object, year must refer to one variable in the data. If NULL, year is assumed to be the current year.")

  }

  # should we use a finer-grained date instead of year????
  # after all, parties can change names during a year...
  # this is not that much an edge case, tho it depends on the data's time granularity
  # ---to resolve later---
  # NB: argument against more finely grained time var: Party Facts is annualized
  # So, after all, what would be the point?
  # This raises the question, tho: are there instances of overlap in the Party Facts data?
  # i.e. cases in which a party AND its renamed successor both appear in the same year?
  # again, to be resolved later -- perhaps via testing PF data




  #############################################################
  ## INPUTS ALL RESOLVED & rolled into data ##################################
  #############################################################

  #############################################################
  ## NOW TO RESOLVE PARTY NAMES ##############################################
  #############################################################




  #############################################################
  ## GET Party Facts core data
  #   1) pull in *latest* CORE PartyFacts parties
  #   2) filter to countries in target data
  #   3) filter to years (potentially) relevant to target data
  #   4) stretch years (i.e. from PF "from-->to" format to one row per year * party)
  pfcp <- utils::read.csv(file = "https://partyfacts.herokuapp.com/download/core-parties-csv/",
                          encoding = "UTF-8") %>%
    tibble::as_tibble() %>%
    dplyr::filter(country %in% x$jnry_country) %>%
    dplyr::filter(is.na(year_last) | year_last >= min(x$jnry_year)) %>%
    dplyr::mutate(year_first = ifelse(year_first < min(x$jnry_year),
                                      min(x$jnry_year),
                                      year_first)) %>%
    stretch_pf()
  #############################################################



  #############################################################
  ## Generate consistent references table
  ## party * year * country table
  ## then fill in the appropriate columns for lookup
  ## in Party Facts data

  # list of vars IN INPUT DATA to group over
  grpng_vars <- c(party_ref, "jnry_country", "jnry_year")

  # condense INPUT DATA over unique party-year-country intersections
  # this will be matched to PF
  options(dplyr.summarise.inform = FALSE)
  cons_refs <- x %>%
    dplyr::group_by(
      dplyr::across({{ grpng_vars }})
    ) %>%
    dplyr::summarize() %>%
    dplyr::ungroup()

  # give party ref in this consistent reference dataset a fixed name
  names(cons_refs)[1] <- "opn" # "original party name"
  #############################################################



  #############################################################
  ## FIND PartyFacts match column
  # look for easy *exact* matches
  party_hits <- pfcp %>%
    dplyr::select(
      tidyselect::starts_with('name')
    ) %>%
    purrr::map_lgl(
      function(xx) all(unique(cons_refs$opn) %in% xx)
    )

  #############################################################
  ## double-check that a TRUE in party_hits isn't a mirage
  ## if an inner join doesn't result in the exact same # of rows
  ## in cons_refs, then don't accept it
  ## NB: this is a stringent test, because it rejects cases
  ## in which Party Facts is missing a year for one party
  ## However, it will also reject cases in which a party name
  ## mistakenly matches BY NAME a party in another country.
  ## The risk of ignoring the case merits the higher standard here (for now)
  #############################################################
  if (any(party_hits)) {

    # gen name var in 'cons_refs' for merge to PF data
    check_cons_refs <- cons_refs
    check_cons_refs[[names(party_hits)[party_hits]]] <- check_cons_refs$opn

    # vars to match on for name field
    pf_mtch_vars <- c(names(party_hits)[party_hits],
                      "jnry_country", "jnry_year",
                      "partyfacts_id")

    # does an inner join result in the same # of rows??
    inner_join_check <- check_cons_refs %>%
      dplyr::inner_join(
        pfcp %>%
          dplyr::select({{ pf_mtch_vars }}),
        by = pf_mtch_vars[1:3]
      ) %>%
      nrow() == nrow(check_cons_refs)

    if (!inner_join_check) {
      party_hits[party_hits] <- FALSE
    }
  }
  #############################################################



  #############################################################
  ## IF we have easy matches, take the immediate off-ramp
  if (any(party_hits)) {

    # gen name var in 'cons_refs' for merge to PF data
    cons_refs[[names(party_hits)[party_hits]]] <- cons_refs$opn

    # vars to match on for name field
    pf_mtch_vars <- c(names(party_hits)[party_hits],
                      "jnry_country", "jnry_year",
                      "partyfacts_id")

    # merge cons_refs to pfcp
    cons_refs <- cons_refs %>%
      dplyr::left_join(
        pfcp %>%
          dplyr::select({{ pf_mtch_vars }}),
        by = pf_mtch_vars[1:3]
      )

    ## merge x to cons_refs
    # replace 'cons_refs' opn with original party_ref
    names(cons_refs) <- names(cons_refs) %>% stringr::str_replace("opn", party_ref)

    # merging vars
    mrg_vars <- c(names(cons_refs)[1:3],
                  "partyfacts_id")

    # execute merge
    x <- x %>%
      dplyr::left_join(
        cons_refs %>%
          dplyr::select({{ mrg_vars }}),
        by = mrg_vars[1:3]
      )

    # reattach quarantined NA obs
    if (exists("quar.x")) {
      x <- x %>% dplyr::bind_rows(quar.x)
      cat("\nNote that one or more NAs were present in party, country, or year variables.")
    }

    # report progress
    cat("\nAll party names have exact matches in Party Facts.\n")

    ## return orig data w/jnry ctry+year variables & partyfacts_id
    return(x)

  } else {

    #############################################################
    ## DON'T GIVE UP ON EXACT MATCHES RIGHT AWAY
    ## FIND ANY, IF THEY EXIST

    # Catch scattered exact matches, if any
    exact_successes <- pfcp %>%
      dplyr::select(tidyselect::starts_with('name')) %>%
      purrr::map(function(xx) unique(cons_refs$opn) %in% xx)

    # which PF vars had matches?
    pf_name_hits <- exact_successes %>%
      purrr::map(function(xx) sum(xx)) %>%
      unlist()

    # WERE THERE ANY EXACT MATCHES AT ALL???
    if (sum(pf_name_hits) > 0) {

      # COME DOWN TO **ONE** BEST PF VAR TO USE
      # if only one var had exact matches
      if (sum(pf_name_hits == max(pf_name_hits)) == 1) {
        best_pf_name_var <- pf_name_hits[which(pf_name_hits == max(pf_name_hits))] %>% names

        # if two PF vars had equal #s of exact matches
      } else if (sum(pf_name_hits == max(pf_name_hits)) > 1) {

        # choose the preferred var (NB: joinery is opinionated here)
        best_pf_name_var <- pf_name_hits[which(pf_name_hits == max(pf_name_hits))] %>% names
        if ("name_english" %in% best_pf_name_var) {
          best_pf_name_var <- "name_english"
        } else if ("name" %in% best_pf_name_var) {
          best_pf_name_var <- "name"
        } else if ("name_short" %in% best_pf_name_var) {
          best_pf_name_var <- "name_short"
        }
      }

      # roll EXACT matches into 'cons_refs'
      pf_mtch_vars <- c("jnry_country", "jnry_year", best_pf_name_var, "partyfacts_id")

      cons_refs <- cons_refs %>%
        dplyr::left_join(
          pfcp %>%
            dplyr::select({{ pf_mtch_vars }}) %>%
            dplyr::mutate(opn = .data[[best_pf_name_var]]),
          by = c('opn', pf_mtch_vars[1:2])
        ) %>%
        dplyr::mutate(match_exact = !is.na(partyfacts_id))


      # Report progress & warn about heuristic matching
      cat(stringr::str_c("\n-----------------------------------------------------------\nFound ", sum(cons_refs$match_exact), " exact matches of ", nrow(cons_refs), "\nparty x year observations.\n-----------------------------------------------------------\n"))
      cat("\nParty identifier (party_ref) had some party names without exact match in Party Facts data.\n\n---Trying heuristic matching (English only)---\n")
    } else {
      # note in 'cons_refs' there are 0 exact matches
      cons_refs <- cons_refs %>% dplyr::mutate(match_exact = F)

      # warn user about heuristic matching
      cat("\nParty identifier (party_ref) had NO exact matches in Party Facts data.\n\n---Trying heuristic matching (English only)---\n")
    }



    #############################################################
    ## HEURISTIC MATCHING w/regex reconstruction of party names

    # get country into the heur_par_target to pass on for future filtering
    c_p_combo <- paste(cons_refs$opn, cons_refs$jnry_country, sep = "----")

    # regex reconstruct TARGET party names
    heur_par_target <- stats::setNames(object = as.list(unique(c_p_combo[!cons_refs$match_exact])),
                                       nm = unique(c_p_combo[!cons_refs$match_exact])) %>%
      purrr::map(
        function(xx) {
          transforms <- xx %>% stringr::str_replace("----.+", "") %>% party_name_heuristics
          transforms <- c(stringr::str_extract(xx, "[A-Z]{3}$"), transforms)
        }
      )

    # apply heuristic regex tranforms & check success by party
    any_heur_success <- heur_par_target %>% purrr::map_lgl(
      function(p_heuristics) {
        any(
          purrr::map_lgl(pfcp %>%
                           dplyr::filter(jnry_country %in% p_heuristics[1]) %>%
                           dplyr::select(tidyselect::starts_with('name')),
                         function(pf_names) any(p_heuristics[2:length(p_heuristics)] %in% pfcp_fast_munge(pf_names)))
        )
      }
    ) %>%
      which()

    # get PF columns for parties w/heur success
    # NB: if 'any_heur_success' comes up empty, this still runs w/o error
    # col_heur_hits just ends up having zero rows --> catch this below to check
    # for any heuristic match successes!
    col_heur_hits <- heur_par_target[names(heur_par_target) %in% names(any_heur_success)] %>%
      purrr::map(
        function(p_heuristics) {
          purrr::map_lgl(pfcp %>%
                           dplyr::filter(jnry_country %in% p_heuristics[1]) %>%
                           dplyr::select(tidyselect::starts_with('name')),
                         function(pf_names) any(p_heuristics[2:length(p_heuristics)] %in% pfcp_fast_munge(pf_names)))
        }
      ) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(
        opn = (names(heur_par_target)[names(heur_par_target) %in% names(any_heur_success)]) %>% stringr::str_replace_all("----[A-Z]{3}$", ""),
        jnry_country = (names(heur_par_target)[names(heur_par_target) %in% names(any_heur_success)]) %>% stringr::str_extract("[A-Z]{3}$")
        )

    # remove parties *already* exact matched
    col_heur_hits <- col_heur_hits %>%
      dplyr::filter(!(opn %in% cons_refs$opn[cons_refs$match_exact]))


    #############################################################
    # WERE ANY HEURISTIC MATCHES DISCOVERED??
    if (nrow(col_heur_hits) > 0) { # CASE: Yes

      # get heuristic matches & col in pfcp where they are
      # NB: this will create a problem if more than one of the
      # munged party heuristic names appears in a munged pfcp
      # name variable (see: use of 'which')
      heur_pfcp_refs <- col_heur_hits %>%
        purrr::array_branch(margin = 1) %>%
        purrr::map(
          function(xx) {
            if ( exists("best_pf_name_var") && (best_pf_name_var %in% names(xx[as.logical(xx)])) ) {
              c(party_name_heuristics(xx['opn'])[which(party_name_heuristics(xx['opn']) %in% pfcp_fast_munge(pfcp[[best_pf_name_var]][pfcp$jnry_country %in% xx['jnry_country']]))], best_pf_name_var)
            } else {
              if ("name_english" %in% names(xx[as.logical(xx)])) {
                c(party_name_heuristics(xx['opn'])[which(party_name_heuristics(xx['opn']) %in% pfcp_fast_munge(pfcp$name_english[pfcp$jnry_country %in% xx['jnry_country']]))], "name_english")
              } else if ("name" %in% names(xx[as.logical(xx)])) {
                c(party_name_heuristics(xx['opn'])[which(party_name_heuristics(xx['opn']) %in% pfcp_fast_munge(pfcp$name[pfcp$jnry_country %in% xx['jnry_country']]))], "name")
              } else if ("name_short" %in% names(xx[as.logical(xx)])) {
                c(party_name_heuristics(xx['opn'])[which(party_name_heuristics(xx['opn']) %in% pfcp_fast_munge(pfcp$name_short[pfcp$jnry_country %in% xx['jnry_country']]))], "name_short")
              } else {
                c(party_name_heuristics(xx['opn'])[which(party_name_heuristics(xx['opn']) %in% pfcp_fast_munge(pfcp$name_other[pfcp$jnry_country %in% xx['jnry_country']]))], "name_other")
              }
            }
          }
        )


      #############################################################
      ## IF MORE THAN ONE MATCH in Party Facts data
      ## we may be looking at joint lists or something
      ## we can't unambiguously match the party
      ## WARN USER & REMOVE UNAMBIGUOUS MATCHES
      if (any((purrr::map_int(heur_pfcp_refs, length)) > 2)) {

        # warn user about ambiguous party matches
        cat("\nSome parties have multiple heuristic matches. These have been ignored.\nReview party identifiers for, e.g., cases of joint lists or other\nambiguities that would prevent identifying single parties.\nNOTE: This may cause bad fuzzy matches. Review those carefully.\n")

        # remove unambiguous matches
        col_heur_hits <- col_heur_hits %>%
          dplyr::filter(purrr::map_int(heur_pfcp_refs, length) < 3)
        heur_pfcp_refs <- heur_pfcp_refs[purrr::map_int(heur_pfcp_refs, length) < 3]
      }
      #############################################################


      #############################################################
      # fill in name matches and (if needed) add new 'name' columns to 'cons_refs'
      # (NB: can this loop be vectorized?? Maybe w/case_when? ... too many row-wise operations)
      for (i in 1:length(heur_pfcp_refs)) {
        # fix shorthad references to pfcp party name
        if (!heur_pfcp_refs[[i]][1] %in% pfcp[[heur_pfcp_refs[[i]][2]]]) {
          heur_pfcp_refs[[i]][1] <- pfcp[[heur_pfcp_refs[[i]][2]]][which(pfcp_munge(pfcp[[heur_pfcp_refs[[i]][2]]]) %>% purrr::map_lgl(function(k) heur_pfcp_refs[[i]][1] %in% k))[1]]
        }

        # put correct pfcp name spelling into cons_refs in appropriate field
        if (heur_pfcp_refs[[i]][2] %in% names(cons_refs)) {
          cons_refs[[heur_pfcp_refs[[i]][2]]] <- ifelse((cons_refs$opn %in% col_heur_hits$opn[i]),
                                                        heur_pfcp_refs[[i]][1],
                                                        cons_refs[[heur_pfcp_refs[[i]][2]]])
        } else {
          cons_refs[[heur_pfcp_refs[[i]][2]]] <- ifelse((cons_refs$opn %in% col_heur_hits$opn[i]),
                                                        heur_pfcp_refs[[i]][1],
                                                        NA)
        }
      }


      #############################################################
      ## get PF ids for heuristic matches --> cons_refs

      # which parties in 'cons_refs' have a PF name??
      has_pfcp_name <- cons_refs %>%
        dplyr::select(tidyselect::starts_with("name")) %>%
        as.matrix %>%
        apply(1, function(xx) sum(!is.na(xx))) %>%
        `>`(0)

      # dummy for **heuristic** matches
      cons_refs <- cons_refs %>% dplyr::mutate(match_heur = (has_pfcp_name & !match_exact))

      # split-merge-restack 'cons_refs' to get PF ids for heuristic matches
      # exact- & non-matched cases
      keep <- cons_refs %>%
        dplyr::filter(!match_heur) %>%
        dplyr::mutate(wikipedia = NA)

      # heuristic-matched cases
      to.match <- cons_refs %>%
        dplyr::filter(match_heur)

      # get name fields (carefully, in case more than one)
      # NB: also, don't create a conflict by naming a col "name*" in 'cons_refs'!!!!!!!!!!!!!!!
      cr_name_fields <- stringr::str_subset(names(cons_refs), "^name.+")

      # cycle over PF name cols now added to 'cons_refs'
      # then merge 'to.match' to pfcp
      if (length(cr_name_fields) == 1) {

        # vars to match on for name field
        pf_mtch_vars <- c(cr_name_fields,
                          "jnry_country", "jnry_year",
                          "partyfacts_id", "wikipedia")

        # merge 'to.match' to PF data
        to.match <- to.match %>%
          dplyr::select(-partyfacts_id) %>%
          dplyr::left_join(pfcp %>%
                             dplyr::select({{ pf_mtch_vars }}),
                           by = pf_mtch_vars[1:3]) # is this 'by' field dangerous? Only used to suppress messages and avoid incidental errors

        # rebuild 'cons_refs' by stacking 'keep' with 'to.match'
        cons_refs <- keep %>% dplyr::bind_rows(to.match)

        # CASE: heuristic party names match in more than one PF name field
      } else {

        # Error for apparently impossible case of zero-length 'cr_name_fields'
        if (length(cr_name_fields) == 0) stop("Error in executing heuristic matching.") # Will I ever see this error? Seems impossible...

        # gen list for loop output
        party.to.match <- list()

        # Loop over 'cr_name_fields'
        for (i in 1:length(cr_name_fields)) {

          # vars to match on for name field 'i'
          pf_mtch_vars <- c(cr_name_fields[i],
                            "jnry_country", "jnry_year",
                            "partyfacts_id", "wikipedia")

          # merge subset of 'to.match' to PF, using name field 'i'
          party.to.match[[i]] <- to.match %>%
            dplyr::filter(
              opn %in% col_heur_hits$opn[which(purrr::map_chr(heur_pfcp_refs, `[`, 2) %in% cr_name_fields[i])]
            ) %>%
            dplyr::select(-partyfacts_id) %>%
            dplyr::left_join(pfcp %>%
                               dplyr::select({{ pf_mtch_vars }}),
                             by = pf_mtch_vars[1:3])
        }

        # reconstitute 'to.match' frame
        to.match <- dplyr::bind_rows(party.to.match)

        # rebuild 'cons_refs' by stacking 'keep' with 'to.match'
        cons_refs <- keep %>% dplyr::bind_rows(to.match)
      }
    } else {
      cons_refs <- cons_refs %>% dplyr::mutate(match_heur = FALSE)
    }


    #############################################################
    # Check if we're done after completing heuristic matching
    if (all(cons_refs$match_exact | cons_refs$match_heur)) { # CASE: YES, COMPLETE AFTER HEURISTIC MATCHING

      #############################################################
      ## REPORT PROGRESS ON EXACT & HEURISTIC MATCHING METHODS
      cat(stringr::str_c("\n-----------------------------------------------------------\nFound ", sum(cons_refs$match_exact), " exact matches and\n", sum(cons_refs$match_heur)," heuristic matches of ", nrow(cons_refs), "\nparty x year observations.\n-----------------------------------------------------------\n"))


      #############################################################
      ## merge x to cons_refs

      # replace 'cons_refs' opn with original party_ref
      names(cons_refs) <- names(cons_refs) %>% stringr::str_replace("opn", party_ref)

      # characterize match
      cons_refs <- cons_refs %>%
        dplyr::mutate(
          jnry_match = ifelse(match_exact, "exact", NA),
          jnry_match = ifelse(match_heur, "heuristic", jnry_match),
          jnry_match = ifelse(is.na(jnry_match), "none", jnry_match)
        ) %>%
        dplyr::select(
          -match_exact,
          -match_heur
        )

      # fix Party Facts name(s)
      pf_names <- names(cons_refs)[stringr::str_detect(names(cons_refs), "^name")]

      if (length(pf_names) == 1) {
        cons_refs[["partyfacts_name"]] <- cons_refs[[pf_names]]
      } else {
        cons_refs[["partyfacts_name"]] <- NA
        for (ii in 1:length(pf_names)) {
          cons_refs[["partyfacts_name"]] <- ifelse(!is.na(cons_refs[[pf_names[ii]]]),
                                                   cons_refs[[pf_names[ii]]],
                                                   cons_refs[["partyfacts_name"]])
        }
      }

      cons_refs <- cons_refs %>%
        dplyr::select(-{{ pf_names }})

      # merging vars
      mrg_vars <- c(names(cons_refs)[1:3],
                    "partyfacts_id", "wikipedia", "jnry_match", "partyfacts_name")

      # execute merge
      x <- x %>%
        dplyr::left_join(
          cons_refs %>%
            dplyr::select({{ mrg_vars }}),
          by = mrg_vars[1:3]
        )

      # reattach quarantined NA obs
      if (exists("quar.x")) {
        x <- x %>% dplyr::bind_rows(quar.x)
        cat("\nNote that one or more NAs were present in party, country, or year variables.")
      }

      ## return orig data w/jnry ctry+year variables & partyfacts_id
      return(x)


      # close complete after heuristic matching
    } else { # CASE: NO, NOT DONE

      #############################################################
      #############################################################
      ## REPORT PROGRESS ON BOTH MATCHING METHODS ATTEMPTED SO FAR
      ## WARN THAT WE'RE MOVING TO FUZZY MATCHING NOW
      cat(stringr::str_c("\n-----------------------------------------------------------\nFound ", sum(cons_refs$match_exact), " exact matches and\n", sum(cons_refs$match_heur)," heuristic matches of ", nrow(cons_refs), " \nparty x year observations.\n-----------------------------------------------------------\n"))
      cat("\nParty identifier (party_ref) had some party names without exact\nor heuristic matches in Party Facts data.\n\n---Trying fuzzy matching---\n")
      #############################################################
      #############################################################

      ## DO THE FUZZY MATCHING -- HOPING DELAYS AREN'T TOO BAD

      # get parties still lacking matches
      unmatched <- cons_refs %>%
        dplyr:: mutate(match = match_exact | match_heur) %>%
        dplyr::group_by(opn) %>%
        dplyr::summarize(match = max(match)) %>%
        dplyr::filter(match == 0) %>%
        dplyr::pull(opn)

      # get their heuristic variations [is this step necessary? Seems prudent, but invites disagreements]
      fuzz_par_target <- stats::setNames(object = as.list(unmatched),
                                         nm = unmatched) %>%
        purrr::map(party_name_heuristics)

      # fuzzy match each to pfcp name fields
      pfcp_name_fields <- names(pfcp)[grepl("^name", names(pfcp))]

      # save each set of matches
      fuzz_matches <- list()

      # loop over possible name fields
      for (ii in 1:length(pfcp_name_fields)) {
        fuzz_matches[[ii]] <- purrr::map(fuzz_par_target,
                                         function(xx) {
                                           pfhs <- sapply(xx, function(xxx) agrep(xxx, unique(pfcp[[pfcp_name_fields[ii]]]), max.distance = 0.1))
                                           if (length(unlist(pfhs)) == 0) {return(NULL)} else {
                                             u <- unique(unlist(pfhs))
                                             tab <- tabulate(match(unlist(pfhs), u))
                                             return(u[tab == max(tab)])
                                           }
                                         }
        )
      }

      # name fuzzy match list items
      names(fuzz_matches) <- pfcp_name_fields

      # count successes in each col of pfcp
      fuzz_success <- fuzz_matches %>% purrr::map_int(function(xx) sum(!purrr::map_lgl(xx, is.null)))

      #############################################################
      # get best performing col for fuzzy matches
      if (sum(fuzz_success == max(fuzz_success)) > 1) {
        if ("name_english" %in% names(fuzz_success[which(fuzz_success == max(fuzz_success))])) {
          fuzz_best <- "name_english"
        } else if ("name" %in% names(fuzz_success[which(fuzz_success == max(fuzz_success))])) {
          fuzz_best <- "name"
        } else if ("name_other" %in% names(fuzz_success[which(fuzz_success == max(fuzz_success))])) {
          fuzz_best <- "name_other"
        } else {
          fuzz_best <- "name_short"
        }
      } else {
        fuzz_best <- names(fuzz_success[which(fuzz_success == max(fuzz_success))])
      }

      # are there any cases with multiple possible matches??
      fuzz_ct_matches <- fuzz_matches[[fuzz_best]] %>% purrr::map_int(length)

      # make sure 'fuzz_best' is in cons_refs
      if (!fuzz_best %in% names(cons_refs)) {
        cons_refs[[fuzz_best]] <- NA
      }

      #############################################################
      ## Loop again over parties left to fuzzy match -- put party names into cons_refs
      for (ii in 1:length(fuzz_ct_matches)) {
        # CASE: ONE POSITIVE FUZZY MATCH
        if (fuzz_ct_matches[ii] == 1) {
          cons_refs[[fuzz_best]] <- ifelse(cons_refs$opn %in% names(fuzz_ct_matches)[ii],
                                           unique(pfcp[[fuzz_best]])[fuzz_matches[[fuzz_best]][[ii]]],
                                           cons_refs[[fuzz_best]])
          # CASE: ZERO POSITIVE FUZZY MATCHES
        } else if (fuzz_ct_matches[ii] == 0) {
          next
          # CASE: MORE THAN ONE POSITIVE FUZZY MATCH
        } else {
          # get Party Facts names to check
          fuzz_to_check <- unique(pfcp[[fuzz_best]])[fuzz_matches[[fuzz_best]][[ii]]]

          # vector to store approximate string distances
          fuzz_adist <- rep(NA, length(fuzz_to_check))

          # loop over Party Facts names -- select lowest approx dist
          for (jj in 1:length(fuzz_to_check)) {
            # get distances
            fuzz_adist[jj] <- utils::adist(x = names(fuzz_ct_matches)[[ii]],
                                           y = fuzz_to_check[jj])
          }

          # if the answers are equally distant... just take the first (fix later?)
          if (sum(fuzz_adist == min(fuzz_adist)) > 1) {
            cons_refs[[fuzz_best]] <- ifelse(cons_refs$opn %in% names(fuzz_ct_matches)[ii],
                                             fuzz_to_check[1],
                                             cons_refs[[fuzz_best]])
          } else {
            # otherwise, get closest approx match
            cons_refs[[fuzz_best]] <- ifelse(cons_refs$opn %in% names(fuzz_ct_matches)[ii],
                                             fuzz_to_check[fuzz_adist == min(fuzz_adist)],
                                             cons_refs[[fuzz_best]])
          }
        }
      }

      # add in variables if not added under heuristic matching
      if (!'wikipedia' %in% names(cons_refs)) cons_refs[['wikipedia']] <- NA
      if (!'partyfacts_id' %in% names(cons_refs)) cons_refs[['partyfacts_id']] <- NA


      #############################################################
      ## split & merge the new fuzzy matches
      keep <- cons_refs %>%
        dplyr::filter(match_exact | match_heur) %>%
        dplyr::mutate(match_fuzzy = FALSE)

      # vars to match on
      pf_mtch_vars <- c(fuzz_best,
                        "jnry_country", "jnry_year",
                        "partyfacts_id", "wikipedia")

      # merge fuzzy matches (and remains) to 'pfcp'
      to.match <- cons_refs %>%
        dplyr::filter(!match_exact & !match_heur) %>%
        dplyr::select(-partyfacts_id, -wikipedia) %>%
        dplyr::left_join(
          pfcp %>%
            dplyr::select({{ pf_mtch_vars }}),
          by = pf_mtch_vars[1:3]
        ) %>%
        dplyr::mutate(match_fuzzy = !is.na(partyfacts_id))

      # reassemble cons_refs
      cons_refs <- keep %>% dplyr::bind_rows(to.match)

      # report progress
      cat(stringr::str_c("\n-----------------------------------------------------------\nFound ",
                         sum(cons_refs$match_exact), " exact matches,\n", sum(cons_refs$match_heur),
                         " heuristic matches, and\n", sum(cons_refs$match_fuzzy), " fuzzy matches of\n",
                         nrow(cons_refs), " party x year observations.\n-----------------------------------------------------------\n"))


      #############################################################
      ## merge x to cons_refs

      # replace 'cons_refs' opn with original party_ref
      names(cons_refs) <- names(cons_refs) %>% stringr::str_replace("opn", party_ref)

      # characterize match
      cons_refs <- cons_refs %>%
        dplyr::mutate(
          jnry_match = ifelse(match_exact, "exact", NA),
          jnry_match = ifelse(match_heur, "heuristic", jnry_match),
          jnry_match = ifelse(match_fuzzy, "fuzzy", jnry_match),
          jnry_match = ifelse(is.na(jnry_match), "none", jnry_match)
        ) %>%
        dplyr::select(
          -match_exact,
          -match_heur,
          -match_fuzzy
        )

      # fix Party Facts name(s)
      pf_names <- names(cons_refs)[stringr::str_detect(names(cons_refs), "^name")]

      if (length(pf_names) == 1) {
        cons_refs[["partyfacts_name"]] <- cons_refs[[pf_names]]
      } else {
        cons_refs[["partyfacts_name"]] <- NA
        for (ii in 1:length(pf_names)) {
          cons_refs[["partyfacts_name"]] <- ifelse(!is.na(cons_refs[[pf_names[ii]]]),
                                                   cons_refs[[pf_names[ii]]],
                                                   cons_refs[["partyfacts_name"]])
        }
      }

      cons_refs <- cons_refs %>%
        dplyr::select(-{{ pf_names }})

      # merging vars
      mrg_vars <- c(names(cons_refs)[1:3],
                    "partyfacts_id", "wikipedia", "jnry_match", "partyfacts_name")

      # execute merge
      x <- x %>%
        dplyr::left_join(
          cons_refs %>%
            dplyr::select({{ mrg_vars }}),
          by = mrg_vars[1:3]
        )

      # reattach quarantined NA obs
      if (exists("quar.x")) {
        x <- x %>% dplyr::bind_rows(quar.x)
        cat("\nNote that one or more NAs were present in party, country, or year variables.")
      }

      ## return orig data w/jnry ctry+year variables & partyfacts_id
      return(x)

      # close fuzzy matching
    }

    # close if/else of fallbacks after not finding ALL exact matches
  }

  # close function
}
