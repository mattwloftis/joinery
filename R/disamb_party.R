
#' Disambiguate political party names + match to Party Facts ID
#'
#' @param x Data frame containing party
#' @param party_ref A string identifying the variable in the data, \code{x}, containing political party names or other party identifiers
#' @param country A string identifying the country, if single-country data, or vector of strings identifying the country of each observation if multi-country data.
#' @param year A four-digit numeric identifying the year of the data, or a string identifying the variable in \code{x} containing the year variable (four-digit numeric). If \code{year = NULL}, assumes the current calendar year.
#' @param origin A string identifying the origin format of country names (see \code{countrycode} package). If \code{origin = NULL}, attempts to resolve the origin format automatically.
#'
#' @return A tibble, \code{x}, with a new column for Party Facts id
#' @export
#'
#' @importFrom magrittr `%>%`
#' @examples
#'


disamb_party <- function(x, party_ref, country, year = NULL, origin = NULL) {

  #############################################################
  ## SANITY CHECK INPUT: x

  # x must be data frame / tibble
  if (!("data.frame" %in% class(x))) stop("x must be a data frame or tibble.")


  #############################################################
  ## SANITY CHECK INPUT: party_ref

  # party_ref needs to be an internal-to-data-environment reference!!!
  if (!(is.atomic(party_ref) &&
        length(party_ref) == 1L) &&
      is.character(party_ref) &&
      (party_ref %in% names(x))) stop("party_ref must be a string identifying the variable in the data containing the political party names or other party identifiers")



  #############################################################
  ## SANITY CHECK INPUT: origin
  if (!is.null(origin)) { # CASE: origin *not* NULL
    # reject non-valid 'origin' specifications
    if (!(origin %in% c("cctld", "country.name",
                        "country.name.de", "cowc", "cown", "dhs", "ecb",
                        "eurostat", "fao", "fips", "gaul", "genc2c", "genc3c",
                        "genc3n", "gwc", "gwn", "imf", "ioc", "iso2c", "iso3c",
                        "iso3n", "p4c", "p4n", "un", "un_m49", "unicode.symbol",
                        "unhcr", "unpd", "vdem", "wb", "wb_api2c", "wb_api3c",
                        "wvs", "country.name.en.regex", "country.name.de.regex"))) {
      stop("origin must be a valid orgin for countrycode function. See help(countrycode). If origin is not specified, this function will try to resolve a valid input for 'origin'.")
      # In future: change this to attempt to resolve valid origin if
      # provided origin is non-valid -- warn user when this is done
    }

    clean_origin <- origin
  }

  # if origin is NULL, attempt to resolve country format
  if (is.null(origin)) {
    origin_hits <- purrr::map_lgl(countrycode::codelist, function(xx) all(country %in% xx))
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
    } else {
      stop("Cannot resolve origin format of country name or identifier. Try explicitly specifying origin format & checking country identifiers for errors/misspellings.")
    }

    cat(paste0("\nCountrycode origin format not provided. Assuming: ", "\"", clean_origin, "\" (See countrycode package)\n"))
  }


  #############################################################
  ## SANITY CHECK INPUT: country

  # country is vector or scalar
  if (!(is.atomic(country)) ||  length(country) == 0) stop("Country must be a string or vector of strings")



  #############################################################
  ## CLEAN INPUT: country
  x <- x %>%
    dplyr::mutate(jnry_country = countrycode::countrycode(
      sourcevar = country,
      origin = clean_origin,
      destination = "iso3c"
    )) %>%
    tibble::as_tibble()


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
      dplyr::mutate(jnry_year = Sys.Date() %>% str_extract("^\\d{4}"))

    # warn user that year --> current year
    warning("No year provided. Defaulting to current calendar year.\n")

  } else if (!is.atomic(year) || length(year) == 0) {

    # if year is provided, but it is not the correct class or is empty --> BREAK
    stop("year not provided correctly. If a four-digit numeric scalar, year will be applied to all observations. If a numeric vector, year will be applied to each observation. If a character object, year must refer to a variable in the data. If NULL, year is assumed to be the current year.")

  } else if (is.numeric(year)) { # CASE: numeric

    # catch if any numeric year is not four digits (i.e. somebody mistyped?)
    if (!all(str_length(year) == 4)) stop("year not provided correctly. If a four-digit numeric scalar, year will be applied to all observations. If a numeric vector, year will be applied to each observation. If a character object, year must refer to a variable in the data. If NULL, year is assumed to be the current year.")

    # CASE: vector
    # if year is numeric vector, catch incorrect length (i.e. length != nrow(x))
    if (length(year) > 1 && (length(year) != nrow(x))) {
      stop("If year is a vector, it must be the same length as the number of rows in the data")
    }

    # if no other errors, put the year into the data (x)
    x <- x %>% dplyr::mutate(jnry_year = year)

  } else if (is.character(year) && (length(year) == 1)) { # CASE: character

    # if year is character scalar, get that column in the data
    year <- x[[year]]

    # catch years not four digits
    if (!is.numeric(year) || !all(str_length(year) == 4)) stop("year not provided correctly -- must be numeric and consist of only four-digit numbers")

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
  pfcp <- read.csv(file = "https://partyfacts.herokuapp.com/download/core-parties-csv/",
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
    dplyr::group_by(across({{ grpng_vars }})) %>%
    dplyr::summarize() %>%
    dplyr::ungroup()

  # give party ref in this consistent reference dataset a fixed name
  names(cons_refs)[1] <- "opn" # "original party name"
  #############################################################



  #############################################################
  ## FIND PartyFacts match column
  # 1) look for easy *exact* matches
  party_hits <- pfcp %>%
    dplyr::select(starts_with('name')) %>%
    purrr::map_lgl(function(xx) all(unique(cons_refs$opn) %in% xx))


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
          select({{ pf_mtch_vars }}),
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
      left_join(
        cons_refs %>%
          select({{ mrg_vars }}),
        by = mrg_vars[1:3]
      )

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
      dplyr::select(starts_with('name')) %>%
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

    # regex reconstruct TARGET party names
    heur_par_target <- stats::setNames(object = as.list(unique(cons_refs$opn[!cons_refs$match_exact])),
                                nm = unique(cons_refs$opn[!cons_refs$match_exact])) %>%
      purrr::map(party_name_heuristics)

    # apply heuristic regex tranforms & check success by party
    any_heur_success <- heur_par_target %>% purrr::map_lgl(
      function(p_heuristics) {
        any(
          purrr::map_lgl(pfcp %>% dplyr::select(starts_with('name')),
                         function(pf_names) any(p_heuristics %in% pfcp_fast_munge(pf_names)))
        )
      }
    ) %>%
      which

    # get PF columns for parties w/heur success
    # NB: if 'any_heur_success' comes up empty, this still runs w/o error
    # col_heur_hits just ends up having zero rows --> catch this below to check
    # for any heuristic match successes!
    col_heur_hits <- heur_par_target[names(heur_par_target) %in% names(any_heur_success)] %>%
      purrr::map(
        function(p_heuristics) {
          purrr::map_lgl(pfcp %>% select(starts_with('name')),
                         function(pf_names) any(p_heuristics %in% pfcp_fast_munge(pf_names)))
        }
      ) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(opn = names(heur_par_target)[names(heur_par_target) %in% names(any_heur_success)])

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
            if (exists("best_pf_name_var") && (best_pf_name_var %in% names(xx[as.logical(xx)])) ) {
              c(party_name_heuristics(xx['opn'])[which(party_name_heuristics(xx['opn']) %in% pfcp_fast_munge(pfcp[[best_pf_name_var]]))], best_pf_name_var)
            } else {
              if ("name_english" %in% names(xx[as.logical(xx)])) {
                c(party_name_heuristics(xx['opn'])[which(party_name_heuristics(xx['opn']) %in% pfcp_fast_munge(pfcp$name_english))], "name_english")
              } else if ("name" %in% names(xx[as.logical(xx)])) {
                c(party_name_heuristics(xx['opn'])[which(party_name_heuristics(xx['opn']) %in% pfcp_fast_munge(pfcp$name))], "name")
              } else if ("name_short" %in% names(xx[as.logical(xx)])) {
                c(party_name_heuristics(xx['opn'])[which(party_name_heuristics(xx['opn']) %in% pfcp_fast_munge(pfcp$name_short))], "name_short")
              } else {
                c(party_name_heuristics(xx['opn'])[which(party_name_heuristics(xx['opn']) %in% pfcp_fast_munge(pfcp$name_other))], "name_other")
              }
            }
          }
        )

      # fill in name matches and (if needed) add new 'name' columns to 'cons_refs'
      # (NB: can this loop be vectorized?? Maybe w/case_when? ... too many row-wise operations)
      for (i in 1:length(heur_pfcp_refs)) {
        # fix shorthad references to pfcp party name
        if (!heur_pfcp_refs[[i]][1] %in% pfcp[[heur_pfcp_refs[[i]][2]]]) {
          heur_pfcp_refs[[i]][1] <- pfcp[[heur_pfcp_refs[[i]][2]]][which(pfcp_munge(pfcp[[heur_pfcp_refs[[i]][2]]]) %>% map_lgl(function(k) heur_pfcp_refs[[i]][1] %in% k))[1]]
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
        dplyr::select(starts_with("name")) %>%
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
                               select({{ pf_mtch_vars }}),
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
            fuzz_adist[jj] <- adist(x = names(fuzz_ct_matches)[[ii]],
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
        select(-{{ pf_names }})

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

      ## return orig data w/jnry ctry+year variables & partyfacts_id
      return(x)


      # close fuzzy matching
    }

    # close if/else of fallbacks after not finding ALL exact matches
  }

  # close function
}
