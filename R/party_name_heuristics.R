
# heuristic transformations of party names

party_name_heuristics <- function(xx) {
  # start by spliting on any '/'s and replacing the party name
  # with both options
  if (length(xx) == 1 && stringr::str_detect(xx, "/")) {
    xx <- stringr::str_split(xx, "/") %>% unlist() %>% stringr::str_trim()
  }

  # define regular expressions for heuristic transformations
  party_regex_patterns <- c(
    " ?Party",
    "\\b[Tt]he\\b ?",
    "Democratic",
    "\\'",
    "Democrats",
    "[Pp]eoples",
    "[[:punct:]]"
  )

  # define replacements for respective regexs
  party_regex_replacements <- c(
    "",
    "",
    "Democrats",
    "",
    "Democratic",
    "People's",
    ""
  )

  # append all new party name formulations to vector of heuristic
  # transformations
  for (j in seq_along(party_regex_patterns)) {
    if (!any(stringr::str_replace_all(xx,
                                      party_regex_patterns[j],
                                      party_regex_replacements[j]) %in% xx)) {
      tmp <- xx %>% stringr::str_replace_all(party_regex_patterns[j],
                                             party_regex_replacements[j])
      xx <- c(xx, tmp[!(tmp %in% xx)])
    } else {
      xx
    }
  }

  xx <- unique(xx)

  return(xx)
}
