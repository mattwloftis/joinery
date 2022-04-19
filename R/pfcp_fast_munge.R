
pfcp_fast_munge <- function(pfd_var) {
  stringr::str_trim(unlist(stringr::str_split(unique(pfd_var), "/")))
}
