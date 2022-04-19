
pfcp_munge <- function(pfd_var) {
  stringr::str_split(pfd_var, "/") %>% map(stringr::str_trim)
}
