
# munge Party Facts data

pfcp_munge <- function(pfd_var) {
  stringr::str_split(pfd_var, "/") %>% purrr::map(stringr::str_trim)
}
