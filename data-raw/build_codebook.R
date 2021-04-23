
get_codebook_for_version <- function(version) {
  version <- package_version(version)
  if (version < 2) {
    codebook_tbls <- get_codebook_v1(version)
  } else {
    codebook_tbls <- get_codebook_v2(version)
  }

  codebook_tbls %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(version = version) %>%
    dplyr::rename(var_name = `variable name`) %>%
    dplyr::select(version, item_id, lrn_question_reference, var_name)
}

get_codebook_v1 <- function(version) {
  url <- paste0("https://github.com/UCLATALL/CourseKataData/wiki/Codebook-v", version)
  rvest::read_html(url) %>%
    rvest::html_elements('table') %>%
    rvest::html_table()
}

get_codebook_v2 <- function(version) {
  url <- paste0("https://github.com/UCLATALL/CourseKataData/wiki/Codebook-v", version)
  tbls <- rvest::read_html(url) %>%
    rvest::html_elements('table') %>%
    rvest::html_table()

  # the odd tables are just info about where to find the surveys
  # the even tables are the code books
  is_even <- function(x) x %% 2 == 0
  keep <- seq_len(length(tbls)) %>% is_even()
  tbls[keep]
}

versions <- c("1.6", "1.7", "1.8", "2.2")
codebook <- purrr::map(versions, get_codebook_for_version) %>%
  purrr::reduce(vctrs::vec_c)

usethis::use_data(codebook, internal = TRUE, overwrite = TRUE)
