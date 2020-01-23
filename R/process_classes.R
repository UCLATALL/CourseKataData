#' Process information about classes in a CourseKata class data download
#'
#' @param path The path to a data download zip file, the path to an extracted
#'   data download zip directory, or the path to a \code{classes.csv} file from
#'   a data download.
#'
#' @return A \code{\link{tibble}} of the classes found in the given path. The
#'   information is parsed from the first classes.csv file that is found (only
#'   one should exist).
#' @export
process_classes <- function(path) {
  path %>%
    load_data(paste0("classes[.]csv$", collapse = "|"), limit_search = 1) %>%
    dplyr::mutate_at("setup_yaml", ~ purrr::map(.x, jsonlite::parse_json))
}
