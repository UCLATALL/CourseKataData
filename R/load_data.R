#' Load file(s) from a directory, zipped directory, or file path.
#'
#' @param path The path to find the files at.
#' @param regexp If \code{path} is a directory or zipped directory, a
#'   \code{\link{regex}} pattern indicating which file names to search for.
#' @param limit_search If \code{path} is a directory or zipped directory, an
#'   integer indicating how many files to match, e.g. \code{1} indicates that
#'   only the first file found should be loaded. \code{NULL}, the default, uses
#'   all files that \code{regexp} matches.
#' @param class_id A character vector of the class ID(s) to load.
#'
#' @return A \code{\link{tibble}} of the data found at the path given, or if the
#'   path given is a directory or zipped directory, the paths matching
#'   \code{regexp}. If \code{limit_search} is specified, only that number of
#'   files will be read and merged. The files are read using
#'   \code{\link{map_dfr}} and \code{\link{read.csv}} with
#'   \code{stringsAsFactors} set to \code{FALSE}.
#'
#' @name load_data
NULL

#' @rdname load_data
#' @export
load_data <- function(path, regexp, limit_search = NULL) {
  if (fs::is_file(path) & fs::path_ext(path) == "zip") {
    ex_dir <- fs::path_temp(fs::path_ext_remove(fs::path_file(path)))
    files <- stringr::str_subset(zip::zip_list(path)$filename, regexp)
    zip::unzip(path, files = files, exdir = ex_dir)
    path <- ex_dir
  }

  if (fs::is_dir(path)) {
    path <- fs::dir_ls(path, regexp = regexp, recurse = TRUE)
    if (!purrr::is_empty(limit_search)) path <- path[1:limit_search]
  }

  furrr::future_map_dfr(path, utils::read.csv, stringsAsFactors = FALSE) %>%
    dplyr::as_tibble()
}

#' @rdname load_data
#' @export
load_class_data <- function(path, regexp, class_id, limit_search = NULL) {
  cls_pattern <- ifelse(class_id == "", class_id, paste0(class_id, "/"))
  pattern <- paste0(cls_pattern, regexp, collapse = "|")
  load_data(path, pattern, limit_search)
}
