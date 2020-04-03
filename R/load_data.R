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
#' @export
load_data <- function(path, regexp, class_id = NULL, limit_search = NULL) {
  if (!is.null(class_id)) {
    cls_pattern <- ifelse(class_id == "", class_id, paste0(class_id, "/"))
    pattern <- paste0(cls_pattern, regexp, collapse = "|")
    return(load_data(path, pattern, limit_search = limit_search))
  }

  if (fs::is_file(path) & fs::path_ext(path) == "zip") {
    zip_list <- utils::unzip(path, list = TRUE)[["Name"]]
    targets <- stringr::str_subset(zip_list, regexp)

    # remove possible bad directory with : in filenames
    safe_targets <- stringr::str_split_fixed(targets, "/", 2)[, 2]
    safe_directories <- fs::path_dir(safe_targets)

    # extract to a temporary directory
    temp_dir <- fs::path_ext_remove(fs::path_file(path)) %>% fs::path_temp()
    purrr::walk2(targets, safe_directories, function(file_name, directory) {
      ex_dir <- fs::path(temp_dir, directory)
      utils::unzip(path, file_name, junkpaths = TRUE, exdir = ex_dir)
    })

    # set the path to the ex_dir so it gets picked up in next if block
    path <- temp_dir
  }

  if (fs::is_dir(path)) {
    path <- fs::dir_ls(path, regexp = regexp, recurse = TRUE)
    if (!purrr::is_empty(limit_search)) path <- path[1:limit_search]
  }

  purrr::map_dfr(path, utils::read.csv, stringsAsFactors = FALSE) %>%
    tibble::as_tibble()
}


load_object <- function(object, type = NULL, class_id = NULL, limit_search = NULL) {
  stopifnot(is.data.frame(object) || is.character(object))

  # by default assume we are getting a data frame,
  # but if we get a string instead...
  if (is.character(object)) {
    file_name <- sprintf("%s[.]csv$", type)
    object <- load_data(object, file_name, class_id, limit_search)
  }

  tibble::as_tibble(object) %>%
    purrr::modify(as.character)
}
