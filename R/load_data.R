#' Load file(s) from a directory, zipped directory, or file path.
#'
#' @param object The object to process the data from. This can be one of a
#'   variety of options:
#'   - the path to a CourseKata data download zip file
#'   - the path to an extracted data download directory
#'   - the path to a specific CSV file (e.g. `classes.csv`)
#'   - the `data.frame` loaded from a CSV file (useful for writing unit tests)
#' @param regexp If `object` is a directory or zipped directory, a
#'   \code{\link{regex}} pattern indicating which file names to search for.
#' @param limit_search If `object` is a directory or zipped directory, an
#'   integer indicating how many files to match, e.g. `1` indicates that only
#'   the first file found should be loaded. `NULL`, the default, uses all files
#'   that `regexp` matches.
#' @param class_id A character vector of the class ID(s) to load.
#'
#' @return If the `object` is a `data.frame` it is cast to a
#'   \code{\link{tibble}}. If `object` is a path of some type, the files at the
#'   given location are matched against `regexp`, `class_id`, and
#'   `limit_search`, and then the matched files are are read in. As the files
#'   are read, the rows of each table are appended to the rows of the previous
#'   tables, yielding one merged table. The resulting data frame is returned as
#'   a `tibble`.
#'
#' @export
load_data <- function(object, regexp, class_id = NULL, limit_search = NULL) {
  stopifnot(is.data.frame(object) || is.character(object))

  if (is.character(object)) {
    # add class ID to the regex pattern
    if (!is.null(class_id)) {
      cls_pattern <- ifelse(class_id == "", class_id, paste0(class_id, "/"))
      pattern <- paste0(cls_pattern, regexp, collapse = "|")
      return(load_data(object, pattern, limit_search = limit_search))
    }

    # unzip compressed archive
    if (fs::is_file(object) && fs::path_ext(object) == "zip") {
      zip_list <- utils::unzip(object, list = TRUE)[["Name"]]
      targets <- stringr::str_subset(zip_list, regexp)

      # remove possible bad directory with : in filenames
      safe_targets <- stringr::str_split_fixed(targets, "/", 2)[, 2]
      safe_directories <- fs::path_dir(safe_targets)

      # extract to a temporary directory
      temp_dir <- fs::path_ext_remove(fs::path_file(object)) %>% fs::path_temp()
      purrr::walk2(targets, safe_directories, function(file_name, directory) {
        ex_dir <- fs::path(temp_dir, directory)
        utils::unzip(object, file_name, junkpaths = TRUE, exdir = ex_dir)
      })

      # set the path to the ex_dir so it gets picked up in next if block
      object <- temp_dir
    }

    if (fs::is_dir(object)) {
      object <- fs::dir_ls(object, regexp = regexp, recurse = TRUE)
      if (!is.null(limit_search)) object <- object[1:limit_search]
    }

    object <- purrr::map(object, utils::read.csv, stringsAsFactors = FALSE) %>%
      purrr::reduce(vctrs::vec_c)
  }

  tibble::as_tibble(object) %>%
    purrr::modify_if(is.factor, as.character)
}
