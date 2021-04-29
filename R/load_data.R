#' Load file(s) from a directory, zipped directory, or file path.
#'
#' @param object The object to process the data from. This can be one of a variety of options:
#'   - the path to a CourseKata data download zip file
#'   - the path to an extracted data download directory
#'   - the path to a specific CSV file (e.g. `classes.csv`)
#'   - the `data.frame` loaded from a CSV file (useful for writing unit tests)
#' @param regexp If `object` is a directory or zipped directory, a [`regex`] pattern indicating
#'   which file names to search for. The default value `'.*'` matches all files.
#' @param class_id (Optional) A character vector of the IDs of specific classes you want to extract
#'   data for. These values should correspond to the names of the directories where the desired CSV
#'   files can be found in the original data download.
#'
#' @return The resulting data frame will always be returned as a [`tibble`] with all factor
#'   variables converted to character. If the `object` is a `data.frame` it will still be converted
#'   to a `tibble`. If `object` is a path of some type, the files at the given location are filtered
#'   against `regexp` and `class_id`, and then read in. As the files are read, the rows of each
#'   table are merged int a single `tibble`.
#'
#' @export
load_data <- function(object, regexp = '.*', class_id = NULL) {
  UseMethod("load_data", object)
}

load_data.data.frame <- function(object, regexp = ".*", class_id = NULL) {
  tibble::as_tibble(object) %>%
    purrr::modify_if(is.factor, as.character)
}

load_data.character <- function(object, regexp = ".*", class_id = NULL) {
  object <- path_expand(object)

  if (!is_null(class_id)) {
    cls_pattern <- ifelse(class_id == "", class_id, paste0(class_id, "/"))
    regexp <- paste0(cls_pattern, regexp, collapse = "|")
  }

  # extract all zips and filter all directories down to a file list
  files_and_dirs <- purrr::map_if(object, is_zip_file, extract_to_temp, regexp = regexp)
  lists_of_files <- purrr::map_if(files_and_dirs, fs::is_dir, dir_to_files, regexp = regexp)
  files <- stringr::str_subset(purrr::flatten_chr(lists_of_files), regexp)

  if (length(files) == 0) abort(c(
    'No files were found matching the regexp/class_id combination given.\n',
    sprintf('Combined regular expression: %s', regexp)
  ))

  # read in and combine files
  dfs <- purrr::map(files, utils::read.csv, stringsAsFactors = FALSE)
  purrr::reduce(dfs, vctrs::vec_c) %>% load_data.data.frame()
}
