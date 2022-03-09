#' Load CourseKata data from a directory, zipped directory, file path(s), or memory
#'
#' This function is useful for loading CourseKata files from various sources. As a general purpose
#' function, it has a few different parameters to work with. The most common usage will likely be
#' selecting specific types of files to load from a CourseKata download. To do this, pass in a path
#' (or a vector of paths) that points to a data download, and additionally use the `regexp` argument
#' to specify what kind of file you want (e.g. "responses"). The function will then load in all of
#' the "responses.csv" files it files at the locations you specified. You can also directly pass in
#' file names instead of the directory/pattern combination. Finally, you can optionally specify a
#' `class_id` if you know it.
#'
#' @param object The object to process the data from. This can be one of a variety of options:
#'   - the path(s) to a CourseKata data download zip file or the directory extracted from that zip
#'   - the path(s) to a specific CSV file (e.g. `classes.csv`)
#'   - a [`data.frame`]
#' @param regexp If `object` is a directory or zipped directory, a [`regex`] pattern indicating
#'   which file names to search for. The default value `'.*'` matches all files.
#' @param class_id (Optional) A character vector of the IDs of specific classes you want to extract
#'   data for. These values should correspond to the names of the directories where the desired CSV
#'   files can be found in the original data download.
#'
#' @return
#'   When a file path (or vector of file paths) is given, the files are read in and merged into a
#'   single table. Behind the scenes, we use `data.table` to process the data because it is very
#'   fast. However, the `data.table` interface is often unintuitive, so we convert the output to a
#'   [`tibble`], which is just a prettier version of a [`data.frame`].
#'
#'   When a directory or zip file is passed, the `regexp` argument is required to determine which
#'   files you want from that directory. In general, you can keep this simple as the CourseKata data
#'   directories are structured simply and consistently. Something like "responses" should reliably
#'   find the "responses.csv" files. If you are having troubles (perhaps the instructor included
#'   supplementary files called "student_responses_and_comments.csv") you can construct a more
#'   specific pattern (see [`regex`]). The matched files are read in just as if you had passed their
#'   specific paths to the function.
#'
#'   Sometimes it is useful to only extract data related to a specific class. To do this, you can
#'   use the `class_id` argument. If you include the ID of the class you want (you can get this from
#'   "classes.csv"), the `regexp` argument will be modified such that it only matches files from
#'   that class. For this reason, it isn't recommended to use the `class_id` in your `regexp` *and*
#'   use the `class_id` argument.
#'
#' @keywords internal
load_data <- function(object, regexp = '.*', class_id = NULL) {
  UseMethod("load_data", object)
}

load_data.data.frame <- function(object, regexp = ".*", class_id = NULL) {
  purrr::modify_if(tibble::tibble(object), is.factor, as.character)
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
  reader <- function(path) data.table::fread(
    file = path,
    stringsAsFactors = FALSE,
    showProgress = FALSE
  )
  tbl <- files %>%
    purrr::map(reader) %>%
    purrr::reduce(vctrs::vec_c) %>%
    tibble::as_tibble() %>%
    structure(".internal.selfref" = NULL)
}
