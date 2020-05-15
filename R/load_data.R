#' Load file(s) from a directory, zipped directory, or file path.
#'
#' @param object The object to process the data from. This can be one of a
#'   variety of options:
#'   - the path to a CourseKata data download zip file
#'   - the path to an extracted data download directory
#'   - the path to a specific CSV file (e.g. `classes.csv`)
#'   - the `data.frame` loaded from a CSV file (useful for writing unit tests)
#' @param regexp If `object` is a directory or zipped directory, a
#'   \code{\link{regex}} pattern indicating which file names to search for. The
#'   default value `'.*'` matches all files.
#' @param class_id (Optional) A character vector of the IDs of specific classes
#'   you want to extract data for. These values should correspond to the names
#'   of the directories where the desired CSV files can be found in the original
#'   data download.
#'
#' @return The resulting data frame will always be returned as a
#'   \code{\link{tibble}} with all factor variables converted to character. If
#'   the `object` is a `data.frame` it will still be converted to a `tibble`. If
#'   `object` is a path of some type, the files at the given location are
#'   filtered against `regexp` and `class_id`, and then read in. As the files
#'   are read, the rows of each table are merged int a single `tibble`.
#'
#' @section TODO: It might be useful to vectorize on all args not just `object`.
#'   For `class_id` this would mean allowing a character vector (non-vectorized
#'   method) or a list of character vectors (vectorizing on the list). This
#'   seems better accomplished using something like `purrr` though.
#'
#' @export
load_data <- function(object, regexp = '.*', class_id = NULL) {
  stopifnot(is.data.frame(object) || is.character(object))

  # add class ID to the regex pattern and start over
  if (is.character(object) && !is.null(class_id)) {
    cls_pattern <- ifelse(class_id == "", class_id, paste0(class_id, "/"))
    new_regexp <- paste0(cls_pattern, regexp, collapse = "|")

    return(load_data(object, new_regexp))
  }

  if (is.character(object)) {
    object <- fs::path_expand(object)

    # predicate for map_if
    is_zip_file <- function(path) {
      fs::is_file(path) & fs::path_ext(path) == 'zip'
    }

    # functional for map_if
    # inner regexp is pulled from load_data args
    extract_to_temp <- function(zip_file) {
      zip_list <- utils::unzip(zip_file, list = TRUE)[["Name"]]
      targets <- stringr::str_subset(zip_list, regexp)

      # remove possible bad directory with : in filenames
      safe_targets <- stringr::str_split_fixed(targets, "/", 2)[, 2]
      safe_directories <- fs::path_dir(safe_targets)

      # extract to a temporary directory
      temp_dir <- fs::path_ext_remove(fs::path_file(zip_file)) %>% fs::path_temp()
      purrr::walk2(targets, safe_directories, function(file_name, directory) {
        ex_dir <- fs::path(temp_dir, directory)
        utils::unzip(zip_file, file_name, junkpaths = TRUE, exdir = ex_dir)
      })

      temp_dir
    }

    # functional for map_if
    # inner regexp pulled from load_data args
    dir_to_files <- function(dir_path) {
      fs::dir_ls(dir_path, regexp = regexp, type = 'file', recurse = TRUE)
    }

    # extract all zips and filter all dirs down to a file list
    files_and_dirs <- purrr::map_if(object, is_zip_file, extract_to_temp)
    lists_of_files <- purrr::map_if(files_and_dirs, fs::is_dir, dir_to_files)
    files <- stringr::str_subset(purrr::flatten_chr(lists_of_files), regexp)

    if (length(files) == 0) rlang::abort(paste0(
      'No files were found matching the regexp/class_id combination given.\n',
      '  combined regular expression: ', regexp
    ))

    # read in and combine files
    dfs <- purrr::map(files, utils::read.csv, stringsAsFactors = FALSE)
    object <- purrr::reduce(dfs, vctrs::vec_c)
  }

  purrr::modify_if(tibble::as_tibble(object), is.factor, as.character)
}
