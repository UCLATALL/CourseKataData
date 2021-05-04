#' Does the path point to a zip file?
#'
#' @param path A character vector of paths to check.
#'
#' @return A logical vector indicating the paths that are zip files.
#' @keywords internal
is_zip_file <- function(path) {
  fs::is_file(path) & fs::path_ext(path) == "zip"
}

#' Extract a zip file to a temporary directory.
#'
#' Vectorized over path and pattern.
#'
#' @param zip_file A character vector of the zip files to extract.
#' @param regexp Regular expressions for the files to extract from the zip file.
#'
#' @return A character vector of the directory where each zip file was extracted to.
#' @keywords internal
extract_to_temp <- function(path, regexp = ".*") {
  purrr::map2_chr(path, regexp, function(zip_file, regexp) {
    zip_list <- utils::unzip(zip_file, list = TRUE)[["Name"]]
    targets <- stringr::str_subset(zip_list, regexp)

    # remove possible bad directory with : in filenames
    safe_targets <- stringr::str_split_fixed(targets, "/", 2)[, 2]
    safe_directories <- fs::path_dir(safe_targets)

    # extract to a temporary directory
    temp_dir_name <- fs::path_ext_remove(fs::path_file(zip_file))
    temp_dir <- fs::dir_create(fs::path(tempdir(check = TRUE), temp_dir_name))
    purrr::walk2(targets, safe_directories, function(file_name, directory) {
      ex_dir <- fs::path(temp_dir, directory)
      utils::unzip(zip_file, file_name, junkpaths = TRUE, exdir = ex_dir)
    })

    temp_dir
  })
}


#' Get all files in all sub-directories
#'
#' @inheritParams fs::dir_ls
#' @keywords internal
dir_to_files <- function(dir_path, regexp) {
  fs::dir_ls(dir_path, regexp = regexp, type = "file", recurse = TRUE)
}
