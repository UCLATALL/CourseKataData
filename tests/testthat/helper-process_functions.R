#' Get the path to the test data directory
#'
#' Check to see if you are running R interactively and then return the
#' appropriate relative path to the test data directory.
#'
#' @param file_name The file to retrieve.
#'
#' @return The relative path to the test data directory, or the specified files there.
data_dir <- function (file_name = "") {
  fs::path(if (interactive()) "tests/testthat" else ".", "data", file_name)
}

unzipped_dir <- function(file_name = "") {
  dir <- data_dir("unzipped")
  if (!fs::dir_exists(dir)) {
    zip::unzip(data_dir("zipped.zip"), exdir = data_dir("unzipped"))
  }
  fs::path(dir, file_name)
}

class_dir <- function(file_name = "") {
  dir <- unzipped_dir("classes/class_1")
  fs::path(dir, file_name)
}


# Custom expectations -----------------------------------------------------

#' Expectation: does an object have the specified number of rows?
#'
#' @param object data.frame to test the number of rows using \code{\link{nrow}}
#' @param n Expected number of rows
#'
#' @family expectations
#' @export
expect_nrow <- function(object, n) {
  stopifnot(is.data.frame(object), is.numeric(n), length(n) == 1)

  act <- testthat::quasi_label(rlang::enquo(object))
  act$nrow <- nrow(act$val)

  testthat::expect(
    act$nrow == n,
    sprintf("%s has %i rows, not %i.", act$lab, act$nrow, n)
  )

  invisible(act$val)
}
