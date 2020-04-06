#' Get the path to the test data directory
#'
#' Check to see if you are running R interactively and then return the
#' appropriate relative path to the test data directory.
#'
#' @return The relative path to the test data directory.
data_dir <- function() {
  fs::path(if (interactive()) "tests" else "..", "data")
}


#' Get a file from the test data directory
#'
#' @param file_name The file to retrieve.
#'
#' @return The relative file path to the data file in the test data directory.
data_file <- function(file_name) {
  fs::path(data_dir(), file_name)
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

#' Expectation: does the object have variables with vctr properties?
#'
#' @param object data.frame to work within.
#' @param vctrs character vector of the variables to test.
#' @param ptype (Optional) Vector prototype to test against. Should be a size-0
#'   (empty) generalised vector.
#' @param size (Optional) Size to check for.
#'
#' @family expectations
#' @export
expect_vectors_in_df <- function(object, vctrs, ptype = NULL, size = NULL) {
  for (i in vctrs) testthat::expect_vector(object[[!!i]], ptype, size)
  invisible(object)
}
