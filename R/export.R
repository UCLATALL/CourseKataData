#' Write CourseKata data to file
#'
#' Processed data from the course often has one or more list-columns in it. A
#' list-column is a column in a data frame where each of the rows (elements) in
#' that column is a `list`. These columns have some ambiguity in how they should
#' be written to file. As a sensible default, the function provides a mask for
#' \code{\link[utils:write.table]{write.csv}} where the data is first passed
#' through `convert_lists()`. See the details section for more information about
#' the conversion process.
#'
#' Data from CourseKata classes often has JSON (Javascript Object Notation) in
#' some of the columns. Though it is called *Javascript* object notation, this
#' is a standard and common format for sending and receiving data because it has
#' clear rules for representing various data structures. When this kind of data
#' is read-in using one of the `process_*()` functions like
#' \code{\link{process_data}}, it is converted to a `list`, which is a more
#' natural form of data in R. Unfortunately, when a data frame has a
#' list-column, there is ambiguity in how to write that column to file. JSON is
#' a sensible default because it does not lose any information in the conversion
#' process, even in more complex cases.
#'
#' If you would like to write your own converted, the export functions here will
#' let you specify one to use. An example of a custom converter and how to
#' specify it are provided below.
#'
#' @inheritParams utils::write.csv
#' @param converter An function that takes a list and returns a string. By
#'   default, the \code{\link[jsonlite:fromJSON]{toJSON}} from the `jsonlite`
#'   package is included and used.
#' @param ... Arguments passed on to the `converter` function.
#'
#' @return The converted data frame (where lists are formatted as strings) is
#'   returned. This makes it easier to use this function in a pipe
#'   (\code{\link[magrittr:pipe]{%>%}}).
#'
#' @export
#' @name export
#'
#' @examples
#'
#' # example table with a list-column
#' tbl_nested <- tibble::tibble(x = list(list(1, 2, 3), list(4, 5, 6)))
#'
#' # convert the column to a JSON string representation
#' convert_lists(tbl_nested)
#'
#' # custom converter that just comma-separates top-level values
#' to_comma <- function(x) paste0(x, collapse = ",")
#' convert_lists(tbl_nested, to_comma)
#'
#' # an example of a more deeply nested structure being converted to JSON
#' tbl_deep_nested <- tibble::tibble(x = list(
#'   # first element/row of column x
#'   list(
#'     list(1, 2, 3),
#'     list(4, 5, 6)
#'   ),
#'   # second element/row of column x
#'   list(
#'     list(7, 8, 9)
#'   )
#' ))
#' convert_lists(tbl_deep_nested)
write.csv <- function(x, file, na = "NA", row.names = FALSE, converter = jsonlite::toJSON, ...) {
  x <- convert_lists(x, ...)
  utils::write.csv(x, file, na = na, row.names = row.names)
  x
}

#' @rdname export
#' @export
convert_lists <- function(x, converter = jsonlite::toJSON, ...) {
  purrr::modify_if(x, rlang::is_list, function(list_col) {
    purrr::map_chr(list_col, converter, ...)
  })
}
