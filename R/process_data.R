#' Process the data from a CourseKata data download.
#'
#' This function loads and processes data from a CourseKata end-of-class
#' downloads. It functions by passing the path to each of the processing
#' functions in this package. The output data frames are all created in the
#' user's active environment as \code{classes}, \code{responses}, \code{items},
#' \code{page_views}, \code{media_views}, and \code{tags}. If any of the
#' variables already exist, the user is prompted to either allow overwriting or
#' abort the process (if not being run interactively, a message is emitted
#' noting that the variables were overwritten).
#'
#' @param path The path to a data download zip file or the path to an extracted
#'   data download directory.
#'
#' @return This function returns \code{TRUE} if it completes successfully, and
#'   \code{FALSE} if it is aborted.
#' @export
process_data <- function(path) {
  existing <- purrr::keep(
    c("classes", "responses", "page_views", "media_views", "items", "tags"),
    exists
  )

  if (length(existing) > 0) {
    var_string <- paste(existing, collapse = ", ")

    if (interactive()) {
      choice <- utils::menu(
        title = paste("These variables will be overwritten:", var_string),
        choices  = c("Abort.", "Overwrite them.", "Do not overwrite.")
      )

      if (choice != 2) {
        message("Data processing aborted.")
        invisible(FALSE)
      }
    }

    message(paste("Overwriting variables:", var_string))
  }

  assign("classes", process_classes(path), pos = 1)
  assign("responses", process_responses(path), pos = 1)
  assign("page_views", process_page_views(path), pos = 1)
  assign("media_views", process_media_views(path), pos = 1)
  assign("items", process_items(path), pos = 1)
  assign("tags", process_tags(path), pos = 1)

  invisible(TRUE)
}
