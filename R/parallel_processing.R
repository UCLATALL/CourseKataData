#' Enable parallel processing.
#'
#' Many classes will have enormous responses tables that can take a lot of time
#' to process. The processing functions use the \code{furrr} package versions of
#' \code{purrr} functions. By default this will have no effect on the processing
#' of the data compared to just using \code{purrr}. However, if you would like
#' to enable parallel processing (and your computer supports) it, all you need
#' to do is set a \code{\link{plan}} with the \code{future} package. If you know
#' what you are doing, you can set that yourself, otherwise
#' \code{enable_parallel} will enable basic functionality and should improve the
#' speed of processing responses, and \code{disable_parallel} will set things
#' back to normal and close any background workers.
#'
#' @return If a new strategy is chosen, then the previous one is returned
#'   (invisible), otherwise the current one is returned (visibly).
#' @name parallel_processing
NULL

#' @rdname parallel_processing
#' @export
enable_parallel <- function() {
  future::plan("multiprocess")
}

#' @rdname parallel_processing
#' @export
disable_parallel <- function() {
  future::plan("sequential")
}
