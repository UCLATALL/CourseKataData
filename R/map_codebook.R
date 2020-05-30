#' @rdname codebooks
#'
#' @param object The data frame to add variable names to.
#' @param quiet Should errors and messages be suppressed?
#' @param release A character vector of the codebook release versions  to use.
#'   These are combined into a single look up table when the function is run.
#'
#' @return `map_codebook()` will convert the `object` to a \code{\link{tibble}}
#'   and add a new variable, `var_name`, which gives a version independent name
#'   to the to each item in the `object` with a matching
#'   `lrn_question_reference` in the specified codebook(s).
#' @export
map_codebook <- function(object, quiet = FALSE, release = NULL) {
  tbl <- tibble::as_tibble(object)

  if (is.null(release)) {
    release <- guess_release(object, quiet)
  }

  if (all(is.na(release))) {
    return(tbl)
  }

  codebooks <- purrr::map(unique(release), get_codebook)
  codebook <- unique(do.call(vctrs::vec_c, codebooks))

  var_name_indices <- match(
    tbl[['lrn_question_reference']],
    codebook[['lrn_question_reference']]
  )

  tbl[['var_name']] <- codebook[['var_name']][var_name_indices]

  if (!quiet) rlang::inform(paste0(
    'Take a look at the codebook(s) for more information about the survey ',
    'items. Codebooks for these releases were used for these data: \n',
    paste0('\n- ', unique(release)), '\n\n',
    'Type codebook_<release> to view the relevant codebook, e.g. "codebook_1.6"'
  ))

  tbl
}

guess_release <- function(object, quiet = FALSE) {
  if (is.null(object[['release']])) {
    if (!quiet) {
      rlang::abort(stringr::str_squish(
        'Unable to guess which codebook to use. The "release" column is missing
        from your data.'))
    }

    return(NA_character_)
  }

  stringr::str_trim(
    stringr::str_remove(object[['release']], '^release/v?')
  )
}

get_codebook <- function(release) {
  tryCatch({
    name_call <- paste0('CourseKataData::codebook_', release)
    df <- eval(parse(text = name_call))
    tibble::as_tibble(df)
  }, error = function(c) {
    rlang::abort(stringr::str_squish(sprintf(
      'The release you specified, `%s`, could not be found.
      Type `?codebooks` for a list of the codebooks included in
      this package.', release)))
  })
}
