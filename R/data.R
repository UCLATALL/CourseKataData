#' Codebooks for surveys from Introductory Statistics: A Modeling Approach.
#'
#' There are multiple versions of Introductory Statistics: A Modeling Approach.
#' In each version there are some survey items that appear across different
#' versions of the book. These items are sometimes edited for typos, grouped
#' differently, or placed on different pages, though they largely remain
#' unchanged. When the items are edited on Learnosity, the item pool management
#' system used throughout the course, they are given a new reference ID, though
#' the question still asks the same question. To track the question across
#' different versions of the book, these codebooks provide consistent variable
#' names that do not change across versions. When you run any of the
#' `process_*()` functions, the `map_responses()` function is called to add
#' these variable names to the data being processed.
#'
#' @format Each codebook has the same structure, the reference ID for each
#'   question in the codebook, the consistent variable name for that question,
#'   and the prompt and response options (if any). These variables are as
#'   follows:
#'   \describe{
#'     \item{lrn_question_reference}{the unique ID for the question on
#'       Learnosity, and in the book; may or may not match the same question
#'       across versions}
#'     \item{var_name}{the unchanging name for the same question across
#'       different versions of the book}
#'     \item{prompt}{the prompt for the survey item}
#'     \item{lrn_option_<0-n>}{where applicable, the available response options
#'       for the question}
#'   }
#'
#' @name codebooks
#' @seealso \code{\link{process_responses}}, \code{\link{process_auxillary}}

#' @rdname codebooks
'codebook_1.6'
