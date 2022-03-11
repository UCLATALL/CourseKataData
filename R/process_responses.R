#' Process responses from a CourseKata class data download
#'
#' The `process_responses()` function processes the responses found in the given object. The
#' responses are checked for required columns in `ensure_data_in_responses()`, the variables in the
#' outcome table are converted to appropriate types with `convert_types_in_responses()`, and
#' multiple-response option indices in `response` (`lrn_type = 'mcq'`) are mapped to their values
#' (`lrn_option_<index>`) with `map_response_options()`.
#'
#' @inheritParams process_auxiliary
#'
#' @return A [`tibble`] of all responses found in the given path or data frame. If there are
#'   multiple `responses.csv` files, they are merged together and can be distinguished by the
#'   `class_id` column. If `object` is a directory and `class_id` is specified, only the responses
#'   for the specified class or classes are included.
#'
#' @family processing functions
#' @export
process_responses <- function(object, time_zone = "UTC", class_id = NULL) {
  rlang::arg_match(time_zone, OlsonNames())
  stopifnot(is.character(class_id) || is.null(class_id))

  msg <- "Reading response data"
  raw <- load_data(object, "responses[.]csv$", class_id = class_id, progress_message = msg)
  raw %>%
    ensure_data_in_responses() %>%
    convert_types_in_responses(time_zone = time_zone) %>%
    map_response_options() %>%
    tibble::as_tibble()
}

#' Ensure a responses data frame has the required columns.
#'
#' A responses table isn't very useful without some basic information: the class the response was
#' elicited in, the student it was elicited from, and the prompt that elicited the response (but not
#' necessarily the response -- a missing response is a response in itself). This function ensures
#' that the columns exist in the data frame (failing with an error message if not) and filters out
#' rows that are missing required data (emitting a warning if so).
#'
#' @param responses A CourseKata responses table (e.g. from responses.csv).
#'
#' @return If no problems are found, the original object is returned. When problems are found, the
#'   corresponding rows are removed from the output table.
#'
#' @seealso process_responses
#' @export
ensure_data_in_responses <- function(responses) {
  required_cols <- c("class_id", "student_id", "prompt")
  # response is a required column, but they can all be blank, so this is the only check we need
  ensure_columns(responses, c(required_cols, "response"), abort)

  # find missing data in the required columns
  missing_data_matrix <- responses[, required_cols] %>%
    purrr::modify(function(x) {
      trimmed <- stringi::stri_trim_both(x)
      trimmed[stringi::stri_isempty(trimmed)] <- NA_character_
      trimmed
    }) %>%
    is.na()
  is_row_missing_data <- rowSums(missing_data_matrix) > 0
  nrow_missing <- sum(is_row_missing_data)

  # give off some useful warning if data is missing
  if (nrow_missing > 0) {
    rlang::inform(sprintf(
      "Dropped %d %s missing data at either class_id, student_id, or prompt. (This is OK.)",
      nrow_missing, ngettext(nrow_missing, "row", "rows")
    ))
  }

  # filter out the rows with missing data
  responses[!is_row_missing_data, ]
}


#' Convert the columns in a responses table to the appropriate types.
#'
#' `convert_types_in_responses()` takes a CourseKata responses table and returns a responses table
#'  with all of the same data except that the variables in the table are converted to appropriate
#'  types:
#'   - **integer**: `attempt`, `lrn_question_position`
#'   - **number**: `points_possible`, `points_earned`
#'   - **date-time**: `dt_submitted`, `lrn_dt_started`, `lrn_dt_saved`
#'   - **list**: `lrn_response_json`
#'   - **character**: all other columns not listed above
#'
#' @inheritParams process_auxiliary
#' @inheritParams ensure_data_in_responses
#'
#' @return A table of the same size as `responses` with an appropriate type for each variable.
#'
#' @seealso process_responses
#' @export
convert_types_in_responses <- function(responses, time_zone = "UTC", convert_json = FALSE) {
  original_attributes <- attributes(responses)
  integers <- c("attempt", "lrn_question_position")
  doubles <- c("points_possible", "points_earned")
  datetimes <- c("dt_submitted", "lrn_dt_started", "lrn_dt_saved")

  progress_bar <- init_progress("Converting column types", 4)
  # prevent problems if strings are factors
  converted <- purrr::modify(responses, as.character) %>%
    pipe_tick(progress_bar) %>%
    # now do all type conversions
    purrr::modify_at(integers, parse_integer) %>%
    pipe_tick(progress_bar) %>%
    purrr::modify_at(doubles, parse_double) %>%
    pipe_tick(progress_bar) %>%
    purrr::modify_at(datetimes, parse_datetime, time_zone = time_zone) #%>%
    # pipe_tick(progress_bar)

  if (convert_json) {
    progress_bar <- init_progress("Converting JSON to lists", nrow(converted))
    converted <- converted %>%
      purrr::modify_at("lrn_response_json", function(x) {
        progress_bar$tick()
        safe_convert_json(x)
      })
  }

  attributes(converted) <- original_attributes
  converted
}

#' Map multiple-choice options in a responses table to their values.
#'
#' Extract a look-up table for the multiple-choice questions in the set and then use it to map
#' response options to the values. The table is comprised of the `lrn_question_reference` and
#' `lrn_option_<option numbers>` columns, and it is added to the resulting responses table as the
#' `option_value_table` attribute.
#'
#' @inheritParams ensure_data_in_responses
#'
#' @return A responses table as a [`tibble`] with the look-up table added as the `option_value_table`
#'   attribute and the `response` column altered by mapping the responses using the table.
#'
#' @seealso process_responses
#' @export
map_response_options <- function(responses) {
  progress_bar <- init_progress("Mapping responses", nrow(responses))
  progress_bar$tick(0)

  # make sure that we can map multiple-choice questions
  msg <- "Unable to find multiple-choice questions. "
  has_required <- ensure_columns(responses, c("response", "lrn_type"), rlang::warn, msg)
  opt_col <- stringi::stri_detect_regex(names(responses), "^lrn_option_")
  if (!has_required || !any(opt_col)) {
    # nothing to map
    return(responses)
  }

  cols <- c("response", "lrn_type", names(responses)[opt_col])
  mapped <- purrr::pmap_chr(responses[cols], function(response, lrn_type, ...) {
    progress_bar$tick()

    if (lrn_type != "mcq") return(response)
    if (response %in% c("[]", "")) return(NA_character_)

    options <- list(...)
    option_indices <- stringi::stri_split_regex(response, ",")[[1]]
    # the multiple-choice values are 0-indexed, so add 1 to get the right R column
    paste(options[parse_integer(option_indices) + 1], collapse = "; ")
  })

  responses[["response"]] <- mapped
  responses
}

#' Ensure that a data frame has the required columns or signal with a message
#'
#' @param data The [`data.frame`] to check.
#' @param required_columns A character vector of columns to ensure.
#' @param signal The method used to signal if any of the columns are missing (e.g. [`abort`]).
#' @param message_prefix A string to prepend to the message.
#'
#' @return A logical indicating if all the columns were present (`TRUE`) or not (`FALSE`).
#' @keywords internal
ensure_columns <- function(data, required_columns, signal, message_prefix = "") {
  missing_required_cols <- !required_columns %in% names(data)
  if (any(missing_required_cols)) {
    signal(sprintf(
      "%sResponse table missing required %s: %s",
      message_prefix,
      ngettext(sum(missing_required_cols), "column", "columns"),
      paste0(required_columns[missing_required_cols], collapse = ", ")
    ))
  }

  invisible(!any(missing_required_cols))
}

#' Split a responses table into semantically distinct parts.
#'
#' The resulting responses table can be quite large and is filled with many different types of
#' responses. Pass the responses table to `split_responses()` to break the data into three distinct
#' components: pre- and post-course surveys, end-of-chapter quizzes, and in-text questions.
#'
#' @param responses A responses data frame (processed or not) from a CourseKata class.
#'
#' @return A list of [`tibble`] objects with three elements:
#'   1. `surveys`, for all responses from the course pre- and post-course surveys;
#'   2. `quizzes`, for all responses from practice quizzes in the course; and
#'   3. `in_text`, for all other items that appear in the textbook.
#'
#' @export
split_responses <- function(responses) {
  ensure_columns(responses, "item_id", stop, "Cannot split responses. ")
  surveys <- survey_item_map(responses)
  quizzes <- quiz_item_map(responses)
  groups <- ifelse(surveys, "surveys", ifelse(quizzes, "quizzes", "in_text"))
  split(tibble::as_tibble(responses), groups)
}


survey_item_map <- function(responses) {
  item_map_lower <- stringi::stri_trans_tolower(codebook[["item_id"]])
  stringi::stri_trans_tolower(responses[["item_id"]]) %in% item_map_lower
}


quiz_item_map <- function(responses) {
  stringi::stri_detect_regex(responses[["item_id"]], ".*_Practice_Quiz.*")
}
