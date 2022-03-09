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

  load_data(object, "responses[.]csv$", class_id = class_id) %>%
    ensure_data_in_responses() %>%
    convert_types_in_responses(time_zone = time_zone) %>%
    map_response_options()
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
  ensure_columns(responses, required_cols, abort)

  # find missing data in the required columns
  missing_data_matrix <- tibble::as_tibble(responses)[required_cols] %>%
    purrr::modify(~ ifelse(trimws(.x) == "", NA, trimws(.x))) %>%
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
  integers <- c("attempt", "lrn_question_position")
  doubles <- c("points_possible", "points_earned")
  datetimes <- c("dt_submitted", "lrn_dt_started", "lrn_dt_saved")

  converted <- responses %>%
    # prevent problems if strings are factors
    purrr::modify(as.character) %>%
    # now do all type conversions
    purrr::modify_at(integers, parse_integer) %>%
    purrr::modify_at(doubles, parse_double) %>%
    purrr::modify_at(datetimes, parse_datetime, time_zone = time_zone)

  if (convert_json) {
    converted <- converted %>%
      purrr::modify_at("lrn_response_json", safe_convert_json)
  }

  attributes(converted) <- attributes(responses)
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
#' @return A responses table as a [`tibble`] with the lookup table added as the `option_value_table`
#'   attribute and the `response` column altered by mapping the responses using the table.
#'
#' @seealso process_responses
#' @export
map_response_options <- function(responses) {
  responses <- tibble::as_tibble(responses)

  # just easier to type
  lrn_ref <- "lrn_question_reference"

  # make sure there are responses and that we can find multiple-choice questions
  ensure_columns(responses, "response", rlang::abort, "No responses to map.")
  map_cols <- c("lrn_type", lrn_ref)
  if (!ensure_columns(responses, map_cols, rlang::warn, "Can\'t create look-up table.")) {
    return(responses)
  }

  # create the look-up table
  mc_rows <- responses[["lrn_type"]] == "mcq"
  col_pat <- paste0(lrn_ref, "|lrn_option_")
  mc_cols <- stringr::str_starts(names(responses), col_pat)
  lookup_table <- unique(responses[mc_rows, mc_cols]) %>%
    purrr::modify(as.character)

  # map the values
  responses[["response"]] <- as.character(responses[["response"]])
  has_response <- !is.na(responses[["response"]])
  valid <- responses[[lrn_ref]] %in% lookup_table[[lrn_ref]] & has_response

  responses[valid, "response"] <- purrr::map2_chr(
    responses[["response"]][valid], responses[[lrn_ref]][valid],
    map_response,
    lookup_table = lookup_table
  )

  structure(responses, option_value_table = lookup_table)
}


#' Map a response array to its values in a look-up table.
#'
#' If you are using this function you very likely understand why you need to use it and can figure
#' out what it does. It it is more likely that you want the more comprehensive function
#' [`process_responses`]. That said, give it a response, a question reference, and a look-up table,
#' and it will give you back the value from the look-up table that corresponds to the response.
#'
#' @param response A text array of response options, e.g. "\['1', '4'\]"
#' @param reference A reference that corresponds to a `lrn_question_reference`.
#' @param lookup_table A lookup table with a column `lrn_question_reference`, and a column for each
#'   possible response option.
#'
#' @return The value(s) indicated by the response options, separated by semicolons if applicable.
#'
#' @seealso process_responses
#' @export
map_response <- function(response, reference, lookup_table) {
  item_row <- match(reference, lookup_table[["lrn_question_reference"]])

  if (is.na(item_row)) {
    return(response)
  }

  if (response %in% c("[]", "")) {
    return(NA_character_)
  }

  option_numbers <- stringr::str_split(response, ",")[[1]] %>%
    parse_integer()
  lookup_table[item_row, option_numbers + 2] %>%
    unlist() %>%
    paste(collapse = "; ")
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
  item_map_lower <- stringr::str_to_lower(codebook[["item_id"]])
  stringr::str_to_lower(responses[["item_id"]]) %in% item_map_lower
}


quiz_item_map <- function(responses) {
  stringr::str_detect(responses[["item_id"]], ".*_Practice_Quiz.*")
}
