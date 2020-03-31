#' Process responses from a CourseKata class data download
#'
#' This function processes the responses found in the given object. The
#' responses are checked for required columns
#' (\code{\link{ensure_data_in_responses}}), the variables in the outcome table
#' are converted to appropriate types
#' (\code{\link{convert_types_in_responses}}), and multiple-response option
#' indices in the \code{response} column for \code{lrn_type = "mcq"} are mapped
#' to their values (in the \code{lrn_option_<index>} columns
#' (\code{\link{map_response_options}}).
#'
#' @param object The path to a data download zip file, the path to an extracted
#'   data download zip directory, the path to a \code{responses.csv} file from a
#'   data download, or a responses-like \code{data.frame}.
#' @param time_zone The time zone to use when parsing datetime objects (see
#'   \code{\link{timezones}}).
#' @param class_id (Optional) A character vector of the IDs of specific classes
#'   for which to extract responses. These values should correspond to the names
#'   of the directories where the desired \code{responses.csv} files can be
#'   found (this is how the zip file is structured when downloading from
#'   CourseKata).
#'
#' @return A \code{\link{tibble}} of all responses found in the given path or
#'   data frame. If there are multiple \code{responses.csv} files, they are
#'   merged together and can be distinguished by the \code{class_id} column. If
#'   \code{object} is a directory and \code{class_id} is specified, only the
#'   responses for the specified class or classes are included.
#'
#' @name process_responses

#' @rdname process_responses
#' @export
process_responses <- function(object, time_zone = "UTC", class_id = "") {
  # by default assume we are getting a data frame,
  # but if we get a string instead...
  if (is.character(object)) {
    object <- load_class_data(object, "responses[.]csv$", class_id)
  }

  # process the object
  object %>%
    dplyr::as_tibble() %>%
    ensure_data_in_responses() %>%
    convert_types_in_responses() %>%
    map_response_options()
}


#' Convert the columns in a responses table to the appropriate types.
#'
#' @param responses A CourseKata responses table (e.g. from responses.csv).
#' @param time_zone The time zone to use when parsing date-times. See
#'   \code{\link{timezones}} for more information about valid time zones.
#'
#' @return This function returns the same responses table where the variables in
#'   the table have all been converted to appropriate types. See the **Details**
#'   section for information about which variables are converted to which type.
#'
#' @details
#' \describe{
#'   \item{integer}{\code{attempt, lrn_question_position}}
#'   \item{number}{\code{points_possible, points_earned}}
#'   \item{datetime}{\code{dt_submitted, lrn_dt_started, lrn_dt_saved}}
#'   \item{list}{\code{lrn_response_json}}
#'   \item{character}{all other columns in the data frame not listed above}
#' }
#'
#' @rdname process_responses
#' @export
convert_types_in_responses <- function(responses, time_zone = "UTC") {
  responses %>%
    dplyr::mutate_all(as.character) %>% # prevent probs from stringsAsFactors
    mutate_at_if_exists(
      c("attempt", "lrn_question_position"),
      readr::parse_integer
    ) %>%
    mutate_at_if_exists(
      c("points_possible", "points_earned"),
      readr::parse_number
    ) %>%
    mutate_at_if_exists(
      c("dt_submitted", "lrn_dt_started", "lrn_dt_saved"),
      readr::parse_datetime,
      locale = readr::locale(tz = time_zone)
    ) %>%
    mutate_at_if_exists(
      c("lrn_response_json"),
      safe_convert_json
    ) %>%
    set_attributes(attributes(responses))
}


#' Ensure a responses data frame has the required columns.
#'
#' A responses table isn't very useful without some basic information: the class
#' the response was elicited in, the student it was elicited from, and the
#' prompt that elicited the response (but not necessarily the response -- a
#' missing response is a response in itself). This function ensures that the
#' columns exist in the data frame (failing with an error message if not) and
#' filters out rows that are missing required data (emitting a warning if so).
#'
#' @param responses A CourseKata responses table (e.g. from responses.csv).
#'
#' @return If no problems are found, the original responses table is returned.
#'   Otherwise, a responses table where all rows contain the required
#'   informaiton.
#'
#' @rdname process_responses
#' @export
ensure_data_in_responses <- function(responses) {
  required_cols <- c("class_id", "student_id", "prompt")
  ensure_columns(responses, required_cols, stop)

  # find missing data in the required columns
  missing_data_matrix <- responses %>%
    dplyr::select(required_cols) %>%
    dplyr::mutate_all(function(x) {
      stringr::str_trim(x) %>% stringr::str_replace("^$", NA_character_)
    }) %>%
    is.na()
  is_row_missing_data <- rowSums(missing_data_matrix) > 0
  nrow_missing_data <- sum(is_row_missing_data)

  # give off some useful warning if data is missing
  if (nrow_missing_data > 0) {
    general_warning <- sprintf(
      "Dropped %d %s:",
      nrow_missing_data,
      pluralize("row", nrow_missing_data > 1)
    )

    specific_warnings <- as.data.frame(missing_data_matrix) %>%
      furrr::future_imap_chr(function(is_na, name) {
        rows <- which(is_na) %>% paste(collapse = ", ")
        if (nchar(rows) == 0) {
          ""
        } else {
          sprintf(
            "\n - missing %s at %s %s",
            name,
            pluralize("row", nchar(rows) > 1),
            rows
          )
        }
      })

    warning(c(general_warning, specific_warnings) %>% paste0(collapse = ""))
  }

  # filter out the rows with missing data
  responses %>%
    dplyr::slice(which(!is_row_missing_data))
}


#' Map multiple-choice options in a responses table to their values.
#'
#' Extract a lookup table for the multiple-choice questions in the set and then
#' use it to map response options to the values. The table is comprised of the
#' \code{lrn_question_reference} and \code{lrn_option_<option numbers>} columns,
#' and it is added to the resulting respnoses table as the
#' \code{option_value_table} attribute.
#'
#' @param responses A CourseKata responses table (e.g. from responses.csv).
#'
#' @return The responses table with the lookup table added as the
#'   \code{option_value_table} attribute and the \code{response} column altered
#'   by mapping the responses using the table.
#'
#' @rdname process_responses
#' @export
map_response_options <- function(responses) {
  ensure_columns(responses, "response", stop, "No responses to map. ")

  if (!ensure_columns(
    responses, c("lrn_type", "lrn_question_reference"),
    warning, "Cannot create lookup table. "
  )) {
    return(responses)
  }

  lookup_table <- responses %>%
    dplyr::filter_at("lrn_type", function(x) x == "mcq") %>%
    dplyr::select("lrn_question_reference", dplyr::starts_with("lrn_option_")) %>%
    dplyr::distinct() %>%
    dplyr::mutate_all(as.character)

  responses$response <- as.character(responses$response)

  value_positions <-
    responses$lrn_question_reference %in% lookup_table$lrn_question_reference &
      !is.na(responses$response)

  values <- furrr::future_map2_chr(
    responses$response[value_positions],
    responses$lrn_question_reference[value_positions],
    function(x, y) map_response(x, y, lookup_table),
    .progress = TRUE
  )

  responses$response[value_positions] <- values
  responses %>% structure(option_value_table = lookup_table)
}


# Helper functions --------------------------------------------------------

# map a response array to its values in a lookup table
map_response <- function(response, reference, lookup_table) {
  item_row <- match(reference, lookup_table$lrn_question_reference)

  if (is.na(item_row)) {
    return(response)
  }

  if (response %in% c('[]', '')) {
    return(NA_character_)
  }

  option_numbers <- response %>%
    stringr::str_split(",", simplify = TRUE) %>%
    readr::parse_number()

  lookup_table %>%
    dplyr::select(option_numbers + 2) %>%
    dplyr::slice(item_row) %>%
    unlist() %>%
    paste(collapse = "; ")
}

# add an s to a word depending on condition
pluralize <- function(word, condition) {
  if (condition) sprintf("%ss", word) else word
}

# set attributes to an object in a pipe
set_attributes <- function(object, attr_list) {
  attributes(object) <- attr_list
  object
}
