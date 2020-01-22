process_items <- function(object, class_id = "") {
  # by default assume we are getting a data frame,
  # but if we get a string instead...
  if (is.character(object)) {
    object <- load_class_data(object, "items[.]csv$", class_id)
  }

  # process the object
  object %>%
    dplyr::as_tibble() %>%
    dplyr::mutate_all(as.character) %>%
    mutate_at_if_exists("lrn_question_position", readr::parse_integer) %>%
    mutate_at_if_exists("lrn_question_data", safe_convert_json)
}
