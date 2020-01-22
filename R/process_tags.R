process_tags <- function(object, class_id = "") {
  # by default assume we are getting a data frame,
  # but if we get a string instead...
  if (is.character(object)) {
    object <- load_class_data(object, "tags[.]csv$", class_id)
  }

  # process the object
  object %>%
    dplyr::as_tibble() %>%
    dplyr::mutate_all(as.character)
}
