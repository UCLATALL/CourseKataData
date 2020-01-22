process_page_views <- function(object, time_zone = "UTC", class_id = "") {
  # by default assume we are getting a data frame,
  # but if we get a string instead...
  if (is.character(object)) {
    object <- load_class_data(object, "page_views[.]csv$", class_id)
  }

  # process the object
  object %>%
    dplyr::as_tibble() %>%
    dplyr::mutate_all(as.character) %>%
    mutate_at_if_exists(
      c("dt_accessed"),
      readr::parse_datetime,
      locale = readr::locale(tz = time_zone)
    )
}
