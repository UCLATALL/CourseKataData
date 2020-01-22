process_media_views <- function(object, time_zone = "UTC", class_id = "") {
  # by default assume we are getting a data frame,
  # but if we get a string instead...
  if (is.character(object)) {
    object <- load_class_data(object, "media_views[.]csv$", class_id)
  }

  # process the object
  object %>%
    dplyr::as_tibble() %>%
    dplyr::mutate_all(as.character) %>%
    mutate_at_if_exists(
      c("dt_started", "dt_last_event"),
      readr::parse_datetime,
      locale = readr::locale(tz = time_zone)
    ) %>%
    mutate_at_if_exists(
      c("proportion_video", "proportion_time"),
      readr::parse_double
    ) %>%
    mutate_at_if_exists(
      c("log_json"),
      safe_convert_json
    )
}
