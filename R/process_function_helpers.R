
# Capture and discard errors when converting JSON to list
safe_convert_json <- function(json_col) {
  possibly_parse <- purrr::possibly(jsonlite::parse_json, otherwise = list())
  purrr::map(json_col, possibly_parse)
}

# Ensure that a data frame has the required columns or signal with a message
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
