mutate_at_if_exists <- function(.tbl, vars_chr_vctr, .funs, ...) {
  pattern <- paste0("^", vars_chr_vctr, "$", collapse = "|")
  dplyr::mutate_at(.tbl, stringr::str_which(names(.tbl), pattern), .funs, ...)
}

safe_convert_json <- function(json_col) {
  possibly_parse <- purrr::possibly(jsonlite::parse_json, otherwise = list())
  furrr::future_map(json_col, possibly_parse, .progress = TRUE)
}

ensure_columns <- function(data, required_columns, action = warning, message_prefix = "") {
  missing_required_cols <- !required_columns %in% names(data)
  if (any(missing_required_cols)) {
    action(sprintf(
      "%sResponse table missing required %s: %s",
      message_prefix,
      pluralize("column", sum(missing_required_cols) > 1),
      paste0(required_columns[missing_required_cols], collapse = ", ")
    ))
  }

  !any(missing_required_cols)
}
