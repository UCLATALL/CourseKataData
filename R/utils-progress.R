init_progress <- function(name, total) {
  progress::progress_bar$new(
    format = paste(name, "[:bar]", ":percent", sep = "   "),
    total = total,
    show_after = 0
  )
}

pipe_tick <- function(piped, progress_bar) {
  progress_bar$tick()
  piped
}
