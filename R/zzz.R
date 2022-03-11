.onLoad <- function(lib, pkg) {
  rlang::run_on_load()
}

rlang::on_load(rlang::local_use_cli())
