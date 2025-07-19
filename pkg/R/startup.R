config <- new.env()

startup <- function(){
  home_dir <- Sys.getenv("HOME")
  jupyter_config <- file.path(home_dir, ".jupyter", "RKernelSession-config.R")
  if (file.exists(jupyter_config)) {
    log_out(sprintf("Loading '%s'",jupyter_config))
    source(jupyter_config)
  }
  parents <- strsplit(getwd(), .Platform$file.sep)[[1]]
  parents <- Reduce(file.path, parents, accumulate = TRUE)
  for (parent in parents) {
    dot_file <- file.path(parent, ".RKernelSession-profile")
    if (file.exists(dot_file)) {
      log_out(sprintf("Loading '%s'",startup_file))
      source(dot_file)
    }
    startup_file <- file.path(parent, "RKernelSession-startup.R")
    if (file.exists(startup_file)) {
      log_out(sprintf("Loading '%s'",startup_file))
      source(startup_file)
    }
  }
}

setup_session <- function(http_port) {
  # log_out("setup_session")
  options(error=NULL)
  inject_send_options()
  suppressWarnings(rm(.pbd_env,envir=.GlobalEnv))
  install_browseURL()
  set_config(use_widgets = FALSE)
  set_http_port(http_port)
  set_help_displayed(TRUE)
  install_output_hooks()
  install_safe_q()
  install_readline()
  install_scan()
  install_menu()
  set_help_displayed()
  install_globalCallingHandlers()
  install_debugging()
  # log_out("setup_session DONE")
}
