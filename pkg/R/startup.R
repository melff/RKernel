config <- new.env()

startup <- function(){
  home_dir <- Sys.getenv("HOME")
  jupyter_config <- file.path(home_dir, ".jupyter", "RKernel-config.R")
  if (file.exists(jupyter_config)) {
    source(jupyter_config)
  }
  parents <- strsplit(getwd(), .Platform$file.sep)[[1]]
  parents <- Reduce(file.path, parents, accumulate = TRUE)
  for (parent in parents) {
    dot_file <- file.path(parent, ".RKernel-profile")
    if (file.exists(dot_file)) {
      source(dot_file)
    }
    startup_file <- file.path(parent, "RKernel-startup.R")
    if (file.exists(startup_file)) {
      source(startup_file)
    }
  }
}

setup_session <- function(help_port) {
  options(error=NULL)
  inject_send_options()
  suppressWarnings(rm(.pbd_env,envir=.GlobalEnv))
  install_browseURL()
  set_config(use_widgets = FALSE)
  set_help_port(help_port)
  set_help_displayed(TRUE)
  install_output_hooks()
  install_safe_q()
  install_readline()
  install_scan()
  install_menu()
  set_help_displayed()
  install_globalCallingHandlers()
  install_debugging()
}
