config <- new.env()

#' @export
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