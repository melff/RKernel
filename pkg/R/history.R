history_env <- new.env()

#' @importFrom jsonlite stream_out
history_record <- function(session,number,input) {
  d <- data.frame(session,number,input)
  stream_out(d, history_con(),verbose=FALSE)
  flush(history_con())
}

history_con <- function() {
  if(is.null(history_env$con)) {
    history_env$con <- file(history_fn(), "a")
  }
  history_env$con
}

#' @importFrom rappdirs user_cache_dir
history_fn <- function() {
  history_dir <- user_cache_dir("RKernel")
  if(!dir.exists(history_dir))
    dir.create(history_dir, recursive = TRUE, mode = "700")
  file.path(history_dir, "history")
}

history_close <- function() {
  close(history_env$con)
}

#' @importFrom jsonlite stream_in
history_load <- function() {
  con <- file(history_fn(), "r")
  on.exit(close(con))
  stream_in(con, verbose = FALSE)
}