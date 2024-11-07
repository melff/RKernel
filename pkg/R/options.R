snc_opts <- new.env()
# sent_opts <- new.env()

add_sync_options <- function(n) {
  for(n_ in n)
    assign(n_,TRUE, envir = snc_opts)
}

#' @export
send_options <- function(){
  # Do not use 'log_out' here as it may lead to a recursion
  # log_out("send_options")
  n <- ls(snc_opts)
  n <- intersect(n, names(.Options))
  opts <- .Options[n]
  if(length(opts)){
    msg_send(list(
      type = "options",
      content = opts
    ))
  }
}

import_options <- function(opts){
    # log_out("import_opts")
    n <- intersect(ls(snc_opts),names(opts))
    do.call("options",opts[n])
}

options_orig <- getFromNamespace("options", "base")

options_with_send <- function(...) {
  # Do not use 'log_out' here as it may lead to a recursion
  args <- list(...)
  res <- options_orig(...)
  if(length(names(args))) send_options()
  invisible(res)
}

#' @export
inject_send_options <- function() {
  replace_in_package("base","options", options_with_send)
}
