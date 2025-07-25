snc_opts <- new.env()
# sent_opts <- new.env()

add_sync_options <- function(n) {
  for(n_ in n)
    assign(n_,TRUE, envir = snc_opts)
}

send_options <- function(nms){
  n <- ls(snc_opts)
  n <- intersect(nms,n)
  n <- intersect(n, names(.Options))
  if(length(n)) {
    opts <- .Options[n]
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


options_with_send <- function(...) {
  args <- list(...)
  res <- orig_func$options(...)
  if(length(n <- names(args))) {
    send_options(n)
  }
  invisible(res)
}

#' @importFrom utils getFromNamespace
inject_send_options <- function() {
  orig_func$options <- getFromNamespace("options", "base")
  replace_in_package("base","options", options_with_send)
}
