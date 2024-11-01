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
  # log_out(n, use.print = TRUE)
  n <- intersect(n, names(.Options))
  # log_out(n, use.print = TRUE)
  opts <- .Options[n]
  # log_out(opts, use.str = TRUE)
  # sent <- ls(sent_opts)
  # new <- setdiff(n, sent)
  # opts <- .Options[new]
  # n <- intersect(n,sent)
  # changed <- rep(FALSE, length(n))
  # names(changed) <- n
  # for(i in n){
  #   changed[i] <- !identical(snc_opts[i], sent_opts[i])
  # }
  # opts <- c(opts,.Options[changed])
  # for(i in names(opts))
  #  sent_opts[i] <- opts[i]
  if(length(opts)){
    msg_send(list(
      type = "options",
      content = opts
    ))
  }
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
