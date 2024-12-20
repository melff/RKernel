EOT <- "\x04"
DLE <- "\x10"
ETB <- "\x17"
STX <- "\x02"
ETX <- "\x03"

request_handlers <- new.env()

#'@export 
handle_request <- function(msg){
  # log_out("handle_request")
  msg_type <- msg$type
  # log_out("msg_type")
  envir <- parent.frame()
  handler <- request_handlers[[msg_type]]
  if (is.function(handler)) {
    response <- tryCatch(handler(msg, envir),
      error = function(e) {
        log_error(conditionMessage(e))
        # log_error(msg,use.str=TRUE)
        return(NULL)
      }
    )
  } else {
    response <- request_default_handler(msq,env)
  }
  if (length(response)) {
    tryCatch(
      msg_send(response),
      error = function(e) {
        log_error(conditionMessage(e))
        return(NULL)
      }
    )
  }
}

#' @export
make_dput_request <- function(msg) {
  msg_dput <- wrap_dput(msg)
  paste0("RKernel::handle_request(", msg_dput, ")")
}

wrap_dput <- function(msg) {
  con <- textConnection(NULL, open = "w")
  dput(msg, file = con)
  paste(textConnectionValue(con), collapse = "")
}

#' @export
send_dput_msg <- function(msg) {
  msg_dput <- wrap_dput(msg)
  cat_(DLE, STX, msg_dput, ETX, DLE, sep = "")
}

unwrap_dput_msg <- function(msg){
  msg <- remove_prefix(msg, STX) |> remove_suffix(ETX)
  # log_out(msg)
  eval(str2lang(msg))
}

request_default_handler <- function(msg, ...) {
  list(
    type = "unknown_reply",
    content = list(
      status    = "error",
      name      = "UnkownRequest",
      evalue    = msg$type,
      traceback = list()
    )
  )
}

request_handlers$echo <- function(msg) {
  msg_send(msg)
}

request_handlers$is_complete_request <- function(msg, ...) {
  code <- msg$content$code
  status <- code_status(code)
  list(
    type = "is_complete_reply",
    content = list(
      status = status,
      indent = ""
    )
  )
}

request_handlers$complete_request <- function(msg, ...) {
  code <- msg$content$code
  cursor_pos <- msg$content$cursor_pos
  response <- get_completions(code, cursor_pos)
  list(
    type = "complete_reply",
    content = list(
      status = "ok",
      matches = response$matches,
      cursor_start = response$start,
      cursor_end = response$end,
      metadata = emptyNamedList
    )
  )
}

request_handlers$inspect_request <- function(msg, ...) {
  code <- msg$content$code
  cursor_pos <- msg$content$cursor_pos
  detail_level <- msg$content$detail_level
  response <- inspect_reply(code, cursor_pos, detail_level)
  list(
    type = "inspect_reply",
    content = list(
      found = response$found,
      status = "ok",
      data = response$data,
      metadata = emptyNamedList
    )
  )
}

request_handlers$comm_info_request <- function(msg, ...) {
  target_name <- NULL
  if ("target_name" %in% names(msg$content)) {
    target_name <- msg$content$target_name
  }
  cm <- get_comm_manager()
  comms <- cm$get_comms(target_name)
  list(
    type = "comm_info_reply",
    content = list(
      status = "ok",
      comms = comms
    )
  )
}

request_handlers$comm_open <- function(msg, ...) {
  # log_out("request_handler - comm open")
  cm <- get_comm_manager()
  target_name <- msg$content$target_name
  id <- msg$content$comm_id
  data <- msg$content$data
  cm$handle_open(target_name, id, data)
  # log_out("done")
}

request_handlers$comm_msg <- function(msg, ...) {
  # log_out("request_handler - comm msg")
  # log_out(msg,use.str=TRUE)
  cm <- get_comm_manager()
  id <- msg$content$comm_id
  data <- msg$content$data
  data$buffers <- msg$buffers
  cm <- get_comm_manager()
  cm$handle_msg(id, data)
  # log_out("done")
}

request_handlers$comm_close <- function(msg, ...) {
  # log_out("request_handler - comm close")
  cm <- get_comm_manager()
  id <- msg$content$comm_id
  data <- msg$content$data
  cm <- get_comm_manager()
  cm$handle_close(id, data)
  # log_out("done")
}


request_handlers$debug_request <- function(msg, envir, ...) {
  ds <- get_dap_server()
  request <- msg$content
  response <- ds$handle(request, envir)
  list(
    type = "debug_reply",
    content = response
  )
}

request_handlers$cell_magic <- function(msg, ...) {
  req <- msg$content
  dispatch_magic_handler(
    req$command,
    req$code,
    req$argsq
  )
}

msg_env <- new.env()
msg_env$send <- json_send
msg_send <- function(msg) msg_env$send(msg)