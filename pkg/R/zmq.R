#' @importFrom pbdZMQ ZMQ.MC
#' @importFrom pbdZMQ ZMQ.ST
#' @importFrom pbdZMQ zmq.socket zmq.bind zmq.connect zmq.ctx.new zmq.ctx.destroy
#' @importFrom pbdZMQ zmq.recv.multipart zmq.send.multipart
#' 
#' @include json.R

zmq_env <- new.env()

#' @export
zmq_init <- function(){
     # log_out("zmq_init")
    zmq_env$context <- zmq.ctx.new()
    zmq_env$sockets <- list()
    zmq_env$ports <- integer()
}

#' @export
zmq_new_receiver <- function(port, bind = FALSE){
    # log_out("zmq_new_receiver")
    socket <- zmq.socket(zmq_env$context, ZMQ.ST()$PULL)
    port <- as.integer(port)
    addr <- if (bind) sprintf("tcp://*:%d", port) 
            else sprintf("tcp://localhost:%d", port)
    index <- as.character(port)
    if (bind) zmq.bind(socket, addr)
    else zmq.connect(socket,addr)
    zmq_env$sockets[["receiver"]] <- socket
    zmq_env$ports["receiver"] <- port
}

#' @export
zmq_new_sender <- function(port, bind = FALSE){
    # log_out("zmq_new_sender")
    socket <- zmq.socket(zmq_env$context, ZMQ.ST()$PUSH)
    port <- as.integer(port)
    addr <- if (bind) sprintf("tcp://*:%d", port) 
            else sprintf("tcp://localhost:%d", port)
    index <- as.character(port)
    if (bind) zmq.bind(socket, addr)
    else zmq.connect(socket,addr)
    zmq_env$sockets[["sender"]] <- socket
    zmq_env$ports["sender"] <- port
}



#' @export
zmq_receive <- function(){
    log_out("zmq_receive")
    socket <- zmq_env$sockets[["receiver"]]
    # log_out(socket,use.str=TRUE)
    msg <- zmq.recv.multipart(socket,unserialize=FALSE)
    msg <- fromRawJSON(msg[[1]])
    # log_out("message received")
    # log_out(msg,use.str=TRUE)
    log_out(msg$type)
    return(msg)
}

#' @export
zmq_send <- function(msg){
    log_out("zmq_send")
    log_out(msg$type)
    # log_out(msg,use.str=TRUE)
    socket <- zmq_env$sockets[["sender"]]
    msg <- toRawJSON(msg)
    msg <- append(list(msg),list(raw(0)))
    zmq.send.multipart(socket,msg,serialize=FALSE)
    # log_out("message sent")
    # log_out(msg,use.str=TRUE)
}

#' @export
zmq_shutdown <- function(){
    # log_out("zmq_shutdown")
    for(socket in zmq_env$sockets)
        zmq.close(socket)
    zmq.ctx.destroy(zmq_env$context)
}

zmq_handlers <- list()

#' @export
zmq_request <- function(){
    log_out("zmq_request")
    msg <- zmq_receive()
    # log_out("message recieved")
    # log_out(msg,use.str=TRUE)
    envir <- parent.frame()
    type <- msg$type
    log_out(paste("Type:",type))
    handler <- zmq_handlers[[type]]
    if(length(handler)){
        # log_out(sprintf("Found handler for '%s'", type))
        response <- tryCatch(handler(msg, envir),
            error = function(e) {
                log_error(conditionMessage(e))
                # log_error(msg,use.str=TRUE)
                return(NULL)
            }
        )
    }
    else {
        response <- zmq_default_handler(msq,env)
    }
    # log_out("handler success")
    if (is.null(response)) {
        log_out("Empty response ...")
    }
    else {
        log_out(paste("Response:",response$type))
        zmq_send(response)
    }
    # log_out("done")
}

#' @export
zmq_request_noreply <- function(){
    log_out("zmq_request_noreply")
    msg <- zmq_receive()
    # log_out(msg,use.str=TRUE)
    envir <- parent.frame()
    type <- msg$type
    log_out(paste("Type:", type))
    handler <- zmq_handlers[[type]]
    if(length(handler)){
        tryCatch(handler(msg,envir),
                 error=function(e){
                     log_error(conditionMessage(e))
                     #log_error(msg,use.str=TRUE)
                 })
    }
    log_out("done")
}

DLE <- '\x10'
ZMQ_PUSH <- '[!ZMQ_PUSH]'

zmq_push <- function(msg){
    # log_out("zmq_push")
    # log_out(msg,use.str=TRUE)
    cat_(DLE)
    cat_(ZMQ_PUSH)
    zmq_send(msg)
    cat_(DLE)
    cat_("")
    # log_out("zmq_push done")
}


zmq_default_handler <- function(msg,...){
    # log_out("zmq-default-handler")
    list(type="unknown_reply",
         content = list(
             status    = "error",
             name      = "UnkownRequest",
             evalue    = msg$type,
             traceback = list()
         ))
}

zmq_handlers$is_complete_request <- function(msg,...){
    code <- msg$content$code
    status <- code_is_complete(code)
    list(
        type = "is_complete_reply",
        content = list(
            status = status,
            indent = ""
        )
    )
}

zmq_handlers$complete_request <- function(msg,...){
    code <- msg$content$code
    cursor_pos <- msg$content$cursor_pos
    response <- get_completions(code,cursor_pos)
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

zmq_handlers$inspect_request <- function(msg,...){
    code <- msg$content$code
    cursor_pos <- msg$content$cursor_pos
    detail_level <- msg$content$detail_level
    response <- inspect_reply(code,cursor_pos,detail_level)
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

zmq_handlers$comm_info_request <- function(msg,...){
      target_name <- NULL
      if("target_name" %in% names(msg$content))
          target_name <- msg$content$target_name
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

zmq_handlers$comm_open <- function(msg,...){
    # log_out("zmq_handler - comm open")
    cm <- get_comm_manager()
    target_name <- msg$content$target_name
    id <- msg$content$comm_id
    data <- msg$content$data
    cm$handle_open(target_name,id,data)
    # log_out("done")
}

zmq_handlers$comm_msg <- function(msg,...){
    # log_out("zmq_handler - comm msg")
    # log_out(msg,use.str=TRUE)
    cm <- get_comm_manager()
    id <- msg$content$comm_id
    data <- msg$content$data
    data$buffers <- msg$buffers
    cm <- get_comm_manager()
    cm$handle_msg(id,data)
    # log_out("done")
}

zmq_handlers$comm_close <- function(msg,...){
    # log_out("zmq_handler - comm close")
    cm <- get_comm_manager()
    id <- msg$content$comm_id
    data <- msg$content$data
    cm <- get_comm_manager()
    cm$handle_close(id,data)
    # log_out("done")
}


zmq_handlers$debug_request <- function(msg,envir,...){
    ds <- get_dap_server()
    request <- msg$content
    response <- ds$handle(request,envir)
    list(
        type = "debug_reply",
        content = response
    )
}

zmq_handlers$cell_magic <- function(msg,...){
    req <- msg$content
    dispatch_magic_handler(req$command,
                           req$code,
                           req$argsq)
}

#' @export
zmq_input_request <- function(prompt='',password = FALSE){
    msg <- list(
        type = 'input_request',
        content = list(
            prompt = prompt,
            password = password
        )
    )
    zmq_send(msg)
    resp <- zmq_receive()
    return(resp)
}

