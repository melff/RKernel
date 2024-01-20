#' @importFrom pbdZMQ ZMQ.MC
#' @importFrom pbdZMQ ZMQ.ST
#' @importFrom pbdZMQ zmq.socket zmq.bind zmq.connect zmq.ctx.new zmq.ctx.destroy
#' @importFrom pbdZMQ zmq.recv.multipart zmq.send.multipart
#' 
#' @include json.R

zmq_env <- new.env()

#' @export
zmq_init <- function(){
    zmq_env$context <- zmq.ctx.new()
    zmq_env$sockets <- list()
}

#' @export
zmq_new_responder <- function(port){
    socket <- zmq.socket(zmq_env$context, ZMQ.ST()$REP)
    addr <- sprintf("tcp://*:%d",port)
    index <- as.character(port)
    zmq.bind(socket,addr)
    zmq_env$sockets[[index]] <- socket
}

#' @export
zmq_new_requester <- function(port){
    socket <- zmq.socket(zmq_env$context, ZMQ.ST()$REQ)
    addr <- sprintf("tcp://localhost:%d",port)
    index <- as.character(port)
    zmq.connect(socket,addr)
    zmq_env$sockets[[index]] <- socket
}



#' @export
zmq_receive <- function(port){
    index <- as.character(port)
    socket <- zmq_env$sockets[[index]]
    content <- zmq.recv.multipart(socket,unserialize=FALSE)
    return(content)
}

#' @export
zmq_send <- function(port,content){
    index <- as.character(port)
    socket <- zmq_env$sockets[[index]]
    content <- append(content,list(raw(0)))
    zmq.send.multipart(socket,content,serialize=FALSE)
}

#' @export
zmq_shutdown <- function(){
    for(socket in zmq_env$sockets)
        zmq.close(socket)
    zmq.ctx.destroy(zmq_env$context)
}

zmq_handlers <- list()

#' @export
zmq_reply <- function(port){
    log_out("zmq_reply")
    msg <- zmq_receive(port)
    log_out(msg,use.str=TRUE)
    msg <- fromRawJSON(msg[[1]])
    type <- msg$type
    handler <- zmq_handlers[[type]]
    if(length(handler)){
        response <- handler(msg)
    }
    else {
        response <- zmq_default_handler(msq)
    }
    log_out(response,use.str=TRUE)
    response <- toRawJSON(response)
    zmq_send(port,list(response))
}

zmq_default_handler <- function(msg){
  list(type="unknown_reply",
       content = list(
           status    = "error",
           name      = "UnkownRequest",
           evalue    = msg$type,
           traceback = list()
       ))
}

zmq_handlers$is_complete_request <- function(msg){
    code <- msg$content$code
    status <- code_is_complete(code)
    list(
        type = "is_complete_reply",
        status = status
    )
}

#' @export
zmq_request <- function(port,msg){
    msg <- toRawJSON(msg)
    tryCatch(zmq_send(port,list(msg)),
             error=function(e) log_error(conditionMessage(e)))
    response <- tryCatch(zmq_receive(port),
             error=function(e) log_error(conditionMessage(e)))
    response <- fromRawJSON(response[[1]])
    return(response)
}
