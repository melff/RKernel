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
    zmq_env$ports <- integer()
}

#' @export
zmq_new_receiver <- function(port){
    socket <- zmq.socket(zmq_env$context, ZMQ.ST()$PULL)
    addr <- sprintf("tcp://localhost:%d",port)
    index <- as.character(port)
    zmq.connect(socket,addr)
    zmq_env$sockets[[index]] <- socket
    zmq_env$ports["receiver"] <- as.integer(port)
}

#' @export
zmq_new_sender <- function(port){
    socket <- zmq.socket(zmq_env$context, ZMQ.ST()$PUSH)
    addr <- sprintf("tcp://localhost:%d",port)
    index <- as.character(port)
    zmq.connect(socket,addr)
    zmq_env$sockets[[index]] <- socket
    zmq_env$ports["sender"] <- as.integer(port)
}



#' @export
zmq_receive <- function(){
    log_out("zmq_receive")
    port <- zmq_env$ports["receiver"]
    index <- as.character(port)
    socket <- zmq_env$sockets[[index]]
    # log_out(socket,use.str=TRUE)
    content <- zmq.recv.multipart(socket,unserialize=FALSE)
    # log_out(content,use.str=TRUE)
    content <- fromRawJSON(content[[1]])
    return(content)
}

#' @export
zmq_send <- function(content){
    log_out("zmq_send")
    log_out(content,use.str=TRUE)
    port <- zmq_env$ports["sender"]
    index <- as.character(port)
    socket <- zmq_env$sockets[[index]]
    content <- toRawJSON(content)
    content <- append(list(content),list(raw(0)))
    zmq.send.multipart(socket,content,serialize=FALSE)
    log_out("message sent")
}

#' @export
zmq_shutdown <- function(){
    for(socket in zmq_env$sockets)
        zmq.close(socket)
    zmq.ctx.destroy(zmq_env$context)
}

zmq_handlers <- list()

#' @export
zmq_reply <- function(){
    # log_out("zmq_reply")
    msg <- zmq_receive()
    # log_out(msg,use.str=TRUE)
    type <- msg$type
    handler <- zmq_handlers[[type]]
    if(length(handler)){
        response <- handler(msg)
    }
    else {
        response <- zmq_default_handler(msq)
    }
    zmq_send(response)
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

