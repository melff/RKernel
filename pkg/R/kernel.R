#' @import R6 
#' @importFrom pbdZMQ zmq.socket zmq.bind zmq.ctx.new zmq.getsockopt zmq.setsockopt
#' @importFrom pbdZMQ zmq.poll zmq.poll.get.revents zmq.msg.recv zmq.msg.send
#' @importFrom pbdZMQ zmq.recv.multipart zmq.send.multipart .zmqopt_init
#' @importFrom digest hmac
#' @importFrom uuid UUIDgenerate

PROTOCOL_VERSION <- '5.3'
WIRE_DELIM <- charToRaw("<IDS|MSG>")

#' @export
Kernel <- R6Class("Kernel",

  public = list(

    initialize = function(conn_info,evaluator=NULL){
      .zmqopt_init(envir = private)
      private$zmqctx <- zmq.ctx.new()
      private$sockets$hb      <-  zmq.socket(private$zmqctx, private$.pbd_env$ZMQ.ST$REP)
      private$sockets$iopub   <-  zmq.socket(private$zmqctx, private$.pbd_env$ZMQ.ST$PUB)
      private$sockets$control <-  zmq.socket(private$zmqctx, private$.pbd_env$ZMQ.ST$ROUTER)
      private$sockets$stdin   <-  zmq.socket(private$zmqctx, private$.pbd_env$ZMQ.ST$ROUTER)
      private$sockets$shell   <-  zmq.socket(private$zmqctx, private$.pbd_env$ZMQ.ST$ROUTER)
      for(s in private$sockets[c("control","stdin","shell")])
        zmq.setsockopt(s, private$.pbd_env$ZMQ.SO$ROUTER_HANDOVER, 1L)
      url <- paste0(conn_info$transport, '://', conn_info$ip)
      url_sep <- switch(conn_info$transport, tcp=":", ipc="-")
      for(s in c("hb","iopub","control","stdin","shell")){
        port_name <- paste0(s,"_port")
        url_with_port <- paste0(url,url_sep,conn_info[[port_name]])
        zmq.bind(private$sockets[[s]],url_with_port)
      }
      private$conn_info <- conn_info
      private$evaluator <- evaluator
    },

    run = function(){
      private$evaluator$startup()
      continue <- TRUE
      while(continue) {
        req <- private$poll_request(c("hb","control","shell"))
        #Sys.sleep(1)
        if(req$abort) break
        continue <- switch(req$socket_name,
               hb=private$respond_hb(req),
               control=private$respond_control(req),
               shell=private$respond_shell(req))
      }
    },

    execution_count = 1,

    execute_reply = function(msg){
      private$send_message(type="execute_input",
                           parent=msg,
                           socket_name="iopub",
                           code=msg$content$code,
                           execution_count=self$execution_count)
      results <- private$evaluator$eval(msg$content$code)
      if(length(results)){
        for(result in results){
          if("clear_output" %in% names(result)){
            private$send_message(type="clear_output",
                                 parent=msg,
                                 socket_name="iopub",
                                 wait=results$clear_output
                                 )
          }
          if("stream" %in% names(result)){
            if(nzchar(result$text)){
              private$send_message(type="stream",
                                   parent=msg,
                                   socket_name="iopub",
                                   name=result$stream,
                                   text=result$text)
            }
          }
          if("execute_result" %in% names(result)){
            private$send_message(type="execute_result",
                                 parent=msg,
                                 socket_name="iopub",
                                 data=result$execute_result$data,
                                 metadata=result$execute_result$metadata,
                                 transient=result$execute_result$transient,
                                 execution_count=self$execution_count
                                 )
          }
          if("display_data" %in% names(result)){
            private$send_message(type="display_data",
                                 parent=msg,
                                 socket_name="iopub",
                                 data=result$display_data$data,
                                 metadata=result$display_data$metadata,
                                 transient=result$display_data$transient
                                 )
          }
          if("update_display_data" %in% names(result)){
            private$send_message(type="update_display_data",
                                 parent=msg,
                                 socket_name="iopub",
                                 data=result$update_display_data$data,
                                 metadata=result$update_display_data$metadata,
                                 transient=result$update_display_data$transient
                                 )
          }
          if("error" %in% names(result)){
            private$send_message(type="error",
                                 parent=msg,
                                 socket="iopub",
                                 ename = result$error$name,
                                 evalue = result$error$value,
                                 traceback = result$error$traceback
                                 )
          }
        }
      }
      payload <- private$evaluator$get_payload(clear=TRUE)
      payload <- check_page_payload(payload)
      status <- private$evaluator$get_status(reset=TRUE)
      aborted <- private$evaluator$is_aborted(reset=TRUE)
      private$send_message(type="execute_reply",
                           parent=msg,
                           socket="shell",
                           payload=payload,
                           status=status,
                           execution_count=self$execution_count)
      self$execution_count <- self$execution_count + 1
      #cat("Sent a execute_reply ...\n")
      if(aborted) private$clear_shell_queue()
    },

    kernel_info_reply = function(msg){
      rversion <- paste0(version$major,".",version$minor)
      response <- list(protocol_version= PROTOCOL_VERSION,
                       implementation="RKernel",
                       implementation_version = "0.1",
                       language_info = list(
                         name = "R",
                         codemirror_mode = "R",
                         pygments_lexer = "r",
                         mimetype = "text/x-r-source",
                         file_extension = ".R",
                         version = rversion),
                       banner = version$version.string)
      private$send_message(type="kernel_info_reply",
                           parent=msg,
                           socket_name="shell",
                           content=response)
      #cat("Sent a kernel_info_reply ...\n")
    },

    is_complete_reply = function(msg){
      #cat("is_complete_reply\n")
      #str(msg)
      code <- msg$content$code
      status <- code_is_complete(code)
      private$send_message(type="is_complete_reply",
                           parent=msg,
                           socket_name="shell",
                           status=status,
                           #debug=TRUE,
                           indent="")
      #cat("Sent an is_complete_reply ...\n")
    },

    complete_reply = function(msg){
      code <- msg$content$code
      cursor_pos <- msg$content$cursor_pos
      private$send_message(type="complete_reply",
                           parent=msg,
                           socket_name="shell",
                           status="ok",
                           matches=list(),
                           cursor_start=cursor_pos,
                           cursor_end=cursor_pos,
                           metadata=namedList())
    }
  ),

  private = list(

    .pbd_env = new.env(),
    sockets = list(),
    zmqctx = list(),
    conn_info = list(),
    evaluator = list(),

    poll_request = function(sock_names) {
      POLLIN <- private$.pbd_env$ZMQ.PO$POLLIN
      req <- list()
      r <- tryCatch(
        zmq.poll(private$sockets[sock_names],
                 rep(POLLIN,length(sock_names)),
                 MC=private$.pbd_env$ZMQ.MC),
        interrupt = function(e) "SIGINT"
      )
      req$abort <- identical(r[1],"SIGINT") 
      for(i in seq_along(sock_names)){
        if(bitwAnd(zmq.poll.get.revents(i),POLLIN)){
          req$socket_name <- sock_names[i]
          break
        }
      }
      return(req)
    },

    respond_hb = function(req){
      data <- zmq.msg.recv(private$sockets$hb,
                           flags=private$.pbd_env$ZMQ.SR$BLOCK,
                           unserialize=FALSE)
      zmq.msg.send(data,private$sockets$hb,
                   flags=private$.pbd_env$ZMQ.SR$BLOCK,
                   serialize=FALSE)
      return(TRUE)
    },

    respond_control = function(req){
      msg <- private$get_message("control")
      if(!length(msg)) return(TRUE)
      if(msg$header$msg_type=="shutdown_request")
        return(FALSE)
      else return(TRUE)
    },

    respond_shell = function(req){
      msg <- private$get_message("shell")
      if(!length(msg)) return(TRUE)
      private$send_message(type="status",parent=msg,
                           socket_name="iopub",execution_state="busy")
      #cat("Got a", msg$header$msg_type, "request ...\n")
      # do_stuff ...
      switch(msg$header$msg_type,
             execute_request = self$execute_reply(msg),
             is_complete_request = self$is_complete_reply(msg),
             kernel_info_request = self$kernel_info_reply(msg),
             complete_request = self$complete_reply(msg))

      private$send_message(type="status",parent=msg,
                           socket_name="iopub",execution_state="idle")
      return(TRUE)
    },

    get_message = function(socket_name){
      socket <- private$sockets[[socket_name]]
      #wire_in <- zmq.recv.multipart(socket,
      #                              unserialize=FALSE)
      wire_in <- list()
      i <- 1
      repeat {
        wire_in[[i]] <- zmq.msg.recv(socket,
                                     flags=private$.pbd_env$ZMQ.SR$BLOCK, 
                                     unserialize=FALSE)
        get_more <- zmq.getsockopt(socket,private$.pbd_env$ZMQ.SO$RCVMORE,0L)
        if(get_more != 1) break
        i <- i + 1
      }
      msg <- private$wire_unpack(wire_in)
      #cat("Got message from socket", socket_name)
      return(msg)
    },

    send_message = function(type, parent, socket_name, debug=FALSE, content=list(...), ...){
      msg <- private$msg_new(type,parent,content)
      if(debug) str(msg)
      socket <- private$sockets[[socket_name]]
      wire_out <- private$wire_pack(msg)
      #zmq.send.multipart(socket,wire_out,serialize=FALSE)
      l <- length(wire_out)
      for(i in 1:l){
        flag <- if(i < l) private$.pbd_env$ZMQ.SR$SNDMORE 
                else private$.pbd_env$ZMQ.SR$BLOCK
        zmq.msg.send(wire_out[[i]],socket,flag=flag,serialize=FALSE)
      }
      #cat("Sent message to socket", socket_name)
    },

    wire_unpack = function(wire_in){
      l <- length(wire_in)
      found <- FALSE
      for(i in 1:l)
        if(identical(wire_in[[i]],WIRE_DELIM)) {
          found <- TRUE
          break
        }
      if(!found) return(NULL)
      if(i + 5 < l) return(NULL)
      signature <- rawToChar(wire_in[[i+1]])
      msg <- wire_in[i + 2:5]
      if(signature != private$get_signature(msg)) return(NULL)
      msg <- lapply(msg,fromRawJSON)
      names(msg) <- c("header","parent_header","metadata","content")
      if(i > 1)
        msg$identities <- wire_in[1:(i-1)]
      if(l > i + 5)
        msg$extra <- wire_in[(i+5):l]
      return(msg)
    },

    wire_pack = function(msg){
      msg_body <- lapply(msg[c("header","parent_header","metadata","content")],
                         toRawJSON,auto_unbox=TRUE)
      signature <- private$get_signature(msg_body)
      c(msg$identities,
        list(WIRE_DELIM),
        list(charToRaw(signature)),
        msg_body)
    },

    get_signature = function(msg){
      msg <- unlist(msg)
      hmac(private$conn_info$key,msg,"sha256")
    },

    msg_new = function(type,parent,content){
      parent_header <- parent$header
      header <- list(
        msg_id = UUIDgenerate(),
        session = parent_header$session,
        username = parent_header$username,
        date = strftime(as.POSIXlt(Sys.time(),"UTC"),"%Y-%m-%dT%H:%M:%OS6Z"),
        msg_type = type,
        version = PROTOCOL_VERSION
      )
      list(header = header,
           parent_header = parent_header,
           content = content,
           identities = parent$identities,
           metadata = namedList())
    },

    clear_shell_queue = function(...){
      # Empty message queue from shell
      POLLIN <- private$.pbd_env$ZMQ.PO$POLLIN
      repeat {
        r <- zmq.poll(c(private$sockets$shell),POLLIN,0L)
        if(bitwAnd(zmq.poll.get.revents(1),POLLIN)){
          request <- private$get_message("shell")
          request_type <- request$header$msg_type
          reply_type <- sub("_request","_reply",request_type,fixed=TRUE)
          private$send_message(type=reply_type,
                           parent=request,
                           socket_name="shell",
                           status="aborted")
        }
        else break
      }
    }
  )
)

fromRawJSON <- function(raw_json) {
    json <- rawToChar(raw_json)
    Encoding(json) <- "UTF-8"
    fromJSON(json)
}

toRawJSON <- function(x,...){
  json <- toJSON(x,...)
  charToRaw(json)
}

namedList <- function() structure(list(),names=character(0))

check_page_payload <- function(payload){
  for(i in seq_along(payload)){
    payload_item <- payload[[i]]
    if(payload_item$source=="page"){
      data <- payload_item$data
      if(!("text/plain" %in% names(data))){
        payload_item$data[["text/plain"]] <- "[No plain text data for paging]"
        payload[[i]] <- payload_item
      }
    }      
  }
  payload
}



# Local Variables:
# ess-indent-offset: 2
# End:
