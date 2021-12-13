#' @import R6 
#' @importFrom pbdZMQ zmq.socket zmq.bind zmq.ctx.new zmq.getsockopt zmq.setsockopt
#' @importFrom pbdZMQ zmq.poll zmq.poll.get.revents zmq.msg.recv zmq.msg.send
#' @importFrom pbdZMQ zmq.recv.multipart zmq.send.multipart .zmqopt_init
#' @importFrom digest hmac
#' @importFrom uuid UUIDgenerate

PROTOCOL_VERSION <- '5.3'
WIRE_DELIM <- charToRaw("<IDS|MSG>")

kernel <- new.env()

#' @export
Kernel <- R6Class("Kernel",

  public = list(

    initialize = function(conn_info){
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
      evaluator <- Evaluator$new(self)
      comm_manager <- CommManager(self,evaluator)
      self$comm_manager <- comm_manager
      self$evaluator <- evaluator
      self$evaluator$comm_manager <- comm_manager
      kernel$current <- self
    },

    evaluator = list(),
    comm_manager = list(),

    run = function(){
      self$evaluator$startup()
      continue <- TRUE
      while(continue) {
        req <- private$poll_request(c("hb","control","shell"))
        #Sys.sleep(1)
        if(req$abort) break
        # print(req$socket_name)
        continue <- switch(req$socket_name,
               hb=private$respond_hb(req),
               control=private$respond_control(req),
               shell=private$respond_shell(req))
      }
      self$evaluator$shutdown()
    },

    execution_count = 1,

    execute_reply = function(msg){
      self$execute_request <- msg
      private$send_message(type="execute_input",
                           parent=msg,
                           socket_name="iopub",
                           content=list(
                             code=msg$content$code,
                             execution_count=self$execution_count))
      self$evaluator$eval(msg$content$code)
      payload <- self$evaluator$get_payload(clear=TRUE)
      payload <- check_page_payload(payload)
      status <- self$evaluator$get_status(reset=TRUE)
      aborted <- self$evaluator$is_aborted(reset=TRUE)
      content <- list(status = status,
                      execution_count = self$execution_count)
      if(length(payload))
        content$payload <- payload

      private$send_message(type="execute_reply",
                           parent=msg,
                           socket="shell",
                           content=content)
      #cat("Sent a execute_reply ...\n")
      # message("Code:", msg$content$code)
      #message("Store history:", msg$content$store_history)
      #message("Execution count:", self$execution_count)
      if(msg$content$store_history)
        self$execution_count <- self$execution_count + 1
      if(aborted) private$clear_shell_queue()
    },

    execute_request = character(0),

    clear_output = function(wait){
      private$send_message(type="clear_output",
                                 parent=self$execute_request,
                                 socket_name="iopub",
                                 content=list(wait=wait))
    },

    stream = function(text,stream){
      private$send_message(type="stream",
                           parent=self$execute_request,
                           socket_name="iopub",
                           content=list(
                             name=stream,
                             text=text))
    },

    execute_result = function(data,metadata=NULL){
      content <- list(data=data,
                      execution_count=self$execution_count)
      if(length(metadata))
        content$metadata <- metadata
      else
        content$metadata <- namedList()
      private$send_message(type="execute_result",
                           parent=self$execute_request,
                           socket_name="iopub",
                           content=content)
    },

    display_send = function(d){
      if(class(d)%in%c("display_data","update_display_data"))
        msg_type <- class(d)
      else stop("'display_data' or 'update_display_data' object required")

      # log_out("kernel$display_send")
      # log_out(sprintf("msg_type = %s",msg_type))
      
      private$send_message(type=msg_type,
                           parent=self$execute_request,
                           socket_name="iopub",
                           content=list(
                             data=d$data,
                             metadata=d$metadata,
                             transient=d$transient))
      self$display_id <- d$transient$display_id
    },
    display_id = character(0),
    last_display = function() self$display_id,
    
    display_data = function(data,metadata=NULL,transient=NULL){
      content <- list(data=data,transient=transient)
      if(length(metadata))
        content$metadata <- metadata
      else
        content$metadata <- namedList()
      private$send_message(type="display_data",
                           parent=self$execute_request,
                           socket_name="iopub",
                           content=list(
                             data=data,
                             metadata=metadata,
                             transient=transient
                           ))
    },

    update_display_data = function(data,metadata=NULL,transient){
      private$send_message(type="update_display_data",
                           parent=self$execute_request,
                           socket_name="iopub",
                           content=list(
                             data=data,
                             metadata=metadata,
                             transient=transient
                           ))
    },

    send_error = function(name,value,traceback){
            private$send_message(type="error",
                           parent=self$execute_request,
                           socket="iopub",
                           content=list(
                             ename = name,
                             evalue = value,
                             traceback = traceback
                           ))
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
      status <- self$evaluator$code_is_complete(code)
      private$send_message(type="is_complete_reply",
                           parent=msg,
                           socket_name="shell",
                           content=list(
                             status=status,
                             indent=""))
      #cat("Sent an is_complete_reply ...\n")
    },

    complete_reply = function(msg){
      code <- msg$content$code
      cursor_pos <- msg$content$cursor_pos
      result <- self$evaluator$get_completions(code,cursor_pos)
      private$send_message(type="complete_reply",
                           parent=msg,
                           socket_name="shell",
                           content=list(
                             status="ok",
                             matches=result$matches,
                             cursor_start=result$start,
                             cursor_end=result$end,
                             metadata=namedList()))
    },

    comm_info_reply = function(msg){
      target_name <- NULL
      if("target_name" %in% names(msg$content))
        target_name <- msg$content$target_name
      comms <- self$comm_manager$get_comms(target_name)
      private$send_message(type="comm_info_reply",
                           parent=msg,
                           socket_name="shell",
                           content=list(
                             status="ok",
                             comms=comms))
    },

    handle_comm_open = function(msg){
      target_name <- msg$content$target_name
      id <- msg$content$comm_id
      data <- msg$content$data
      self$comm_manager$handle_open(target_name,id,data)
      private$comm_parent <- msg
    },

    handle_comm_msg = function(msg){
      id <- msg$content$comm_id
      data <- msg$content$data
      self$comm_manager$handle_msg(id,data)
      private$comm_parent <- msg
    },

    handle_comm_close = function(msg){
      id <- msg$content$comm_id
      data <- msg$content$data
      self$comm_manager$handle_close(id,data)
      private$comm_parent <- msg
    },

    send_comm_msg = function(id,data,metadata=NULL){
      private$send_message(type="comm_msg",debug=FALSE,
                   parent=private$comm_parent,
                   socket_name="iopub",
                   content=list(
                     comm_id=id,
                     data=data),
                   metadata=metadata)
    },
    
    send_comm_open = function(id,target_name,data,metadata=NULL){
      private$send_message(type="comm_open",debug=FALSE,
                   parent=private$comm_parent,
                   socket_name="iopub",
                   content=list(
                     comm_id=id,
                     target_name=target_name,
                     target_module=list(),
                     data=data),
                   metadata=metadata)
    },
    
    send_comm_close = function(id,data,metadata=NULL){
      private$send_message(type="comm_close",debug=FALSE,
                   parent=private$comm_parent,
                   socket_name="iopub",
                   content=list(
                     comm_id=id,
                     data=data),
                   metadata=metadata)
    },
    
    log_out = function(message,...,use.print=FALSE){
      tstate <- tracingState(on=FALSE)
      if(use.print)
        message <- capture.output(print(message))
      else message <- paste(message,...,collapse="")
      cat(crayon::bgBlue(format(Sys.time()),message,"\n"),file=stderr())
      tracingState(on=tstate)
    }

  ),

  private = list(

    .pbd_env = new.env(),
    sockets = list(),
    zmqctx = list(),
    conn_info = list(),

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
      if(msg$header$msg_type=="shutdown_request"){
        # cat("shutdown_request received")
        return(FALSE)
      }
      else return(TRUE)
    },

    respond_shell = function(req,debug=FALSE){
      msg <- private$get_message("shell")
      if(!length(msg)) return(TRUE)
      private$send_message(type="status",
                           parent=msg,
                           socket_name="iopub",
                           content=list(
                             execution_state="busy"))
      if(debug)
        self$log_out(paste("Got a", msg$header$msg_type, "request ..."))
      # cat("Got a", msg$header$msg_type, "request ...\n")
      # do_stuff ...
      switch(msg$header$msg_type,
             comm_open = self$handle_comm_open(msg),
             comm_msg = self$handle_comm_msg(msg),
             comm_close = self$handle_comm_close(msg),
             execute_request = self$execute_reply(msg),
             is_complete_request = self$is_complete_reply(msg),
             kernel_info_request = self$kernel_info_reply(msg),
             complete_request = self$complete_reply(msg),
             comm_info_request = self$comm_info_reply(msg)
             )

      private$send_message(type="status",
                           parent=msg,
                           socket_name="iopub",
                           content=list(
                             execution_state="idle"))
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
      if(!length(private$session)){
        header <- msg$header
        private$session <- header$session
        private$username <- header$username
        # cat("Session:",private$session,"\n")
        # cat("User:",private$username,"\n")
      }
      return(msg)
    },

    send_message = function(type, parent, socket_name, debug=FALSE, content, metadata=NULL){
      msg <- private$msg_new(type,parent,content,metadata)
       if(debug) {
         msg_body <- msg[c("header","parent_header","metadata","content")]
         msg_body <- to_JSON(msg_body,pretty=TRUE,auto_unbox=TRUE)
         self$log(format(msg_body))
       }
      socket <- private$sockets[[socket_name]]
      wire_out <- private$wire_pack(msg)
      #zmq.send.multipart(socket,wire_out,serialize=FALSE)
      l <- length(wire_out)
      for(i in 1:l){
        flag <- if(i < l) private$.pbd_env$ZMQ.SR$SNDMORE 
                else private$.pbd_env$ZMQ.SR$BLOCK
        #if(debug) print(wire_out[[i]])
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

    session = character(0),
    username = character(0),

    msg_new = function(type,parent,content,metadata=NULL){
      if(is.null(metadata))
        metadata = namedList()
      if(length(parent) && "header" %in% names(parent)){
        parent_header <- parent$header
        session <- parent_header$session
        username <- parent_header$username
      }
      else {
        parent_header <- namedList()
        session <- private$session
        username <- private$username
      }
      header <- list(
        msg_id = UUIDgenerate(),
        session = session,
        username = username,
        date = strftime(as.POSIXlt(Sys.time(),"UTC"),"%Y-%m-%dT%H:%M:%OS6Z"),
        msg_type = type,
        version = PROTOCOL_VERSION
      )
      list(header = header,
           parent_header = parent_header,
           content = content,
           identities = parent$identities,
           metadata = metadata)
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
                           content=list(
                             status="aborted"))
        }
        else break
      }
    },

    comm_parent = list()

  )
)

#' @importFrom jsonlite fromJSON toJSON

to_JSON <- function(x,...){
  x <- toJSON(x,...)
  x <- gsub("[]","null",x,fixed=TRUE)
  # x <- gsub("[{}]","[]",x,fixed=TRUE)
  x
}
# to_JSON <- toJSON

fromRawJSON <- function(raw_json) {
    json <- rawToChar(raw_json)
    Encoding(json) <- "UTF-8"
    fromJSON(json)
}

toRawJSON <- function(x,...){
  json <- to_JSON(x,...)
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

get_current_kernel <- function() kernel$current

log_out <- function(...) kernel$current$log_out(...)


# Local Variables:
# ess-indent-offset: 2
# End:
