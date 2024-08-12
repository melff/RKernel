#' @import R6 
#' @importFrom pbdZMQ zmq.socket zmq.bind zmq.ctx.new zmq.getsockopt zmq.setsockopt
#' @importFrom pbdZMQ zmq.poll zmq.poll.get.revents zmq.msg.recv zmq.msg.send
#' @importFrom pbdZMQ zmq.recv.multipart zmq.send.multipart .zmqopt_init
#' @importFrom pbdZMQ ZMQ.MC
#' @importFrom digest hmac
#' @importFrom uuid UUIDgenerate
#' @importFrom jsonlite prettify


PROTOCOL_VERSION <- '5.3'
WIRE_DELIM <- charToRaw("<IDS|MSG>")

EOT <- "\x04"
DLE <- "\x10"
ETB <- "\x17"
ZMQ_PUSH <- "[!ZMQ_PUSH]"
JSON_MSG <- "[!JSON]"

kernel <- new.env()

fn_kernel_logfile <- file.path(dirname(tempdir()),"RKernel.log")

#' The Kernel Class
#'
#' @description An object of this class handles the low-level communication with
#'   the Jupyter frontend or kernel manager. There should only be one object of this
#'   class in existence.
#' @export
Kernel <- R6Class("Kernel",

  public = list(
    #' @description
    #' Initialize the kernel
    #' @param conn_info A list with the connection info from the front-end
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
      private$pid <- Sys.getpid()
      kernel$current <- self
    },
    start_session = function(){
      self$r_session <- RKernelSession$new(callbacks = list(
        stdout = private$handle_r_stdout,
        stderr = private$handle_r_stderr,
        msg = self$session_msg,
        readline = private$r_get_input
      ))
      # log_out(self$r_session, use.print = TRUE)
    },
    start = function(){
      self$start_session()
      private$r_install_hooks()
      private$r_start_graphics()
      private$install_r_handlers()
      private$r_init_help()
      private$r_set_help_displayed()
    },
    #' @field r_session See \code{\link{RKernelSession}}.
    r_session = list(),
    #' @field comm_manager See \code{\link{CommManagerClass}}.
    comm_manager = list(),
    #' @field DAPServer The current DAP server
    DAPServer = NULL,
    #' @description
    #' Run the kernel.
    run = function(){
      self$start()
      # log_out("*** RKernel started ***")
      continue <- TRUE
      while(continue) {
        continue <- self$poll_and_respond()
      }
      self$r_session$close()
    },
    #' @description
    #' A single iteration of the kernel loop
    poll_and_respond = function(){
        rkernel_poll_timeout <- getOption("rkernel_poll_timeout",10L)
        poll_timeout <- rkernel_poll_timeout 
        # log_out(sprintf("poll_timeout = %d",poll_timeout))
        req <- private$poll_request(c("hb","control","shell"),timeout=poll_timeout)
        # log_out("kernel$poll_request")
        if(!length(req)) return(TRUE)
        #Sys.sleep(1)
        ## if(req$abort) break
        if(req$interrupt) {
          return(TRUE)
        }
        # print(req$socket_name)
        if(!length(req$socket_name)) return(TRUE)
        switch(req$socket_name,
               hb=private$respond_hb(req),
               control=private$respond_control(req),
               shell=private$respond_shell(req))
    },
    #' @description
    #' Clear the current output cell in the frontend.
    #' @param wait Logical value, whether to wait until output is cleared.
    clear_output = function(wait){
      private$send_message(type="clear_output",
                                 parent=private$parent$shell,
                                 socket_name="iopub",
                                 content=list(wait=wait))
    },
    #' @description
    #' Stream text to the frontend.
    #' @param text Text to be sent to the frontend
    #' @param stream A string to select the stream -- either "stout" or "stderr"
    stream = function(text,stream){
      private$send_message(type="stream",
                           parent=private$parent$shell,
                           socket_name="iopub",
                           content=list(
                             name=stream,
                             text=text))
    },
    #' @description
    #' Stream text to the frontend via 'stdout' stream.
    #' @param text Text to be sent to the frontend
    stdout = function(text) {
      self$stream(text,stream="stdout")
    },
    #' @description
    #' Stream text to the frontend via 'stderr' stream.
    #' @param text Text to be sent to the frontend
    stderr = function(text) {
      self$stream(text,stream="stderr")
    },
    #' @description
    #' Stream message creatd from R session process to the frontend via 'stderr' stream.
    #' @param msg The message created by the 'RKernelSession' object.
    session_msg = function(msg) {
      text <- capture.output(print(msg))
      self$stream(text,stream="stderr")
    },
    #' @description
    #' Send execution results to the frontend
    #' @param data Execution result in rich format
    #' @param metadata A list with metadata
    execute_result = function(data,metadata=emptyNamedList){
      # log_out("kernel$execute_result")
      # log_out(sprintf("msg_type = %s",msg_type))
      content <- list(data=data,
                      metadata=metadata,
                      execution_count=private$execution_count)
      private$send_message(type="execute_result",
                           parent=private$parent$shell,
                           socket_name="iopub",
                           content=content)
    },
    #' @description
    #' Send rich format data to the frontend
    #' @param msg A list with the appropriate structure. [TODO]
    display_send = function(msg){
      log_out("display_send")
      private$send_message(type=msg$type,
                           parent=private$parent$shell,
                           socket_name="iopub",
                           content=msg$content)
      private$display_id <- msg$content$transient$display_id
      log_out("display_send succeeded")
    },
    #' @description
    #' Send rich format data to the frontend
    #' @param data A list with mime-type members.
    #' @param metadata A named list with metadata.
    #' @param transient An optional list with the current display id.
    display_data = function(data,metadata=emptyNamedList,transient=NULL){
      #content <- list(data=data,transient=transient)
      #if(length(metadata))
      #  content$metadata <- metadata
      #else
      #  content$metadata <- emptyNamedList
      # log_out("== display_data ===========")
      # for(n in names(data)){
      #   log_out("--",n,"--")
      #   log_out(data[[n]])
      # }
      if(!length(transient))
        transient <- emptyNamedList
      private$send_message(type="display_data",
                           parent=private$parent$shell,
                           socket_name="iopub",
                           content=list(
                             data=data,
                             metadata=metadata,
                             transient=transient
                           ))
    },
    #' @description
    #' Update rich format data to the frontend
    #' @param data A list with mime-type members.
    #' @param metadata A named list with metadata.
    #' @param transient An list with the current display id.
    update_display_data = function(data,metadata=emptyNamedList,transient){
      private$send_message(type="update_display_data",
                           parent=private$parent$shell,
                           socket_name="iopub",
                           content=list(
                             data=data,
                             metadata=metadata,
                             transient=transient
                           ))
    },
    #' @description
    #' Send an error message and traceback to the frontend.
    #' @param name A string, the error name.
    #' @param value A string, the value of the error message.
    #' @param traceback A character vector with the traceback.
    send_error = function(name,value,traceback){
            private$send_message(type="error",
                           parent=private$parent$shell,
                           socket="iopub",
                           content=list(
                             ename = name,
                             evalue = value,
                             traceback = traceback
                           ))
    },
    #' @description
    #' Send a message via a comm.
    #' @param id A string that identifies the comm.
    #' @param data A list with data.
    #' @param metadata An optional list with metadata.
    #' @param buffers An optional list of raw vectors.
    send_comm_msg = function(id,data,metadata=emptyNamedList,buffers=NULL){
      # log_out("kernel$send_comm_msg")
      private$send_message(type="comm_msg",debug=FALSE,
                   parent=private$parent$shell,
                   socket_name="iopub",
                   content=list(
                     comm_id=id,
                     data=data),
                   metadata=metadata,
                   buffers=buffers)
    },
    #' @description
    #' Open a comm in the frontend.
    #' @param id A string that identifies the comm.
    #' @param target_name A string that identifies a group of related comms.
    #' @param data A list with data.
    #' @param metadata An optional list with metadata.
    #' @param buffers An optional list of raw vectors.
    send_comm_open = function(id,target_name,data,metadata=emptyNamedList,buffers=NULL){
      # log_out("kernel$send_comm_open")
      private$send_message(type="comm_open",debug=FALSE,
                   parent=private$parent$shell,
                   socket_name="iopub",
                   content=list(
                     comm_id=id,
                     target_name=target_name,
                     target_module=NULL,
                     data=data),
                   metadata=metadata,
                   buffers=buffers)
    },
    #' @description
    #' Close a comm in the frontend.
    #' @param id A string that identifies the comm.
    #' @param data A list with data.
    #' @param metadata An optional list with metadata.
    #' @param buffers An optional list of raw vectors.
    send_comm_close = function(id,data=emptyNamedList,metadata=emptyNamedList,buffers=NULL){
      # log_out("kernel$send_comm_close")
      private$send_message(type="comm_close",debug=FALSE,
                   parent=private$parent$shell,
                   socket_name="iopub",
                   content=list(
                     comm_id=id,
                     data=data),
                   metadata=metadata,
                   buffers=buffers)
    },
    #' @description
    #' The parent of the message currently sent.
    #' @param channel A string, the relevant input channel.
    get_parent = function(channel="shell"){
      return(private$parent[[channel]])
    },
    #' @description
    #' Return the current connection info.
    get_conn_info = function(){
      return(private$conn_info)
    },
    #' @description
    #' Check if the current process is a fork from the original kernel process
    is_child = function(){
      return(Sys.getpid()!=private$pid)
    },
    #' @description
    #' Send an input request to the frontend
    #' @param prompt A prompt string
    #' @param password Logical value; whether the input should be hidden like in a
    #'    password dialog
    input_request = function(prompt="",password=FALSE){
      private$send_message(type="input_request",
                           parent=private$parent$shell,
                           socket_name="stdin",
                           content=list(
                             prompt=prompt,
                             password=password))
    },
    #' @description
    #' Read a line from the frontend
    read_stdin = function(){
      continue <- TRUE
      input <- ""
      while(continue){
        rkernel_poll_timeout <- getOption("rkernel_poll_timeout",10L)
        poll_timeout <- -1L
        # log_out(sprintf("poll_timeout = %d",poll_timeout))
        req <- private$poll_request("stdin",timeout=poll_timeout)
        # log_out("req")
        # log_out(req,use.str=TRUE)
        # Setting poll_timeout to anything other than -1 seems to be
        # futile right now as get_message("stdin") is blocking ...
        msg <- private$get_message("stdin")
        if(msg$header$msg_type != "input_reply")
          next
        input <- msg$content$value
        continue <- FALSE
      }
      return(input)
    },
    #' @description
    #' Send a debug event to the frontend
    #' @param content A list, content provided by the debug adapter
    send_debug_event = function(content){
      private$send_message(type="debug_event",
                           parent=private$parent$control,
                           socket="iopub",
                           content=content)
    },
    errored = FALSE
  ),

  private = list(
    pid = 0,
    execution_count = 1,
    handle_execute_request = function(msg){
      log_out("handle_execute_request")
      log_out(msg,use.print=TRUE)
      if(msg$content$silent){
        if(msg$content$store_history){
          log_warning("store_history forced to FALSE")
          msg$content$store_history <- FALSE
        }
      }
      execution_count <- private$execution_count
      private$send_message(
        type = "execute_input",
        parent = private$parent$shell,
        socket_name = "iopub",
        content = list(
          code = msg$content$code,
          execution_count = execution_count
        )
      )
      self$errored <- FALSE
      private$r_run_cell_begin_hooks()
      r <- tryCatch(self$r_session$run_code(msg$content$code),
        error = function(e) structure("errored", message = conditionMessage(e)), # ,traceback=.traceback()),
        interrupt = function(e) "interrupted"
      )
      # log_out(r, use.print = TRUE)
      private$r_run_cell_end_hooks()
      payload <- NULL
      aborted <- FALSE
      if (is.character(r) && (identical(r[1], "errored") || identical(r[1], "interrupted"))) {
        aborted <- TRUE
      }
      if (is.character(r)) {
        self$errored <- TRUE
        log_error(r)
        r_msg <- attr(r, "message")
        if (length(r_msg)) log_error(r_msg)
        tb <- attr(r, "traceback")
        if (length(tb)) {
          tb <- unlist(tb)
          #log_error(paste(tb, sep = "\n"))
          log_error(tb,use.print=TRUE)
        }
      }
      if (!self$r_session$is_alive()) {
        aborted <- TRUE
        self$stderr("\nR session ended - restarting ... ")
        self$start_session()
        self$stderr("done.\n")
      } else if (self$errored) {
          content <- list(
            status = "error",
            ename = r,
            evalue = r_msg,
            traceback = tb,
            execution_count = execution_count 
          )
      }
      else {
          # Collect output created by cell-end hooks etc.
          content <- list(status = "ok",
                          execution_count = execution_count)
          if(length(payload))
            content$payload <- payload
      }
      if(!isTRUE(msg$content$silent))
        private$send_message(type="execute_reply",
                              parent=private$parent$shell,
                              socket="shell",
                              content=content)
      if(msg$content$store_history)
        private$execution_count <- private$execution_count + 1
      if (aborted) private$clear_shell_queue()
      log_out("handle_execute_request done")
    },

    display_id = character(0),
    last_display = function() private$display_id,

    handle_debug_request = function(msg){
      request <- msg$content
      reply <- try(self$DAPServer$handle(request))
      if(inherits(reply,"try-error")){
        log_error(reply)
        return(NULL)
      }
      private$send_message(type="debug_reply",
                           parent=private$parent$control,
                           socket="control",
                           content=reply)
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
                       banner = self$r_session$banner,
                       debugger = FALSE)
      private$send_message(type="kernel_info_reply",
                           parent=private$parent$shell,
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
                           parent=private$parent$shell,
                           socket_name="shell",
                           content=list(
                             status=status,
                             indent=""))
      #cat("Sent an is_complete_reply ...\n")
    },

    complete_reply = function(msg){
      code <- msg$content$code
      cursor_pos <- msg$content$cursor_pos
      result <- get_completions(code,cursor_pos)
      private$send_message(type="complete_reply",
                           parent=private$parent$shell,
                           socket_name="shell",
                           content=list(
                             status="ok",
                             matches=result$matches,
                             cursor_start=result$start,
                             cursor_end=result$end,
                             metadata=emptyNamedList))
    },

    comm_info_reply = function(msg){
      return(NULL)
      target_name <- NULL
      if("target_name" %in% names(msg$content))
        target_name <- msg$content$target_name
      comms <- self$comm_manager$get_comms(target_name)
      comms <- list()
      private$send_message(type="comm_info_reply",
                           parent=private$parent$shell,
                           socket_name="shell",
                           content=list(
                             status="ok",
                             comms=comms))
    },

    handle_comm_open = function(msg){
      return(NULL)
      target_name <- msg$content$target_name
      id <- msg$content$comm_id
      data <- msg$content$data
      self$comm_manager$handle_open(target_name,id,data)
    },

    handle_comm_msg = function(msg){
      return(NULL)
      # log_out("kernel$handle_comm_msg")
      # log_out(msg,use.str=TRUE)
      id <- msg$content$comm_id
      data <- msg$content$data
      data$buffers <- msg$buffers
      self$comm_manager$handle_msg(id,data)
    },

    handle_comm_close = function(msg){
      return(NULL)
      id <- msg$content$comm_id
      data <- msg$content$data
      self$comm_manager$handle_close(id,data)
    },

    .pbd_env = new.env(),
    sockets = list(),
    zmqctx = list(),
    conn_info = list(),

    parent = list(),

    poll_request = function(sock_names,timeout=-1L) {
      if(self$is_child()) return(TRUE)
      POLLIN <- private$.pbd_env$ZMQ.PO$POLLIN
      req <- list()
      r <- tryCatch(
        zmq.poll(private$sockets[sock_names],
                 rep(POLLIN,length(sock_names)),
                 timeout=timeout,
                 MC=ZMQ.MC(check.eintr=TRUE)),
        interrupt = function(e) "SIGINT"
      )
      req$interrupt <- identical(r[1L],"SIGINT") 
      if(!req$interrupt){
        for(i in seq_along(sock_names)){
          if(bitwAnd(zmq.poll.get.revents(i),POLLIN)){
            req$socket_name <- sock_names[i]
            break
          }
        }
      }
      return(req)
    },

    respond_hb = function(req){
      if(self$is_child()) return(TRUE)
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
      private$parent$control <- msg
      if(msg$header$msg_type=="shutdown_request"){
        # cat("shutdown_request received")
        return(FALSE)
      }
      else if(msg$header$msg_type=="debug_request"){
        log_out("debug_request received")
        private$send_busy(private$parent$control)
        private$handle_debug_request(msg)
        private$send_idle(private$parent$control)
        return(TRUE)
      }
      else return(TRUE)
    },

    respond_shell = function(req,debug=FALSE){
      if(debug)
        log_out("respond_shell")
      msg <- private$get_message("shell")
      if(debug)
         log_out(msg,use.str=TRUE)
      private$parent$shell <- msg
      if(!length(msg)) return(TRUE)
      private$send_busy(private$parent$shell)
      if(debug)
        log_out(paste("Got a", msg$header$msg_type, "request ..."))
      # cat("Got a", msg$header$msg_type, "request ...\n")
      # do_stuff ...
      switch(msg$header$msg_type,
             comm_open = private$handle_comm_open(msg),
             comm_msg = private$handle_comm_msg(msg),
             comm_close = private$handle_comm_close(msg),
             execute_request = private$handle_execute_request(msg),
             is_complete_request = private$is_complete_reply(msg),
             kernel_info_request = private$kernel_info_reply(msg),
             complete_request = private$complete_reply(msg),
             comm_info_request = private$comm_info_reply(msg)
             )
      private$send_idle(private$parent$shell)
      return(TRUE)
    },

    send_busy = function(parent){
      private$send_message(type="status",
                           parent=parent,
                           socket_name="iopub",
                           content=list(
                             execution_state="busy"))
    },

    send_idle = function(parent){
      private$send_message(type="status",
                           parent=parent,
                           socket_name="iopub",
                           content=list(
                             execution_state="idle"))
    },
    
    get_message = function(socket_name){
      if(self$is_child()) return(NULL)
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
        # self$cat("i = %d", i)
      }
      # log_out("kernel$get_message")
      # log_out("wire_in:")
      # log_out(wire_in,use.str=TRUE)
      msg <- private$wire_unpack(wire_in)
      # log_out("msg:")
      # log_out(msg,use.str=TRUE)
      # self$cat("Got message from socket", socket_name)
      if(!length(private$session)){
        header <- msg$header
        private$session <- header$session
        private$username <- header$username
        # cat("Session:",private$session,"\n")
        # cat("User:",private$username,"\n")
      }
      return(msg)
    },

    send_message = function(type,parent,socket_name,debug=FALSE,content,
                            metadata=emptyNamedList,buffers=NULL){
      if(self$is_child()) return(NULL)
      msg <- private$msg_new(type,parent,content,metadata)
      if(debug) {
         msg_body <- msg[c("header","parent_header","metadata","content")]
         msg_body <- to_json(msg_body,auto_unbox=TRUE)
         log_out(prettify(msg_body))
         # log_out(buffers,use.print=TRUE)
       }
      socket <- private$sockets[[socket_name]]
      wire_out <- private$wire_pack(msg)
      wire_out <- append(wire_out,buffers)
      #zmq.send.multipart(socket,wire_out,serialize=FALSE)
      # if(debug) cat("\nSending message to socket", socket_name)
      l <- length(wire_out)
      for(i in 1:l){
        flag <- if(i < l) private$.pbd_env$ZMQ.SR$SNDMORE 
                else private$.pbd_env$ZMQ.SR$BLOCK
        # if(debug) {
        #   cat("\n i =",i)
        #   print(wire_out[[i]])
        # }
        zmq.msg.send(wire_out[[i]],socket,flag=flag,serialize=FALSE)
      }
      # if(debug) cat("\nSent message to socket", socket_name)
    },

    wire_unpack = function(wire_in){
      # log_out("wire_unpack")
      l <- length(wire_in)
      found <- FALSE
      # for(i in 1:l){
      #   log_out(paste(sprintf("[[%d]]",i), rawToChar(wire_in[[i]])))
      # }
      for(i in 1:l){
        if(identical(wire_in[[i]],WIRE_DELIM)) {
          found <- TRUE
          break
        }
      }
      # log_out(sprintf("i = %d",i))
      # log_out(sprintf("l = %d",l))
      if(!found) {
        log_error("WIRE_DELIM not found")
        return(NULL)
      }
      signature <- rawToChar(wire_in[[i+1]])
      # log_out(signature)json
      msg <- wire_in[i + 2:5]
      # log_out(private$get_signature(msg))
      if(signature != private$get_signature(msg)) {
        log_error("Incorrect signature")
        return(NULL)
      }
      msg <- lapply(msg,fromRawJSON,simplifyDataFrame=FALSE,simplifyMatrix=FALSE)
      names(msg) <- c("header","parent_header","metadata","content")
      if(i > 1)
        msg$identities <- wire_in[1:(i-1)]
      if(l > i + 5)
        msg$buffers <- wire_in[(i+6):l]
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

    msg_new = function(type,parent,content,metadata=emptyNamedList){
      if(is.null(metadata))
        metadata = emptyNamedList
      if(length(parent) && "header" %in% names(parent)){
        parent_header <- parent$header
        session <- parent_header$session
        username <- parent_header$username
        identities <- parent$identities
      }
      else {
        parent_header <- emptyNamedList
        session <- private$session
        username <- private$username
        identities <- NULL
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
           identities = identities,
           metadata = metadata)
    },

    clear_shell_queue = function(...){
      # Empty message queue from shell
      POLLIN <- private$.pbd_env$ZMQ.PO$POLLIN
      repeat {
        r <- zmq.poll(c(private$sockets$shell),POLLIN,timeout=0L,
                      MC=ZMQ.MC(check.eintr=TRUE))
        if(bitwAnd(zmq.poll.get.revents(1),POLLIN)){
          msg <- private$get_message("shell")
          msg_type <- msg$header$msg_type
          reply_type <- sub("_request","_reply",msg_type,fixed=TRUE)
          private$parent$shell <- msg
          private$send_message(type=reply_type,
                           parent=private$parent$shell,
                           socket_name="shell",
                           content=list(
                             status="aborted"))
        }
        else break
      }
    },

    save_io_handlers = function(){
      self$print <- .BaseNamespaceEnv$print
      self$cat <- .BaseNamespaceEnv$cat
      self$str <- utils::str
      self$httpd <- tools:::httpd
    },
    
    logfile = NULL,

    r_install_hooks = function(){
      # log_out("Installing hooks ...")
      self$r_session$run_cmd("RKernel::install_output_hooks()")
      self$r_session$run_cmd("RKernel::install_cell_hooks()")
      self$r_session$run_cmd("RKernel::install_save_q()")
      self$r_session$run_cmd("RKernel::install_readline()")
      self$r_session$run_cmd("options(error = function()print(traceback()))")
      # log_out("done.")
    },
    r_start_graphics = function(){
      # log_out("Starting graphics ...")
      self$r_session$run_cmd("RKernel::start_graphics()")
      # log_out("done.")
    },
    r_run_cell_begin_hooks = function(){
      self$r_session$run_code("RKernel::runHooks('cell-begin')")
    },
    r_run_cell_end_hooks = function(){
      self$r_session$run_code("RKernel::runHooks('cell-end')")
    },
    json_incomplete = FALSE,
    json_frag = "",
    handle_r_stdout = function(text){
      # log_out("handle_r_stdout")
      if (grepl(DLE, text)) {
        log_out("DLE found")
        text <- split_string1(text, DLE)
      }
      for(chunk in text){
        if (startsWith(chunk, JSON_MSG)) {
          log_out("JSON_MSG found")
          if (endsWith(chunk, ETB)) {
            msg <- remove_prefix(chunk, JSON_MSG) |> remove_suffix(ETB)
            private$handle_r_json(msg)
          } else {
            private$json_incomplete <- TRUE
            private$json_frag <- remove_prefix(chunk, JSON_MSG)
          }
        }
        else if(endsWith(chunk, ETB)){
          msg <- paste0(private$json_frag, remove_suffix(chunk, ETB))
          private$json_incomplete <- FALSE
          private$json_frag <- ""
          private$handle_r_json(msg)
        }
        else {
          if(private$json_incomplete) {
            private$json_frag <- paste0(private$json_frag, chunk)
          }
          else if(nzchar(chunk)) {
            self$stdout(chunk)
          }
        }
      }
      # log_out("handle_r_stdout done")
    },
    handle_r_stderr = function(text){
      if(startsWith(text, "Error"))
        self$errored <- TRUE
      self$stderr(text)
    },
    handle_r_json = function(json_msg){
      log_out("handle_r_json")
      msg <- fromJSON(json_msg)
      log_out(msg, use.str=TRUE)
      msg_type <- msg$type
      msg_handler <- private$r_msg_handlers[[msg_type]]
      if(is.function(msg_handler)){
        msg_handler(msg)
      } else {
        self$stderr(sprintf("R session sent message of unknown type '%s'", msg_type))
      }
    },
    r_msg_handlers = list(),
    install_r_handlers = function(){
      private$r_msg_handlers$display_data <- self$display_send
      private$r_msg_handlers$update_display_data <- self$display_send
    },
    r_init_help = function(){
      self$r_session$run_cmd("RKernel::init_help()")
    },
    r_set_help_displayed = function(){
      self$r_session$run_cmd("RKernel::set_help_displayed(TRUE)")
    },
    r_get_input = function(prompt = ""){
      self$input_request(prompt = prompt)
      self$read_stdin()
    }
  )
)

check_page_payload <- function(payload) {
  for (i in seq_along(payload)) {
    payload_item <- payload[[i]]
    if (payload_item$source == "page") {
      data <- payload_item$data
      if (!("text/plain" %in% names(data))) {
        payload_item$data[["text/plain"]] <- "[No plain text data for paging]"
        payload[[i]] <- payload_item
      }
    }
  }
  payload
}

get_current_kernel <- function() kernel$current

split_string1 <- function(text, pat){
  # log_out("split_string1")
  unlist(strsplit(text, pat, fixed = TRUE))
}

remove_prefix <- function(text, prefix){
  if (!startsWith(text, prefix)) {
    # Maybe issue a warning or throw an error
    return(text)
  } else {
    n <- nchar(text)
    m <- nchar(prefix)
    substr(text, start = m + 1, stop = n)
  }
}

remove_suffix <- function(text, prefix) {
  if (!endsWith(text, prefix)) {
    # Maybe issue a warning or throw an error
    return(text)
  } else {
    n <- nchar(text)
    m <- nchar(prefix)
    substr(text, start = 1, stop = n - m)
  }
}

prefix <- function(x, n) substr(x, start = 1, stop = n)
suffix <- function(x, n) substr(x, start=nchar(x) - n, nchar(x))

# Local Variables:
# ess-indent-offset: 2
# End:
