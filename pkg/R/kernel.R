#' @import R6 
#' @importFrom pbdZMQ zmq.socket zmq.bind zmq.ctx.new zmq.getsockopt zmq.setsockopt
#' @importFrom pbdZMQ zmq.poll zmq.poll.get.revents zmq.msg.recv zmq.msg.send
#' @importFrom pbdZMQ zmq.recv.multipart zmq.send.multipart .zmqopt_init
#' @importFrom pbdZMQ ZMQ.MC random_open_port
#' @importFrom digest hmac
#' @importFrom uuid UUIDgenerate
#' @importFrom jsonlite prettify


PROTOCOL_VERSION <- '5.3'
WIRE_DELIM <- charToRaw("<IDS|MSG>")

EOT <- "\x04"
DLE <- "\x10"
ETB <- "\x17"

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
      truncate_log()
      private$sandbox <- new.env()
      assign("kernel",self,envir=private$sandbox)
      config$use_widgets <- FALSE
    },
    start = function(){
      runner <- RSessionRunner$new(self, self$stream)
      runner$start()
      self$runner <- runner
      self$session <- runner$session
      self$repl <- runner$repl
      private$install_r_handlers()
      self$DAPServer <- DAPServer$new(
        r_session = self$session,
        send_debug_event = self$send_debug_event,
        r_send_request = private$r_send_request,
        r_send_cmd = private$r_send_cmd,
        r_send_input = self$session$send_input
      )
      set_config(browser_in_condition = FALSE)
      msg_env$send <- self$runner$handle_msg
      assign("session",self$session,envir=private$sandbox)
      assign("repl",self$repl,envir=private$sandbox)
      register_magic_handler("plots",function(...){
        frm <- IFrame(url = self$runner$graphics$live_url(),
                      width="7in",
                      height="5in")
        self$display_send(frm)
      })
    },
    #' @field session See \code{\link{RKernelSession}}.
    session = list(),
    #' @field repl An RSessionAdapter for handling input and output
    repl = list(),
    #' @field runner An RSessionRunner
    runner = list(),
    #' @field DAPServer The current DAP server
    DAPServer = NULL,
    #' @description Run some code (for testing purposes)
    #' @param code Some code
    run_code = function(code) {
      self$repl$run_code(code)
      self$runner$display_changed_graphics()
    },
    #' @description
    #' Run the kernel.
    run = function(){
      self$start()
      # log_out("*** RKernel started ***")
      private$frontend_present <- TRUE
      rkernel_poll_timeout <- getOption("rkernel_poll_timeout",1000L)
      continue <- TRUE
      globalCallingHandlers(error=function(...) {
        private$show_error_traceback()
        tryInvokeRestart("continue")
      })
      while(continue) {
        continue <- withRestarts(
                      self$poll_and_respond(rkernel_poll_timeout),
                      abort = function(...) FALSE,
                      continue = function(...) TRUE)
      }
      # log_out("*** RKernel shut down ***")
    },
    #' @description
    #' A single iteration of the kernel loop
    poll_and_respond = function(
      poll_timeout = getOption("rkernel_poll_timeout",10L),
      drop = NULL
      ){
        private$run_services()
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
               shell=private$respond_shell(req, drop = drop))
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
      if(private$frontend_present) {
        private$send_message(type="stream",
                            parent=private$parent$shell,
                            socket_name="iopub",
                            content=list(
                              name=stream,
                              text=text))
      } else {
        outfile <- switch(stream, stdout=stdout(), stderr=stderr())
        cat(text, file = outfile)
      }
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
    readline = function(prompt="") {
      private$r_get_input(prompt=prompt)
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
      if(inherits(msg, "display_data") || inherits(msg, "update_display_data")) {
        d <- msg
        msg <- list(type = class(d),
                    content = unclass(d))
      }
      # log_out("display_send")
      # log_str(d)
      # log_str(msg)
      # log_out(msg$content$transient$display_id)
      private$send_message(type=msg$type,
                           parent=private$parent$shell,
                           socket_name="iopub",
                           content=msg$content)
      private$display_id <- msg$content$transient$display_id
      # log_out("display_send succeeded")
      invisible()
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
    #' @param msg A list containing a comm message.
    send_comm = function(msg){
      # log_out("kernel$send_comm")
      # log_out(msg, use.str=TRUE)
      private$send_message(type=msg$type,debug=FALSE,
                   parent=private$parent$shell,
                   socket_name="iopub",
                   content=msg$content,
                   metadata=msg$metadata,
                   buffers=msg$buffers)
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
    errored = FALSE,
    save_shell_parent = function() {
      # log_out("save_shell_parent")
      # log_out(private$parent$shell$header$msg_id)
      private$parent$shell
    },
    restore_shell_parent = function(saved_parent) {
      if(!length(saved_parent) && 
         private$frontend_present) stop("Empty parent message in restore_shell_parent()")
      private$parent$shell <- saved_parent
    },
    stop_on_error = TRUE,

    shutdown = function() {
      self$session$close()
      invokeRestart("abort")
    },
    restart = function() {
      self$session$close()
      private$clear_shell_queue()
      self$start()
    },
    restore_execute_parent = function() {
      self$restore_shell_parent(private$execute_parent)
    },

    handle_yield = function(timeout) {
      self$poll_and_respond(timeout,
                            drop="execute_request")
    },

    add_service = function(FUN) {
      if(is.function(FUN)) {
        n <- length(private$services) + 1
        private$services[[n]] <- FUN
        private$service_parents[[n]] <- self$save_shell_parent()
      }
    }
  ),

  private = list(

    frontend_present = FALSE,

    pid = 0,
    execution_count = 1,
    execute_parent = NULL,
    handle_execute_request = function(msg){
      # log_out("handle_execute_request")
      # log_out(msg, use.str = TRUE)
      if(msg$content$silent){
        if(msg$content$store_history){
          log_warning("store_history forced to FALSE")
          msg$content$store_history <- FALSE
        }
      }
      private$execute_parent <- private$parent$shell
      self$runner$set_shell_parent(private$execute_parent)
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
      self$stop_on_error <- (msg$content$stop_on_error &&
                             self$repl$getOption("rkernel_stop_on_error",TRUE))
      clear_queue <- FALSE
      code <- msg$content$code

      mparsed <- parse_magic(code)
      if(length(mparsed) > 0){
          magic <- mparsed$magic
          args <- mparsed$args
          code <- mparsed$code
          if(magic == "kernel") {
              # log_out("kernel magic found")
              # log_out(code)
              expr <- str2expression(code)
              tryCatch(eval_capture(expr, 
                                    envir = private$sandbox,
                                    enclos = private$sandbox,
                                    stdout = self$stdout,
                                    stderr = self$stderr),
                      error = function(e) {
                          m <- conditionMessage(e)
                          log_error(m)
                          self$stderr(m)
                          self$errored <- TRUE
                        }
                      )
              if(self$errored)
                clear_queue <- TRUE
          } else if(magic == "detached") {
            det_runner <- RSessionRunner$new(self, self$stream)
            det_runner$set_shell_parent(private$execute_parent)
            det_runner$stdout("Starting new detached session ...\n")
            det_runner$start()
            ses_info <- capture.output(print(det_runner$session))
            ses_info <- paste0(ses_info,"\n")
            det_runner$stdout(ses_info)
            det_repl <- det_runner$repl
            det_repl$run_code(code)
            det_runner$display_changed_graphics()
            det_runner$stop()
            ses_info <- capture.output(print(det_runner$session))
            ses_info <- paste0("\n",ses_info,"\n")
            det_runner$stdout(ses_info)
            det_runner$stdout("Detached session finished.\n")
          } else {
              d <- tryCatch(dispatch_magic_handler(magic,code,args),
                            error = function(e) structure("errored", 
                                                          message = conditionMessage(e)), # ,traceback=.traceback()),
                            interrupt = function(e) "interrupted")
              # log_out(d,use.str=TRUE)
              if(is.character(d)){
                self$stderr(attr(d,"message"))
                self$errored <- TRUE
                if(self$stop_on_error)
                  clear_queue <- TRUE
                }
              else if(inherits(d,"display_data")){
                self$display_send(d)
              }
          } 
          if(self$errored) {
            status <- "errored"
          }
          else {
            status <- "ok"
          }
          content <- list(status = status,
                          execution_count = execution_count)
      }
      else {
          if(grepl(special_regex,code)) {
            speco <- get_special_comments(code)
            # log_out(speco, use.print=TRUE)
            for(s in speco) {
              dispatch_special_comment_handler(opcode = s$opcode,
                                               args = s$args)
            }
          }
          withRestarts(private$run_code_cell(code),
                       continue = function(...) invisible())
          payload <- NULL
          if (!self$session$is_alive()) {
            clear_queue <- TRUE
            self$stderr("\nR session ended - restarting ... ")
            self$restart()
            self$stderr("done.\n")
            clear_queue <- TRUE
          } else if (self$errored) {
            if(length(private$err_msg)) {
              content <- private$err_msg
              content$status <- "error"
              content$execution_count <- execution_count
              private$err_msg <- NULL
            } else {
              content <- list(status = "error",
                              execution_count = execution_count)
            }
            if(self$stop_on_error)
              clear_queue <- TRUE
          }
          else {               
              content <- list(status = "ok",
                              execution_count = execution_count)
              if(length(payload))
                content$payload <- payload
          }
          restore_options()
      }
      private$parent$shell <- private$execute_parent
      if(!isTRUE(msg$content$silent))
        private$send_message(type="execute_reply",
                              parent=private$parent$shell,
                              socket="shell",
                              content=content)
      if(msg$content$store_history)
        private$execution_count <- private$execution_count + 1
      if(clear_queue) {
        private$clear_queue_requested <- TRUE
      }
      # log_out("handle_execute_request done")
    },

    display_id = character(0),
    last_display = function() private$display_id,

    handle_debug_request = function(msg){
      # log_out("handle_debug_request")
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
                       banner = self$session$banner,
                       debugger = FALSE)
      private$send_message(type="kernel_info_reply",
                           parent=private$parent$shell,
                           socket_name="shell",
                           content=response)
      # log_out("Sent a kernel_info_reply ...\n")
    },

    is_complete_reply = function(msg){
      #cat("is_complete_reply\n")
      #str(msg)
      code <- msg$content$code
      status <- code_status(code)
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

    inspect_reply = function(msg){
      # return(NULL)
      # log_out("inspect_reply")
      reply <- private$r_send_request(list(
        type = "inspect_request",
        content = msg$content
      ))
      # log_out(reply, use.print=TRUE)
      private$send_message(
        type = "inspect_reply",
        parent = private$parent$shell,
        socket_name = "shell",
        content = reply$content
      )
    },

    comm_info_reply = function(msg){
      # return(NULL)
      # log_out("comm_info_reply")
      target <- msg$content$target_name
      if(target == "jupyter.widget") {
        config$use_widgets <- TRUE
        self$repl$run_cmd("RKernel:::set_config(use_widgets=TRUE)")
      }
      reply <- private$r_send_request(list(
        type = "comm_info_request",
        content = msg$content
      ))
      # log_out(reply, use.print = TRUE)
      if(reply$type != "comm_info_reply"){
        log_error(sprintf("Expected a 'comm_info_reply', got a '%s' message.", reply$type))
      }
      private$send_message(
        type = "comm_info_reply",
        parent = private$parent$shell,
        socket_name = "shell",
        content = reply$content
      )
    },

    handle_comm_open = function(msg){
      # return(NULL)
      target <- msg$content$target_name
      if(target == "jupyter.widget.control") {
        config$use_widgets <- TRUE
        self$repl$run_cmd("RKernel:::set_config(use_widgets=TRUE)")
      }
      private$r_send_request_noreply(list(
        type = "comm_open",
        content = msg$content
      ))
    },

    handle_comm_msg = function(msg){
      cm <- get_comm_manager()
      id <- msg$content$comm_id
      if(cm$has(id)) {
        data <- msg$content$data
        data$buffers <- msg$buffers
        cm$handle_msg(id, data)
      }
      else {
        private$r_send_request_noreply(list(
          type = "comm_msg",
          content = msg$content
        ))
      }
    },

    handle_comm_close = function(msg){
      # return(NULL)
      private$r_send_request_noreply(list(
        type = "comm_close",
        content = msg$content
      ))
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
      if(req$interrupt) {
        # log_out("Interrupt!!")
        self$repl$interrupt()
      } else {
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

    respond_control = function(req, debug = FALSE){
      # log_out("respond_control")
      msg <- private$get_message("control")
      if(!length(msg)) return(TRUE)
      if (debug) {
        log_out("respond_control")
        log_out(paste("Got a", msg$header$msg_type, "request ..."))
      }
      private$parent$control <- msg
      private$send_busy(private$parent$control)
      continue <- TRUE
      if(msg$header$msg_type=="shutdown_request"){
        # log_out("shutdown_request received")
        # log_out(msg$content)
        restart <- msg$content$restart
        self$session$close()
        response <- list(
          status = "ok",
          restart = restart
        )
        private$send_message(type="shutdown_reply",
                           parent=private$parent$shell,
                           socket_name="iopub",
                           content=response)
        # log_out("shutdown_reply sent")
        continue <- FALSE
      }
      else if(msg$header$msg_type=="debug_request"){
        # log_out("debug_request received")
        private$handle_debug_request(msg)
      }
      private$send_idle(private$parent$control) 
      return(continue)
    },

    respond_shell = function(req, debug=FALSE, drop = NULL){
      if(debug)
        log_out("respond_shell")
      msg <- private$get_message("shell")
      msg_type <- msg$header$msg_type
      if(debug)
        log_out(paste("Got a", msg_type, "request ..."))
      if(length(drop) && is.character(drop) && msg_type %in% drop) {
        private$abort_reply(msg)
        return(TRUE)
      }
      # if(debug)
      #    log_out(msg,use.str=TRUE)
      private$parent$shell <- msg
      if(!length(msg)) return(TRUE)
      private$send_busy(private$parent$shell)
      # do_stuff ...
      private$clear_queue_requested <- FALSE
      switch(msg_type,
             execute_request = private$handle_execute_request(msg),
             is_complete_request = private$is_complete_reply(msg),
             kernel_info_request = private$kernel_info_reply(msg),
             complete_request = private$complete_reply(msg),
             inspect_request = private$inspect_reply(msg),
             comm_info_request = private$comm_info_reply(msg),
             comm_open = private$handle_comm_open(msg),
             comm_msg = private$handle_comm_msg(msg),
             comm_close = private$handle_comm_close(msg)
             )
      private$send_idle(private$parent$shell)
      if (private$clear_queue_requested) private$clear_shell_queue()
      if (debug) log_out("done")
      return(TRUE)
    },

    send_busy = function(parent){
      # log_out("send_busy")
      private$send_message(type="status",
                           parent=parent,
                           socket_name="iopub",
                           content=list(
                             execution_state="busy"))
    },

    send_idle = function(parent){
      # log_out("send_idle")
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
      if(!length(private$wire_session)){
        header <- msg$header
        private$wire_session <- header$session
        private$username <- header$username
        # cat("Session:",private$wire_session,"\n")
        # cat("User:",private$username,"\n")
      }
      return(msg)
    },

    send_message = function(type,parent,socket_name,debug=FALSE,content,
                            metadata=emptyNamedList,buffers=NULL){
      msg <- private$msg_new(type,parent,content,metadata)
      if(private$frontend_present) {
        if(debug) {
          #msg_body <- msg[c("header","parent_header","metadata","content")]
          #msg_body <- to_json(msg_body,auto_unbox=TRUE)
          msg_json <- to_json(msg,auto_unbox=TRUE)
          log_out(prettify(msg_json))
          # log_out(buffers,use.print=TRUE)
        }
        socket <- private$sockets[[socket_name]]
        wire_out <- private$wire_pack(msg)
        wire_out <- append(wire_out,buffers)
        #zmq.send.multipart(socket,wire_out,serialize=FALSE)
        # if(debug) log_out("\nSending message to socket", socket_name)
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
        # if(debug) log_out("\nSent message to socket", socket_name)
      } else {
        str(msg)
      }
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
      # log_out(signature)
      msg <- wire_in[i + 2:5]
      # log_out(private$get_signature(msg))
      if(signature != private$get_signature(msg)) {
        log_error("Incorrect signature")
        log_out(msg, use.print = TRUE)
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

    wire_session = character(0),
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
        if(private$frontend_present) stop("Empty parent message")
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
    clear_queue_requested = FALSE,
    clear_shell_queue = function(...){
      # Empty message queue from shell
      # log_out("clear_shell_queue")
      POLLIN <- private$.pbd_env$ZMQ.PO$POLLIN
      repeat {
        r <- zmq.poll(c(private$sockets$shell),POLLIN,timeout=0L,
                      MC=ZMQ.MC(check.eintr=TRUE))
        if(bitwAnd(zmq.poll.get.revents(1),POLLIN)){
          msg <- private$get_message("shell")
          private$abort_reply(msg, restore = FALSE)
        }
        else break
      }
      # log_out("clear_shell_queue done")
    },
    
    abort_reply = function(msg, restore = TRUE) {
      save_parent <- private$parent$shell
      private$parent$shell <- msg
      private$send_busy(private$parent$shell)
      msg_type <- msg$header$msg_type
      reply_type <- sub("_request","_reply",msg_type,fixed=TRUE)
      private$send_message(type=reply_type,
                           parent=private$parent$shell,
                           socket_name="shell",
                           content=list(
                             status="aborted"))
      private$send_idle(private$parent$shell)
      if(restore)
        private$parent$shell <- save_parent
    },

    stdout_filter = NULL,
    stderr_filter = NULL,
    r_msg_handlers = list(),
    install_r_handlers = function(){
      self$runner$install_msg_handlers(
        display_data = self$display_send,
        update_display_data = self$display_send,
        options = private$handle_options_msg,
        comm_msg = self$send_comm,
        comm_open = self$send_comm,
        comm_close = self$send_comm,
        condition = private$handle_condition_msg
      )
    },
    input_suspended = TRUE,
    r_get_input = function(prompt = ""){
      if(!private$input_suspended) {
        if(config$use_widgets) {
          widget_readline(self, prompt)
        } else {
          if(private$frontend_present) {
            self$input_request(prompt = prompt)
            self$read_stdin()
          } else {
            readline(prompt=prompt)
          }
        }
      }
    },
    r_send_request = function(msg){
      # log_out("r_send_request")
      msg_dput <- wrap_dput(msg)
      cmd <- paste0("RKernel::handle_request(", msg_dput, ")")
      resp <- self$repl$run_cmd(cmd)
      # log_out("Response:")
      # log_out(resp$stdout)
      resp <- remove_prefix(resp$stdout, DLE) |> remove_suffix(DLE)
      # log_out(resp)
      resp <- remove_prefix(resp, MSG_BEGIN) |> remove_suffix(MSG_END)
      # log_out(resp)
      msg_unwrap(resp)
    },
    r_send_request_noreply = function(msg){
      # log_out("r_send_request_noreply")
      msg_dput <- wrap_dput(msg)
      cmd <- paste0("RKernel::handle_request(", msg_dput, ")")
      self$repl$run_cmd(cmd)
      return(invisible())
    },
    r_send_cmd = function(cmd) {
      resp <- self$repl$run_cmd(cmd)
      msg <- msg_extract(resp$stdout)
      msg_unwrap(msg)
    },
    handle_options_msg = function(msg){
      # log_out("handle_options_msg")
      #log_out(msg, use.str = TRUE)
      opts <- msg$content
      # log_out(opts, use.str = TRUE)
      import_options(opts)
      #res <- do.call("options",opts)
      #log_out(res, use.str = TRUE)
      #log_out(.Options[names(res)], use.str = TRUE)
    },

    sandbox = NULL,

    err_msg = NULL,
    handle_condition_msg = function(msg) {
      # log_out("handle_condition_message")
      msg_content <- msg$content
      condition <- msg_content$condition
      options <- msg_content$options
      if(condition == "error") {
        self$errored <- TRUE
        self$stop_on_error <- options$rkernel_stop_on_error
        # log_out(condition, use.str=TRUE)
        if(options$rkernel_show_traceback) {
            tb <- msg_content$traceback
            tb <- c("Traceback:",tb)
        }
        else {
            tb <- NULL
        }
        content <- list(ename="error",
                        evalue=msg_content$message,
                        traceback=tb)
        private$send_message(type="error",
                          parent=private$parent$shell,
                          socket="iopub",
                          content=content)
        private$err_msg <- content
      }
    },

    graphics_new_cell = FALSE,

    run_code_cell = function(code) {
      # log_out("= run_code_cell ========================")
      private$graphics_new_cell <- TRUE
      private$input_suspended <- FALSE
      exec_parent <- private$parent$shell
      on.exit(private$input_suspended <- TRUE)
      self$runner$settings_set(
        force_new_graphics_display = !getOption("jupyter.update.graphics",TRUE)
        # Will be reset to FALSE afer graphics update
        )
      if(getOption("trace_cell",FALSE)) {
        sleep_duration <- getOption("trace_sleep",1)
        trace_interactive <- getOption("trace_interactive",TRUE)
        code_lines <- split_lines1(code)
        if(trace_interactive && config$use_widgets ) {
          tracer <- CellTracer$new(self$runner)
          tracer$run(code_lines)
        }
        else {
          for(line in code_lines) {
            self$errored <- FALSE 
            self$repl$run_code( 
                              line, 
                              io_timeout=10, 
                              echo = TRUE,
                              prompt_callback = function() {
                                    self$stdout("> ")
                                    return(TRUE)
                              }
                          )
            self$runner$display_changed_graphics()
            if(self$errored && 
              self$stop_on_error) break
            Sys.sleep(sleep_duration)
          }
        }
      }
      else {
        code_blocks <- preproc_code(code)
        for(block in code_blocks) {
          self$errored <- FALSE 
          # log_out("- run code block ----")
          # log_out(block, use.str=TRUE)
          self$repl$run_code(block,io_timeout=10)
          self$runner$display_changed_graphics()
          # log_out("- done running code block ----")
          # log_out(sprintf("kernel$errored: %s",self$errored))
          if(self$errored) {
            if(self$stop_on_error) break
          }
        }
      }
    },

    show_error_traceback = function() {
        msg <- geterrmessage()
        out <- function(...) {
          msg <- paste(...)
          if(private$frontend_present) {
            self$stderr(msg)
          } else {
            cat(msg, file = stderr())
          }
          log_error(msg)
        }
        out("------------------------------------------------\n")
        out("Error: ",msg,"\n", sep = "")
        calls <- sys.calls()
        calls <- limitedLabels(calls)
        calls <- tail(calls, -5)
        calls <- head(calls, -2)
        n <- length(calls)
        last_call <- calls[n]
        s <- get_match(last_call, "#[0-9]+: ")
        last_call <- strsplit(last_call, "#[0-9]+: ")[[1]]
        last_call <- paste0(last_call[1], s, "<-- Error likely to have occurred here.")
        calls[n] <- last_call
        calls <- paste(1:n, calls, sep = ": ")
        calls <- paste(calls, collapse = "\n")
        out("Traceback: \n")
        out(calls,"\n")
        out("------------------------------------------------\n")
    },

    services = list(),
    service_parents = list(),
    run_services = function() {
      n <- length(private$services)
      if(n > 0) {
        to_keep <- logical(n)
        to_keep[] <- TRUE
        saved_parent <- self$save_shell_parent()
        for(i in 1:n) {
          service <- private$services[[i]]
          if(is.function(service)) {
            self$restore_shell_parent(private$service_parents[[i]])
            r <- tryCatch(service(),
                          error = function(e) {
                            log_error(conditionMessage(e))
                            return(FALSE)
                          })
            to_keep[i] <- isTRUE(r)
          } else {
            to_keep[i] <- FALSE
          }
        }
        self$restore_shell_parent(saved_parent)
        private$services <- private$services[to_keep]
        private$service_parents <- private$service_parents[to_keep]
      }
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

config <- new.env()

set_config <- function(...) {
  args <- list(...)
  for(n in names(args)) {
    assign(n,args[[n]],envir=config)
  }
}

get_config <- function(name) {
  get0(name,envir=config)
}

# Local Variables:
# ess-indent-offset: 2
# End:
