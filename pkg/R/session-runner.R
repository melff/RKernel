RSessionRunner <- R6Class("RSessionRunner",
  public = list(
    kernel = NULL,
    session = NULL,
    repl = NULL,
    initialize = function(kernel,
                          stream = kernel$stream) {
      self$kernel <- kernel
      private$kernel_stream <- stream
      self$install_msg_handlers(
        menu = private$handle_menu_request,
        event = private$handle_event_msg
      )
    },
    start = function(bare = FALSE) {
      self$session <- RKernelSession$new()
      kernel <- self$kernel
      self$session$connect(kernel = kernel,
                           yield = kernel$handle_yield)
      self$repl <- RSessionAdapter$new(
        session = self$session,
        stdout_callback =  self$handle_stdout,
        stderr_callback =  self$handle_stderr,
        browser_callback = private$handle_browser,
        input_callback =   self$readline)
      kernel <- self$kernel
      if(!bare) {
        self$session$setup()
      }
      private$start_graphics()
      private$stdout_filter <- MessageFilter$new(
          text_handler = self$stdout,
          msg_handler  = self$handle_msg
        )
      private$stderr_filter <- MessageFilter$new(
          text_handler = self$stderr,
          msg_handler  = self$handle_msg
        )
    },
    stop = function() {
      self$repl <- NULL
      self$session$close()
    },
    restart = function() {
      self$stop()
      self$start()
    },
    set_shell_parent = function(msg) {
      if(!length(msg)) stop("Empty parent message in set_shell_parent()")
      private$shell_parent = msg
    },
    get_shell_parent = function() {
      private$shell_parent
    },
    stdout = function(txt) {
      self$stream(txt, stream = "stdout")
    },
    stderr = function(txt) {
      self$stream(txt, stream = "stderr")
    },
    stream = function(txt, stream) {
      kernel <- self$kernel
      kernel$restore_shell_parent(private$shell_parent)
      private$kernel_stream(txt,stream)
    },
    display_send = function(d) {
      kernel <- self$kernel
      kernel$restore_shell_parent(private$shell_parent)
      kernel$display_send(d)
    },
    install_msg_handlers = function(...) {
      l <- list(...)
      nms <- names(l)
      if(length(l) == 1 && !length(nms)) {
        l <- l[[1]]
        nms <- names(l)
      }
      for(n in nms) {
        private$msg_handlers[[n]] <- l[[n]]
      }
    },
    settings_set = function(...) {
      l <- list(...)
      nms <- names(l)
      if(length(l) == 1 && !length(nms)) {
        l <- l[[1]]
      }
      for(name in nms) {
        value <- l[[name]]
        assign(name, value, envir = private$settings)
      }
    },
    settings_get = function(name, default = NULL) {
      get0(name, envir = private$settings, ifnotfound = default)
    },
    set_callback = function(name, FUN) {
      assign(name, FUN, envir = private$callbacks)
    },
    graphics = NULL,

    handle_msg = function(msg) {
      if(!is.list(msg)) {
        err_msg <- "R session sent an invalid message - not a list"
        # self$stderr(err_msg)
        log_error(err_msg, traceback = FALSE)
        dep_msg <- deparse0(msg)
        # self$stderr("\n")
        # self$stderr(dep_msg)
        # self$stderr("\n")
        log_error(dep_msg, traceback = FALSE)
        return(NULL)
      }
      msg_type <- msg$type
      # log_out(msg_type)
      msg_handler <- private$msg_handlers[[msg_type]]
      if(is.function(msg_handler)){
        msg_handler(msg)
      } else {
        w_msg <- sprintf("R session sent message of unknown type '%s'", msg_type)
        # log_out(msg_handler, use.str = TRUE)
        # self$stderr(w_msg)
        dep_msg <- deparse0(msg)
        # self$stderr("\n")
        # self$stderr(dep_msg)
        # self$stderr("\n")
        log_warning(paste(w_msg,dep_msg,sep=":\n "))
      }
    },
    handle_stdout = function(text) {
      private$stdout_filter$process(text)
      self$process_graphics()
    },
    handle_stderr = function(text) {
      private$stderr_filter$process(text)
      self$process_graphics()
    },

    process_graphics = function() {
      log_out("========== process_graphics")
      if(self$session$sleeping()) {
        log_out("Session is sleeping")
        poll_res <- tryCatch(self$graphics$poll(),
                      error = function(...){
                        self$graphics$restart()
                        self$graphics$poll()
                      })
        current_display_id <- as.character(self$graphics$current_display)
        log_out(sprintf("current_display_id = %s", current_display_id))
        log_print(self$graphics$displayed)
        display_ids <- names(self$graphics$displayed)
        for(display_id in display_ids) {
          log_out(sprintf("display_id = %s",display_id))
            if(!self$graphics$displayed[display_id]) {
              private$display_graphics(display_id)
              self$graphics$displayed[display_id] <- TRUE
          } 
        }
        log_print(poll_res)
        if(poll_res["active"]) {
          if(poll_res[2]) { # New plot
            # log_out("New plot")
            private$display_graphics(display_id)
          }
          else if(poll_res[3]) { # Plot update
            # log_out("Plot update")
            force_new_display <- self$settings_get("force_new_graphics_display",FALSE)
            private$display_graphics(display_id,
                                      force_new_display = force_new_display)
            self$settings_set(force_new_graphics_display=FALSE)
          }
        }
      }
      # log_out("process_graphics -- done")
    },

    readline = function(prompt="") {
      self$kernel$readline(prompt=prompt)
    }

  ),
  private = list(

    kernel_stream = NULL,
    msg_handlers = new.env(),
    settings = new.env(),

    handle_browser = function(prompt) {
      dbgConsole(runner = self,
                 prompt = prompt,
                 use_widgets = get_config("use_widgets"))
      self$kernel$restore_shell_parent(self$get_shell_parent())
      return(TRUE)
    },

    handle_event_msg = function(msg) {
      # log_out("handle_event_msg")
      # log_str(msg)
      event <- msg$content$event
      switch(event,
             new_plot = private$handle_new_plot(content = msg$content)
             )
      # log_out("handle_event_msg -- done")
    },

    start_graphics = function(...) {
      add_sync_options(c(
          "jupyter.plot.width",
          "jupyter.plot.height",
          "jupyter.plot.res",
          "jupyter.plot.formats",
          "jupyter.update.graphics"))
      self$session$start_graphics()
      self$graphics = GraphicsObserver$new(session=self$session)
    },
    stdout_filter = NULL,
    stderr_filter = NULL,
    shell_parent = NULL,

    handle_new_plot = function(content) {
      log_out("========== handle_new_plot")
      plot_id <- content$plot_id
      d <- self$graphics$new_display(plot_id)
      # log_print(self$graphics$displayed)
      # log_print(self$graphics$display_desc)
      log_str(d)
      self$display_send(d)
    },

    display_graphics = function(display_id, force_new_display = FALSE) {
      log_out("========== display_graphics")
      plot_id <- self$graphics$display_desc[[display_id]]$plot_id
      d <- self$graphics$render_display(display_id, 
                                        update = !force_new_display)
      display_id  <- d$transient$display_id
      self$graphics$displayed[display_id] <- TRUE
      log_out(sprintf("Sending plot_id = %s, display_id = %s", 
                      plot_id, display_id))
      log_str(d)
      self$display_send(d)
    }
  )
)
