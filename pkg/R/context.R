Context <- R6Class("Context",
public=list(
  stdout_callback = NULL,
  stderr_callback = NULL,
  stdout_filter = NULL,
  stderr_filter = NULL,
  msg_handlers = NULL,
  initialize = function(stdout_callback, 
                        stderr_callback,
                        msg_handlers = NULL) {
    self$stdout_callback <- stdout_callback
    self$stderr_callback <- stderr_callback
    self$msg_handlers <- msg_handlers
    self$stdout_filter <- MessageFilter$new(
      text_handler = stdout_callback,
      msg_handler = self$handle_msg
    )
    self$stderr_filter <- MessageFilter$new(
      text_handler = stderr_callback,
      msg_handler = self$handle_msg
    )
  },
  handle_msg = function(msg) {
    # log_out("Context$handle_msg")
    type <- msg$type
    # log_out(type)
    msg_handlers <- self$msg_handlers
    if(length(msg_handlers)) {
      if(type %in% names(msg_handlers)) {
        handle <- msg_handlers[[type]]
        handle(msg)
      } else if("default"  %in% names(msg_handlers)) {
        handle <- msg_handlers$default
        handle(msg)
      }
    }
    invisible()
  },
  eval = function(expr,envir=list(),enclos=parent.frame()) {
    # log_out("Context$eval")
    serr <- ""
    sout <- ""
    serr_con <- textConnection("serr","w",local=TRUE)
    sout_con <- textConnection("sout","w",local=TRUE)
    sink(serr_con,type="message")
    sink(sout_con,type="output")
    on.exit({
      sink(type="message")
      sink(type="output")
      close(serr_con)
      close(sout_con)
    })
    r <- try(withVisible(eval(expr,
                    envir=envir,enclos=enclos)),
             silent=TRUE)
    # log_out("eval done")
    # log_out(r, use.str=TRUE)
    if(inherits(r,"try-error")) {
      cat(r,file=stderr())
    } else if(r$visible) {
      print(r$value)
    }
    on.exit()
    sink(type="message")
    sink(type="output")
    # NOTE: The following does not work if
    # output is not ended by '\n'
    #serr <- textConnectionValue(serr_con)
    #sout <- textConnectionValue(sout_con)
    close(serr_con)
    close(sout_con)
    # log_out(sprintf("sout = '%s'",sout))
    self$stderr_filter$process(serr)
    self$stdout_filter$process(sout)
  },
  graphics_observer = NULL,
  dev_num = 0,
  start_graphics = function() {
    ugd()
    self$dev_num <- dev.cur()
    self$graphics_observer <- GraphicsObserver$new(internal=TRUE)
  },
  stop_graphics = function() {
    self$graphics_observer <- NULL
    ugd_close(self$dev_num)
  },
  g_display_id = NULL,
  process_graphics = function() {
    poll_res <- self$graphics_observer$poll()
    d <- list()
    if(poll_res[1]) {
      if(poll_res[2]) {
        d <- self$graphics_observer$display_data()
        g_display_id <- display_id(d)
      } else if(poll_res[3]) {
        d <- self$graphics_observer$display_data(
          display_id = self$g_display_id,
          update = TRUE
        )
      }
    }
    if(length(d)) {
      msg <- list(type=class(d),
                  content=unclass(d))
      self$handle_msg(msg)
    }
  }
))