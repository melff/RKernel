#' @export
Context <- R6Class("Context",
   public = list(
       connection = NULL,
       text_callback = NULL,
       message_callback = NULL,
       warning_callback = NULL,
       error_callback = NULL,
       graphics_callback = NULL,
       value_callback = NULL,
       text_output = NULL,
       prev_text_output = NULL,

       envir = NULL,
       enclos = NULL,

       initialize = function(text_callback=NULL,
                             message_callback=NULL,
                             warning_callback=NULL,
                             error_callback=NULL,
                             value_callback=NULL,
                             graphics_callback=NULL,
                             envir=new.env(),
                             enclos=NULL){
           self$text_callback <- text_callback
           self$message_callback <- message_callback
           self$warning_callback <- warning_callback
           self$error_callback <- error_callback
           self$value_callback <- value_callback
           self$graphics_callback <- graphics_callback
           self$connection <- textConnection(NULL,"wr",local=TRUE)
           self$init_graphics()
           self$envir <- envir
           if(is.environment(enclos))
               self$enclos <- enclos
           else self$enclos <- new.env()
           if(is.list(enclos)){
               for(n in names(enclos))
                   self$enclos[[n]] <- self$enclos[[n]]
           }
           # log_out(ls(enclos),use.print=TRUE)
       },

       do = function(...){
           expr <- substitute(...)
           if(class(expr)=="{"){
               expressions <- as.list(expr[-1])
               self$evaluate(expressions)
           }
           else
               self$eval(expr)
       },
       eval = function(expr) self$evaluate(list(expr)),
       evaluate = function(expressions){
           self$enter()
           n <- length(expressions)
           for(expr in expressions){
               # log_out(sprintf("exressions[[%d]]",i))
               # expr <- expressions[[i]]
               ev <- list(value = NULL, visible = FALSE)
               ### See 'evaluate_call' in package "evaluate" (Yihui Xie et al.)
               try(ev <- withCallingHandlers(
                       withVisible(eval(expr,envir=self$envir)),
                       error=self$eHandler,
                       warning=self$wHandler,
                       message=self$mHandler),silent=TRUE)
               self$handle_text()
               self$handle_graphics()
               if(is.function(self$value_callback)){
                   # log_out("handling return value")
                   try(ev <- withCallingHandlers(
                           self$value_callback(ev$value,ev$visible),
                           error=self$eHandler,
                           warning=self$wHandler,
                           message=self$mHandler),silent=TRUE)
                   self$handle_text()
                   self$handle_graphics()
               }
           }
           self$exit()
       },
       mHandler = function(m) {
           self$message_callback(m)
           invokeRestart("muffleMessage")
       },
       wHandler = function(w){
           self$warning_callback(w)
           if (getOption("warn") >= 2) return()
           invokeRestart("muffleWarning")
       },
       eHandler = function(e) {
           self$error_callback(e)
       },
       handle_text = function(new_line=FALSE){
           if(!is.function(self$text_callback)) return()
           if(isIncomplete(self$connection) || new_line)
               cat("\n",file=self$connection)
           self$prev_text_output <- self$text_output
           self$text_output <- textConnectionValue(self$connection)
           if(is.function(self$text_callback)){
               nlines <- length(self$prev_text_output)
               if(nlines > 0)
                   current_text_output <- tail(self$text_output,-nlines)
               else
                   current_text_output <- self$text_output
               if(length(current_text_output)){
                   current_text_output <- paste(current_text_output,collapse="\n")
                   self$text_callback(current_text_output)
               }
           }
       },
      graphics_active = function(){
          res <- self$dev_num > 1 && self$dev_num == dev.cur()
          return(res)
      },

      handle_graphics = function(){
          if(!is.function(self$graphics_callback)) return()
          if(self$graphics_active()){
              # log_out("Context$handle_graphics()")
              self$last_plot <- self$current_plot
              plt <- recordPlot()
              do_send_plot <- !plot_is_empty(plt) && !identical(self$last_plot,plt) 
              if(do_send_plot) {
                  update <- !self$plot_new_called
                  # log_out(format(update))
                  self$graphics_callback(plt,update=update)
                  self$current_plot <- plt
                  self$plot_new_called <- FALSE
              }
          }
      },

      plot_new_called = FALSE,
      graphics_par_usr = numeric(0),

      before_plot_new_hook = function(...){
          if(self$graphics_active()){
              self$handle_text()
          }
      },

      plot_new_hook = function(...){
          if(self$graphics_active()){
              self$plot_new_called <- TRUE
              self$graphics_par_usr <- par("usr")
          }
      },

      dev_filename = character(0),
      dev_name = character(),
      dev_num = 0,
      device = list(),
      last_plot = NULL,
      current_plot = NULL,

      init_graphics = function(){
          os <- .Platform$OS.type
          sysname <- Sys.info()[["sysname"]]
          if(os == "unix" && sysname=="Darwin")
              os <- "osx"
          self$dev_filename <- switch(os,
                                      windows="NUL",
                                      osx=NULL,
                                      unix="/dev/null")
          self$dev_name <- switch(os,
                                  windows="png",
                                  osx="pdf",
                                  unix="png")
          self$device <- function(filename = NULL,
                                 width = getOption("jupyter.plot.width",6),
                                 height = getOption("jupyter.plot.height",6),
                                 res = getOption("jupyter.plot.res",96),
                                 pointsize = getOption("jupyter.plot.pointsize",12),
                                 units = getOption("jupyter.plot.units","in"),
                                 ...){
              dev <- get(self$dev_name)
              if(is.null(filename))
                  dev(filename=self$dev_filename,
                      width=width,
                      height=height,
                      res=res,
                      units=units,
                      ...)
              self$dev_num <- dev.cur()
              dev.control(displaylist="enable")
          }
      },

      cat_hook = function(){
          # log_out("cat_hook")
          self$handle_graphics()
      },
      cat_exit_hook = function(){
          # log_out("cat_exit_hook")
          self$handle_text()
      },

      print_hook = function(){
          self$handle_graphics()
      },
      print_exit_hook = function(){
          self$handle_text(new_line=TRUE)
      },

      str_depth = 0,
      str_hook = function(){
          # log_out("str_hook")
          self$str_depth <- self$str_depth + 1
          if(self$str_depth == 1)
              suppressMessages(untrace(cat))
      },
      str_exit_hook = function(){
          # log_out("str_exit_hook")
          if(self$str_depth == 1){
              self$handle_text()
              suppressMessages(trace(cat,
                                     self$cat_hook,
                                     exit=self$cat_exit_hook,
                                     print=FALSE))
          }
          self$str_depth <- self$str_depth - 1
      },

      orig.device = NULL,
      enter = function(){

          sink(self$connection,split=FALSE)
          self$orig.device <- options(device=self$device)

          setHook('plot.new',self$plot_new_hook)
          setHook('grid.newpage',self$plot_new_hook)
          setHook('before.plot.new',self$before_plot_new_hook)
          setHook('before.grid.newpage',self$before_plot_new_hook)

          suppressMessages(trace(print,
                                 self$print_hook,
                                 exit=self$print_exit_hook,
                                 print=FALSE))
          suppressMessages(trace(cat,
                                 self$cat_hook,
                                 exit=self$cat_exit_hook,
                                 print=FALSE))
          suppressMessages(trace(str,
                                 self$str_hook,
                                 exit=self$str_exit_hook,
                                 print=FALSE))

          attach(self$enclos,name="RKernel::Context",
                 warn.conflicts=FALSE)

      },
      exit = function(){
          
          detach("RKernel::Context")
          sink()
          options(device=self$orig.device)

          setHook('plot.new',NULL,"replace")
          setHook('grid.newpage',NULL,"replace")
          setHook('before.plot.new',NULL,"replace")
          setHook('before.grid.newpage',NULL,"replace")

          suppressMessages(untrace(cat))
          suppressMessages(untrace(str))
          suppressMessages(untrace(print))
      }
   )
)

# cf. 'graphics.r' in package "evaluate" (Yihui Xie et al.)
empty_plot_calls <- c("palette",
                      "palette2",
                      paste0("C_",c("layout",
                                  "par",
                                  "clip",
                                  "strWidth",
                                  "strHeight",
                                  "plot_new",
                                  "plot_window")))

plot_is_empty <- function(plt) {
    if(!length(plt)) return(TRUE)
    pcalls <- plot_calls(plt)
    # log_out(pcalls,use.print=TRUE)
    # log_out(empty_plot_calls,use.print=TRUE)
    # log_out(pcalls%in%empty_plot_calls,use.print=TRUE)
    if(!length(pcalls)) return(TRUE)
    res <- all(pcalls %in% empty_plot_calls)
    # log_out(res)
    return(res)
}

plot_calls <- function(plt){
    plt <- plt[[1]]
    plt <- lapply(plt,"[[",2)
    if(!length(plt)) return(NULL)
    plt <- lapply(plt,"[[",1)
    sapply(plt,get_name_el)
}

get_name_el <- function(x){
    if(length(x$name)) x$name 
    else deparse(x)
}

#' @export
with.Context <- function(data,expr,...) data$eval(substitute(expr))
