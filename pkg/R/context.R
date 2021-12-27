#' @importFrom uuid UUIDgenerate

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
       attachment = NULL,

       id = character(0),
       name = character(0),
       
       initialize = function(text_callback=NULL,
                             message_callback=NULL,
                             warning_callback=NULL,
                             error_callback=NULL,
                             value_callback=NULL,
                             graphics_callback=NULL,
                             envir=new.env(),
                             attachment=new.env()){
           self$text_callback <- text_callback
           self$message_callback <- message_callback
           self$warning_callback <- warning_callback
           self$error_callback <- error_callback
           self$value_callback <- value_callback
           self$graphics_callback <- graphics_callback
           self$connection <- textConnection(NULL,"wr",local=TRUE)
           self$init_graphics()
           self$envir <- envir
           self$attachment <- attachment
           self$id <- UUIDgenerate()
           self$name <- paste0("RKernel-context:",self$id)
       },

       do = function(...,envir=list(),enclos=parent.frame()){
           expr <- substitute(...)
           if(class(expr)=="{"){
               expressions <- as.list(expr[-1])
               self$evaluate(expressions,enclos=enclos)
           }
           else
               self$eval(expr,envir=envir,enclos=enclos)
       },
       eval = function(expr,envir=list(),enclos=parent.frame())
           self$evaluate(list(expr),enclos=enclos),
       evaluate = function(expressions,envir=list(),enclos=parent.frame()){
           if(is.null(envir))
               envir <- self$envir
           self$enter()
           # n <- length(expressions)
           # i <- 0
           # log_out("context$evaluate")
           # log_out(enclos,use.print=TRUE)
           # log_out(ls(enclos),use.print=TRUE)
           for(expr in expressions){
               # i <- i + 1
               # log_out(sprintf("exressions[[%d]]",i))
               # expr <- expressions[[i]]
               ev <- list(value = NULL, visible = FALSE)
               ### See 'evaluate_call' in package "evaluate" (Yihui Xie et al.)
               try(ev <- withCallingHandlers(
                       withVisible(eval(expr,
                                        envir=envir,
                                        enclos=enclos)),
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
          # log_out("Context$handle_graphics()")
          if(self$graphics_active()){
              self$last_plot <- self$current_plot
              plt <- recordPlot()
              do_send_plot <- !plot_is_empty(plt) && !identical(self$last_plot,plt) 
              if(do_send_plot) {
                  update <- !self$plot_new_called
                  # log_out("update =",format(update))
                  self$graphics_callback(plt,update=update)
                  self$current_plot <- plt
                  self$plot_new_called <- FALSE
              }
          } #else log_out("graphics not active ...")
      },

      plot_new_called = FALSE,
      graphics_par_usr = numeric(0),

      before_plot_new_hook = function(...){
          if(self$graphics_active()){
              self$handle_text()
          }
      },

      plot_new_hook = function(...){
          # log_out("plot_new_hook")
          # log_out("self$dev_num==",self$dev_num)
          # log_out("dev.cur()==",dev.cur())
          if(self$graphics_active()){
              self$plot_new_called <- TRUE
              self$graphics_par_usr <- par("usr")
          } #else log_out("graphics not active ...")
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
              
              # log_out('self$device')
              # log_out("self$dev_num==",self$dev_num)

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
      orig.dev_num = 1,

      enter = function(enclos=parent.frame()){

          sink(self$connection,split=FALSE)
          self$orig.device <- options(device=self$device)
          if(self$dev_num == 0 || !(self$dev_num %in% dev.list()))
              self$device()
          if(self$dev_num > 1 && self$dev_num %in% dev.list() && dev.cur() != self$dev_num){
              self$orig.dev_num <- dev.cur()
              dev.set(self$dev_num)
          }

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

          attach(self$attachment,name=self$name,
                 warn.conflicts=FALSE)
          # log_out(sprintf("Attached %s",self$name))
          
          self$run_enter_hooks()

      },
      exit = function(){
          
          self$run_exit_hooks()
          # log_out(search(),use.print=TRUE)
          if(self$name %in% search()){
              # log_out(sprintf("Detaching %s",self$name))
              context_pos <- match(self$name,search())
              detach(pos=context_pos)
          }
          sink()
          options(device=self$orig.device)
          if(self$orig.dev_num > 1 && self$orig.dev_num %in% dev.list()) dev.set(self$orig.dev_num)

          setHook('plot.new',NULL,"replace")
          setHook('grid.newpage',NULL,"replace")
          setHook('before.plot.new',NULL,"replace")
          setHook('before.grid.newpage',NULL,"replace")

          suppressMessages(untrace(cat))
          suppressMessages(untrace(str))
          suppressMessages(untrace(print))
      },

      enter_hooks = list(),
      run_enter_hooks = function(){
          if(inherits(self$enter_hooks,"CallbackDispatcher"))
              self$enter_hooks$run()
      },
      on_enter = function(handler,remove=FALSE){
          if(!length(self$enter_hooks))
              self$enter_hooks <- CallbackDispatcher()
          self$enter_hooks$register(handler,remove)
      },

      exit_hooks = list(),
      run_exit_hooks = function(){
          if(inherits(self$exit_hooks,"CallbackDispatcher"))
              self$exit_hooks$run()
      },
      on_exit = function(handler,remove=FALSE){
          if(!length(self$exit_hooks))
              self$exit_hooks <- CallbackDispatcher()
          self$exit_hooks$register(handler,remove)
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
with.Context <- function(data,expr,enclos=parent.frame(),...){
    # log_out("with.Context")
    # log_out(enclos,use.print=TRUE)
    # log_out(ls(enclos),use.print=TRUE)
    data$eval(substitute(expr),enclos=enclos)
}
