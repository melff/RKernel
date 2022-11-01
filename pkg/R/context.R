#' Context Objects
#'
#' @description Objects of this class are used to capture textual and graphical
#'    output, e.g. to display them in Jupyter notebook cells or dedicated
#'    widgets.
#' @importFrom uuid UUIDgenerate
#' @export
Context <- R6Class("Context",
   public = list(
       #' @description 
       #' Initialize the object
       #' @param text_callback An optional callback function to handle text output
       #' @param message_callback An optional callback function to handle messages
       #' @param warning_callback An optional callback function to handle warnings
       #' @param error_callback An optional callback function to handle errors
       #' @param value_callback An optional callback function to handle return values
       #' @param graphics_callback An optional callback function to handle graphics
       #' @param envir An optional environment within which expressions are evaluated
       #' @param attachment An optional environment that is attached while an expression is evaluated within 
       #'   the context.
       initialize = function(text_callback=NULL,
                             message_callback=NULL,
                             warning_callback=NULL,
                             error_callback=NULL,
                             value_callback=NULL,
                             graphics_callback=NULL,
                             envir=new.env(),
                             attachment=new.env()){
           private$text_callback <- text_callback
           private$message_callback <- message_callback
           private$warning_callback <- warning_callback
           private$error_callback <- error_callback
           private$value_callback <- value_callback
           private$graphics_callback <- graphics_callback
           private$connection <- textConnection(NULL,"wr",local=TRUE)
           private$init_graphics()
           private$envir <- envir
           private$attachment <- attachment
           private$id <- UUIDgenerate()
           private$name <- paste0("RKernel-context:",private$id)
       },

       #' @description
       #' Evaluate one or several expressions
       #' @param ... A single expression or several expressions
       #'   included in curly braces.
       #' @param envir A list or an environment
       #' @param enclos An enclosing an environment (see \code{\link{eval}}).
       do = function(...,envir=list(),enclos=parent.frame()){
           expr <- substitute(...)
           if(class(expr)=="{"){
               expressions <- as.list(expr[-1])
               self$evaluate(expressions,enclos=enclos)
           }
           else
               self$eval(expr,envir=envir,enclos=enclos)
       },
       #' @description
       #' Evaluate a single expression
       #' @param expr A single expression.
       #' @param envir A list or an environment
       #' @param enclos An enclosing an environment (see \code{\link{eval}}).
       eval = function(expr,envir=list(),enclos=parent.frame())
           self$evaluate(list(expr),enclos=enclos),
       #' @description
       #' Evaluate a single expression
       #' @param expressions A list of expressions.
       #' @param envir A list or an environment
       #' @param enclos An enclosing an environment (see \code{\link{eval}}).
       evaluate = function(expressions,envir=list(),enclos=parent.frame()){
           if(is.null(envir))
               envir <- private$envir
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
                       error=private$eHandler,
                       warning=private$wHandler,
                       message=private$mHandler),silent=TRUE)
               self$last.value <- ev
               private$handle_text()
               private$handle_graphics()
               if(is.function(private$value_callback)){
                   try(withCallingHandlers(
                           private$value_callback(ev$value,ev$visible),
                           error=private$eHandler,
                           warning=private$wHandler,
                           message=private$mHandler),silent=TRUE)
                   if(ev$visible){
                       private$handle_text()
                       private$handle_graphics()
                   }
               }
           }
           self$exit()
       },
      last.value = NULL,
      #' @description A function that is called before a set of expressions
      #'     is evaluated (e.g. in a notebook cell).
      #' @param enclos An enclosing environment.
      enter = function(enclos=parent.frame()){

          sink(private$connection,split=FALSE)
          private$orig.device <- options(device=private$device)
          if(private$dev_num == 0 || !(private$dev_num %in% dev.list()))
              private$device()
          if(private$dev_num > 1 && private$dev_num %in% dev.list() && dev.cur() != private$dev_num){
              private$orig.dev_num <- dev.cur()
              dev.set(private$dev_num)
          }

          private$saved_hooks$plot.new            <- getHook('plot.new')
          private$saved_hooks$grid.newpage        <- getHook('grid.newpage')
          private$saved_hooks$before.plot.new     <- getHook('before.plot.new')
          private$saved_hooks$before.grid.newpage <- getHook('before.grid.newpage')
          setHook('plot.new',private$plot_new_hook)
          setHook('grid.newpage',private$grid_newpage_hook)
          setHook('before.plot.new',private$before_plot_new_hook)
          setHook('before.grid.newpage',private$before_plot_new_hook)

          if(!isTRUE(getOption("rkernel_no_output_hooks"))){
              suppressMessages(trace(print,
                                     private$print_hook,
                                     exit=private$print_exit_hook,
                                     print=FALSE))
              suppressMessages(trace(cat,
                                     private$cat_hook,
                                     exit=private$cat_exit_hook,
                                     print=FALSE))
              suppressMessages(trace(str,
                                     private$str_hook,
                                     exit=private$str_exit_hook,
                                     print=FALSE))
          }

          attach(private$attachment,name=private$name,
                 warn.conflicts=FALSE)
          # log_out(sprintf("Attached %s",private$name))
          
          private$run_enter_hooks()

      },

      #' @description A function that is called after a set of expressions
      #'     is evaluated (e.g. in a notebook cell).
      exit = function(){
          
          private$run_exit_hooks()
          # log_out(search(),use.print=TRUE)
          if(private$name %in% search()){
              # log_out(sprintf("Detaching %s",private$name))
              context_pos <- match(private$name,search())
              detach(pos=context_pos)
          }
          sink()
          options(device=private$orig.device)
          if(private$orig.dev_num > 1 && private$orig.dev_num %in% dev.list()) dev.set(private$orig.dev_num)

          setHook('plot.new',           private$saved_hooks$plot.new           ,"replace")
          setHook('grid.newpage',       private$saved_hooks$grid.newpage       ,"replace")
          setHook('before.plot.new',    private$saved_hooks$before.plot.new    ,"replace")
          setHook('before.grid.newpage',private$saved_hooks$before.grid.newpage,"replace")

          if(!isTRUE(getOption("rkernel_no_output_hooks"))){
              suppressMessages(untrace(cat))
              suppressMessages(untrace(str))
              suppressMessages(untrace(print))
          }
      },

      clear_graphics = function(){
          private$current_plot <- NULL
      },
      clear_text = function(){
          private$text_output <- NULL
      },


      #' @description
      #' Add or remove a handler function to be called by the
      #' \code{enter()} function, i.e. before a series of expression 
      #' is evaluated.
      #' @param handler A handler function
      #' @param remove A logical value, whether the handler should be removed or added
      on_enter = function(handler,remove=FALSE){
          if(!length(private$enter_hooks))
              private$enter_hooks <- CallbackDispatcher()
          private$enter_hooks$register(handler,remove)
      },

      #' @description
      #' Add or remove a handler function to be called by the
      #' \code{exit()} function, i.e. after a series of expression 
      #' has been evaluated.
      #' @param handler A handler function
      #' @param remove A logical value, whether the handler should be removed or added
      on_exit = function(handler,remove=FALSE){
          if(!length(private$exit_hooks))
              private$exit_hooks <- CallbackDispatcher()
          private$exit_hooks$register(handler,remove)
      }
   ),
   private = list(

       connection = NULL,
       # A function to be called when text ouput is captured or NULL. Should
       #    be either specified by the constuctor or by inheriting classes.
       text_callback = NULL,
       #  A function to be called when a message is captured or NULL. Should
       #    be either specified by the constuctor or by inheriting classes.
       message_callback = NULL,
       #  A function to be called when a warning is captured or NULL. Should
       #    be either specified by the constuctor or by inheriting classes.
       warning_callback = NULL,
       #  A function to be called when a error is captured or NULL. Should
       #    be either specified by the constuctor or by inheriting classes.
       error_callback = NULL,
       # A function to be called when graphics output is captured or NULL. Should
       #    be either specified by the constuctor or by inheriting classes.
       graphics_callback = NULL,
       # A function or NULL. The function is called when expressions 
       #   evaluation returns a visble object. Should
       #   be either specified by the constuctor or by inheriting classes.
       value_callback = NULL,
       
       text_output = NULL,
       prev_text_output = NULL,
       envir = NULL,
       attachment = NULL,
       id = character(0),
       name = character(0),

       handle_text = function(){
           if(!is.function(private$text_callback)) return()
           # if(!isIncomplete(private$connection) || TRUE)
           cat("\n",file=private$connection)
           private$prev_text_output <- private$text_output
           private$text_output <- textConnectionValue(private$connection)
           if(is.function(private$text_callback)){
               nlines <- length(private$prev_text_output)
               if(nlines > 0)
                   current_text_output <- tail(private$text_output,-nlines)
               else
                   current_text_output <- private$text_output
               if(length(current_text_output)){
                   current_text_output <- paste(current_text_output,collapse="\n")
                   # KLUDGE Ignore empty lines of output
                   if(!identical(current_text_output,"\n"))
                       private$text_callback(current_text_output)
               }
           }
       },
      graphics_active = function(){
          res <- private$dev_num > 1 && private$dev_num == dev.cur()
          return(res)
      },

      handle_graphics = function(){
          if(!is.function(private$graphics_callback)) return()
          # log_out("Context$handle_graphics()")
          private$last_plot <- private$current_plot
          if(private$graphics_active() && par("page")){
              plt <- recordPlot()
              if(plot_has_changed(current=plt,last=private$last_plot)) {
                  update <- !private$plot_new_called
                  private$graphics_callback(plt,update=update)
                  private$current_plot <- plt
                  private$plot_new_called <- FALSE
              } # else log_out("Not sending plot - no changes detected")
          } # else log_out("graphics not active ...")
      },

      plot_new_called = FALSE,
      graphics_par_usr = numeric(0),

      before_plot_new_hook = function(...){
          if(private$graphics_active() && par("page")){
              private$handle_text()
          }
      },

      plot_new_hook = function(...){
          # log_out("plot_new_hook")
          # log_out("private$dev_num==",private$dev_num)
          # log_out("dev.cur()==",dev.cur())
          if(private$graphics_active()){
              private$plot_new_called <- TRUE
              private$graphics_par_usr <- par("usr")
          } #else log_out("graphics not active ...")
      },


      grid_newpage_hook = function(...){
          # log_out("grid_newpage_hook")
          if(private$graphics_active()){
              private$plot_new_called <- TRUE
          } 
      },

      saved_hooks = list(),

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
          private$dev_filename <- switch(os,
                                      windows="NUL",
                                      osx=NULL,
                                      unix="/dev/null")
          private$dev_name <- switch(os,
                                  windows="png",
                                  osx="pdf",
                                  unix="png")
          private$device <- function(filename = NULL,
                                 width = getOption("jupyter.plot.width",6),
                                 height = getOption("jupyter.plot.height",6),
                                 res = getOption("jupyter.plot.res",96),
                                 pointsize = getOption("jupyter.plot.pointsize",12),
                                 units = getOption("jupyter.plot.units","in"),
                                 ...){
              dev <- get(private$dev_name)
              if(is.null(filename))
                  dev(filename=private$dev_filename,
                      width=width,
                      height=height,
                      res=res,
                      units=units,
                      ...)
              private$dev_num <- dev.cur()
              dev.control(displaylist="enable")
              
              # log_out('private$device')
              # log_out("private$dev_num==",private$dev_num)

          }
      },

      enter_hooks = list(),
      run_enter_hooks = function(){
          if(inherits(private$enter_hooks,"CallbackDispatcher"))
              private$enter_hooks$run()
      },

      exit_hooks = list(),
      run_exit_hooks = function(){
          if(inherits(private$exit_hooks,"CallbackDispatcher"))
              private$exit_hooks$run()
      },

      cat_hook = function(){
          # log_out("cat_hook")
          private$handle_graphics()
      },
      cat_exit_hook = function(){
          # log_out("cat_exit_hook")
          private$handle_text()
      },

      print_hook = function(){
          private$handle_graphics()
      },
      print_exit_hook = function(){
          private$handle_text()
      },

      str_depth = 0,
      str_hook = function(){
          # log_out("str_hook")
          private$str_depth <- private$str_depth + 1
          if(private$str_depth == 1)
              suppressMessages(untrace(cat))
      },
      str_exit_hook = function(){
          # log_out("str_exit_hook")
          if(private$str_depth == 1){
              private$handle_text()
              suppressMessages(trace(cat,
                                     private$cat_hook,
                                     exit=private$cat_exit_hook,
                                     print=FALSE))
          }
          private$str_depth <- private$str_depth - 1
      },

      orig.device = NULL,
      orig.dev_num = 1,


       mHandler = function(m) {
           private$message_callback(m)
           invokeRestart("muffleMessage")
       },
       wHandler = function(w){
           private$warning_callback(w)
           if (getOption("warn") >= 2) return()
           invokeRestart("muffleWarning")
       },
       eHandler = function(e) {
           private$error_callback(e)
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

plot_calls <- function(plt){
    plt <- plt[[1]]
    plt <- lapply(plt,"[[",2)
    if(!length(plt)) return(NULL)
    lapply(plt,"[[",1)
}

non_empty_plot_calls <- function(plt){
    if(!length(plt)) return(NULL)
    pcalls <- plot_calls(plt)
    pcnames <- sapply(pcalls,get_name_el)
    empty <- pcnames %in% empty_plot_calls
    pcalls[!empty]
}

get_name_el <- function(x){
    if(length(x$name)) x$name 
    else deparse(x)
}

is_base_graphics <- function(plt){
    plt1 <- plt[[1]][[1]][[2]][[1]]
    get_name_el(plt1) == "C_plot_new"
}

compare_plots <- function(plt1,plt2){
    if((length(plt1) > 0) != (length(plt2) > 0)) return(FALSE)
    if(is_base_graphics(plt1) != is_base_graphics(plt2)) return(FALSE)
    if(!is_base_graphics(plt1)) return(identical(plt1[[3]],plt2[[3]]))
    else {
        ne1 <- non_empty_plot_calls(plt1)
        ne2 <- non_empty_plot_calls(plt2)
        if(!identical(ne1,ne2)) return(FALSE)
        else {
            log_out(ne1)
            return(identical(plt1[[2]],plt2[[2]]))
        }
    }
}

plot_has_changed <- function(current,last){
    if(!length(current) || !length(current)[[1]]) {
        # log_out("Current plot is NULL")
        return(FALSE)
    }
    ne1 <- non_empty_plot_calls(current)
    if(!length(ne1)) {
        # log_out("Current plot is empty")
        return(FALSE)
    }
    if(is_base_graphics(current)){
        if(!length(last)) return(TRUE)
        else if(!is_base_graphics(last)) return(TRUE)
        else {
            ne2 <- non_empty_plot_calls(last)
            if(!identical(ne1,ne2)) return(TRUE)
            else {
                # log_out(digest::sha1(last[[2]]))
                # log_out(digest::sha1(current[[2]]))
                return(!identical(current[[2]],last[[2]]))
                # return(!identical(current[1:2],last[1:2]))
                # return(!identical(current[2],last[2]))
                # return(!identical(current,last))
            }
        }
    }
    else {
        if(is_base_graphics(last)) return(TRUE)
        else if(!length(last)) return(TRUE)
        else return(identical(current[[3]],last[[3]])) 
    }
}

#' Evalute expressions within a context
#'
#' @description A method for the generic function "with" to be used with context
#'     objects.
#'
#' @param data A Context object
#' @param expr An expression (or a set of expression wrapped in curly braces).
#' @param enclos An enclosing environment.
#' @export
with.Context <- function(data,expr,enclos=parent.frame(),...){
    # log_out("with.Context")
    # log_out(enclos,use.print=TRUE)
    # log_out(ls(enclos),use.print=TRUE)
    data$eval(substitute(expr),enclos=enclos)
}
