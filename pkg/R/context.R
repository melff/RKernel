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
       initialize = function(envir=new.env(),
                             attachment=new.env()){
           private$connection <- textConnection(NULL,"w",local=TRUE)
           private$envir <- envir
           private$attachment <- attachment
           private$id <- UUIDgenerate()
           private$name <- paste0("RKernel-context:",private$id)
           private$evaluator <- get_evaluator()
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
           i <- 0
           sink(private$connection,split=FALSE)
           for(expr in expressions){
               i <- i + 1
               # log_out(sprintf("exressions[[%d]]",i))
               # expr <- expressions[[i]]
               # ev <- list(value = NULL, visible = FALSE)
               ### See 'evaluate_call' in package "evaluate" (Yihui Xie et al.)
               ev <- withCallingHandlers(
                       withVisible(eval(expr,
                                        envir=envir,
                                        enclos=enclos)),
                       error=private$eHandler,
                       warning=private$wHandler,
                       message=private$mHandler)
               self$last.value <- ev
               cat("\n",file=private$connection)
               private$run_eval_hooks()
               try(withCallingHandlers(
                   private$run_result_hooks(ev$value,ev$visible),
                   error=private$eHandler,
                   warning=private$wHandler,
                   message=private$mHandler),silent=TRUE)
           }
           sink()
          self$exit()
       },
       last.value = NULL,
       #' @description A function that is called before a set of expressions
       #'     is evaluated (e.g. in a notebook cell).
       #' @param enclos An enclosing environment.
       enter = function(enclos=parent.frame()){

           # log_out("context$enter")
           # log_out(sprintf("Open syncs: %d",sink.number()))

           #sink(private$connection,split=FALSE)
           # sink("RKernel.out",split=FALSE)

           attach(private$attachment,name=private$name,
                  warn.conflicts=FALSE)
           # log_out(sprintf("Attached %s",private$name))
           

           private$run_enter_hooks()

       },

       #' @description A function that is called after a set of expressions
       #'     is evaluated (e.g. in a notebook cell).
       exit = function(){
           
           log_out("context$exit")
           private$run_exit_hooks()
           # log_out(search(),use.print=TRUE)
           if(private$name %in% search()){
              # log_out(sprintf("Detaching %s",private$name))
               context_pos <- match(private$name,search())
               detach(pos=context_pos)
           }


           # sink()

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
       },

       on_eval = function(handler,remove=FALSE){
           if(!length(private$eval_hooks))
               private$eval_hooks <- CallbackDispatcher()
           private$eval_hooks$register(handler,remove)
       },

       on_result = function(handler,remove=FALSE){
           if(!length(private$result_hooks))
               private$result_hooks <- CallbackDispatcher()
           private$result_hooks$register(handler,remove)
       },

       on_print = function(handler=NULL,exit=NULL,remove=FALSE){
           if(is.function(handler)){
               prh <- private$handlers[["print"]]
               prh$register(handler,remove=remove)
           }
           if(is.function(exit)){
               prh <- private$handlers[["print_exit"]]
               prh$register(handler,remove=remove)
           }
       },

       on_cat = function(handler=NULL,exit=NULL,remove=FALSE){
           if(is.function(handler)){
               prh <- private$handlers[["cat"]]
               prh$register(handler,remove=remove)
           }
           if(is.function(exit)){
               prh <- private$handlers[["cat_exit"]]
               prh$register(handler,remove=remove)
           }
       },


       on_str = function(handler=NULL,exit=NULL,remove=FALSE){
           if(is.function(handler)){
               prh <- private$handlers[["str"]]
               prh$register(handler,remove=remove)
           }
           if(is.function(exit)){
               prh <- private$handlers[["str_exit"]]
               prh$register(handler,remove=remove)
           }
       },

       get_text = function(){
           private$prev_text_output <- private$text_output
           private$text_output <- textConnectionValue(private$connection)
           nlines <- length(private$prev_text_output)
           if(nlines > 0)
               current_text_output <- tail(private$text_output,-nlines)
           else
               current_text_output <- private$text_output
           if(length(current_text_output)){
               current_text_output <- paste(current_text_output,collapse="\n")
              # KLUDGE Ignore empty lines of output
               if(!identical(current_text_output,"\n"))
                   return(current_text_output)
               else return("")
           } else return("")
       },

       get_graphics = function(){
           private$last_plot <- private$current_plot
           plt <- recordPlot()
           if(plot_has_changed(current=plt,last=private$last_plot)) {
          # if(!plot_is_empty(plt)){
               private$current_plot <- plt
               return(plt)
           } else return(NULL)
       }


   ),
   private = list(

       handlers = list(),
       saved_handlers = list(),

       init_hooks = function(){
           nms <- ls(envir=textio_hooks)
           for(nm in nms)
               private$handlers[[nm]] <- CallbackDispatcher()
       },
       
       install_hooks = function(){
           nms <- ls(envir=textio_hooks)
           for(nm in nms){
               handlers <- private$handlers[[nm]]$copy_handlers()
               private$saved_handlers[[nm]] <- textio_hooks[[nm]]$shift_handlers(handlers)
           }
       },

       restore_hooks = function(){
           nms <- ls(envir=textio_hooks)
           for(nm in nms){
               handlers <- private$saved_handlers[[nm]]
               textio_hooks[[nm]]$shift_handlers(handlers)
           }
       },


       connection = NULL,
       # A function to be called when text ouput is captured or NULL. Should
       #    be either specified by the constuctor or by inheriting classes.
       
       text_output = NULL,
       prev_text_output = NULL,
       envir = NULL,
       attachment = NULL,
       id = character(0),
       name = character(0),

       last_plot = NULL,
       current_plot = NULL,

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

       eval_hooks = list(),
       run_eval_hooks = function(){
           if(inherits(private$eval_hooks,"CallbackDispatcher"))
               private$eval_hooks$run()
       },

       result_hooks = list(),
       run_result_hooks = function(...){
           if(inherits(private$result_hooks,"CallbackDispatcher"))
               private$result_hooks$run(...)
       },

       evaluator = NULL,

       mHandler = function(m) {
           log_out(m,use.print=TRUE)
           # textio_hooks$message$run()
           invokeRestart("muffleMessage")
       },
       wHandler = function(w){
           log_out(w,use.print=TRUE)
           # textio_hooks$warning$run()
           if (getOption("warn") >= 2) return()
           invokeRestart("muffleWarning")
       },
       eHandler = function(e) {
           # textio_hooks$error$run()
           log_out(e,use.print=TRUE)
       }
   )
)

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


textio_hooks <- new.env()
graphics_hooks <- new.env()
orig_funs <- new.env()
init_hooks <- function(){
    textio_hooks$print <- CallbackDispatcher()
    textio_hooks$print_exit <- CallbackDispatcher()
    textio_hooks$cat <- CallbackDispatcher()
    textio_hooks$cat_exit <- CallbackDispatcher()
    textio_hooks$str <- CallbackDispatcher()
    textio_hooks$str_exit <- CallbackDispatcher()
    textio_hooks$error <- CallbackDispatcher()
    textio_hooks$warning <- CallbackDispatcher()
    textio_hooks$message <- CallbackDispatcher()
    graphics_hooks$before_plot_new <- CallbackDispatcher()
    graphics_hooks$plot_new <- CallbackDispatcher()
    orig_funs$print <- print
    orig_funs$cat <- cat
    orig_funs$str <- str
    # suppressMessages(trace(print,
    #                        textio_hooks$print$run,
    #                        exit=textio_hooks$print_exit$run,
    #                        print=FALSE))
    # suppressMessages(trace(cat,
    #                        textio_hooks$cat$run,
    #                        exit=textio_hooks$cat_exit$run,
    #                        print=FALSE))
    # suppressMessages(trace(str,
    #                        textio_hooks$str$run,
    #                        exit=textio_hooks$str_exit$run,
                           # print=FALSE))
}
