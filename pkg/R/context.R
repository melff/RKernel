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
           for(expr in expressions){
               # i <- i + 1
               # log_out(sprintf("exressions[[%d]]",i))
               # expr <- expressions[[i]]
               # ev <- list(value = NULL, visible = FALSE)
               ### See 'evaluate_call' in package "evaluate" (Yihui Xie et al.)
               # dev.control(displaylist ="enable")
               ev <- withVisible(try(withCallingHandlers(
                               eval(expr,
                                    envir=envir,
                                    enclos=enclos),
                               error=private$eHandler,
                               warning=private$wHandler,
                               message=private$mHandler),silent=TRUE))
               self$last.value <- ev
               cat("\n",file=private$connection)
               private$handle_event("eval")
               try(withCallingHandlers(
                   private$handle_event("result",ev$value,ev$visible),
                   error=private$eHandler,
                   warning=private$wHandler,
                   message=private$mHandler),silent=TRUE)
           }
           self$exit()
       },
       last.value = NULL,
       current_plot = NULL,
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
           

           private$handle_event("enter")
           sink(private$connection,split=FALSE)

       },

       #' @description A function that is called after a set of expressions
       #'     is evaluated (e.g. in a notebook cell).
       exit = function(){
           
           # log_out("context$exit")
           private$handle_event("exit")
           # log_out(search(),use.print=TRUE)
           if(private$name %in% search()){
              # log_out(sprintf("Detaching %s",private$name))
               context_pos <- match(private$name,search())
               detach(pos=context_pos)
           }

           sink()

       },

       #' @description
       #' Add or remove a handler function to be called by the
       #' \code{enter()} function, i.e. before a series of expression 
       #' is evaluated.
       #' @param handler A handler function
       #' @param remove A logical value, whether the handler should be removed or added
       on_enter = function(handler,remove=FALSE){
           if(is.function(handler)){
               private$on_event("enter",handler=handler,remove=remove)
           }
       },

       #' @description
       #' Add or remove a handler function to be called by the
       #' \code{exit()} function, i.e. after a series of expression 
       #' has been evaluated.
       #' @param handler A handler function
       #' @param remove A logical value, whether the handler should be removed or added
       on_exit = function(handler,remove=FALSE){
           if(is.function(handler)){
               private$on_event("exit",handler=handler,remove=remove)
           }
       },

       on_eval = function(handler,remove=FALSE){
           if(is.function(handler)){
               private$on_event("eval",handler=handler,remove=remove)
           }
       },

       on_result = function(handler,remove=FALSE){
           if(is.function(handler)){
               private$on_event("result",handler=handler,remove=remove)
           }
       },

       on_print = function(handler=NULL,exit=NULL,remove=FALSE){
           if(is.function(handler)){
               private$on_event("print",handler=handler,remove=remove)
           }
           if(is.function(exit)){
               private$on_event("print_exit",handler=exit,remove=remove)
           }
       },

       on_cat = function(handler=NULL,exit=NULL,remove=FALSE){
           if(is.function(handler)){
               private$on_event("cat",handler=handler,remove=remove)
           }
           if(is.function(exit)){
               private$on_event("cat_exit",handler=exit,remove=remove)
           }
       },

       on_str = function(handler=NULL,exit=NULL,remove=FALSE){
           if(is.function(handler)){
               private$on_event("str",handler=handler,remove=remove)
           }
           if(is.function(exit)){
               private$on_event("str_exit",handler=exit,remove=remove)
           }
       },

       on_error = function(handler=NULL,remove=FALSE){
           private$on_event("error",handler=handler,remove=remove)
       },

       on_warning = function(handler=NULL,remove=FALSE){
           private$on_event("warning",handler=handler,remove=remove)
       },

       on_message = function(handler=NULL,remove=FALSE){
           private$on_event("message",handler=handler,remove=remove)
       },
       
       get_text = function(){
           # log_out("get_text")
           cat("\n",file=private$connection)
           private$prev_text_output <- private$text_output
           private$text_output <- textConnectionValue(private$connection)
           # log_out(sprintf("text_output = %s\n",paste(private$text_output,collapse="\n")))
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

       get_graphics = function(always = FALSE){
           # log_out("get_graphics")
           # log_out("dev.cur()==",dev.cur())
           # dev.control(displaylist ="enable")
           if(!graphics$current$is_active()) {
               return(NULL)
           }
           else if(par("page")) {
               plt <- recordPlot()
               new_page <- graphics$current$new_page(reset=TRUE)
               private$last_plot <- self$current_plot
               if(always || new_page || plot_has_changed(current=plt,last=private$last_plot)) {
               # if(!plot_is_empty(plt)){
                   self$current_plot <- plt
                   return(structure(plt,new_page=new_page))
               } else return(NULL)
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

       on_event = function(type,handler=NULL,remove=FALSE){
           if(is.function(handler)){
               if(!length(private$handlers[[type]]))
                   private$handlers[[type]] <- CallbackDispatcher()
               prh <- private$handlers[[type]]
               prh$register(handler,remove=remove)
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

       handle_event = function(type,...){
           # log_out(sprintf("Context: Handling event type \"%s\"",type))
           if(length(private$handlers[[type]])){
               prh <- private$handlers[[type]]
               if(inherits(prh,"CallbackDispatcher"))
                   prh$run(...)
           }
       },
       
      
       mHandler = function(m) {
           # log_out(m,use.print=TRUE)
           private$handle_event("message",m)
           # textio_hooks$message$run()
           invokeRestart("muffleMessage")
       },
       wHandler = function(w){
           warning_text <- conditionMessage(w)
           warning_text <- paste(warning_text,collapse="\n")
           log_warning(warning_text)
           # log_out(w,use.print=TRUE)
           # textio_hooks$warning$run()
           private$handle_event("warning",w)
           if (getOption("warn") >= 2) return()
           invokeRestart("muffleWarning")
       },
       eHandler = function(e) {
           # textio_hooks$error$run()
           error_text <- conditionMessage(e)
           error_text <- paste(error_text,collapse="\n")
           log_error(error_text)
           private$handle_event("error",e)
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


