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
       #' @param envir An optional environment within which expressions are evaluated
       #' @param attachment An optional environment that is attached while an expression is evaluated within 
       #'   the context.
       initialize = function(envir=list(),
                             attachment=list()){
           private$connection <- textConnection(NULL,"w",local=TRUE)
           private$envir <- envir
           private$attachment <- attachment
           private$id <- UUIDgenerate()
           private$name <- paste0("RKernel-context:",private$id)
           em <- EventManager(type="conditions")
           private$condition_manager <- em
           em$activate()
           em <- EventManager(type="output")
           private$output_manager <- em
           em$activate()
       },

       #' @description
       #' Evaluate one or several expressions
       #' @param ... A single expression or several expressions
       #'   included in curly braces.
       #' @param envir A list or an environment or NULL
       #' @param enclos An enclosing an environment (see \code{\link{eval}}).
       do = function(...,envir=NULL,enclos=parent.frame()){
           expr <- substitute(...)
           if(class(expr)=="{"){
               expressions <- as.list(expr[-1])
               self$evaluate(expressions,envir=envir,enclos=enclos)
           }
           else
               self$eval(expr,envir=envir,enclos=enclos)
       },
       #' @description
       #' Evaluate a single expression
       #' @param expr A single expression.
       #' @param envir A list or an environment or NULL
       #' @param enclos An enclosing an environment (see \code{\link{eval}}).
       eval = function(expr,envir=NULL,enclos=parent.frame())
           self$evaluate(list(expr),envir=envir,enclos=enclos),
       #' @description
       #' Evaluate a single expression
       #' @param expressions A list of expressions.
       #' @param envir A list or an environment or NULL
       #' @param enclos An enclosing an environment (see \code{\link{eval}}).
       evaluate = function(expressions,envir=NULL,enclos=parent.frame()){
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
               ev <- withVisible(
                       try(withRestarts(
                           withCallingHandlers(
                               eval(expr,
                                    envir=envir,
                                    enclos=enclos),
                               error=private$eHandler,
                               warning=private$wHandler,
                               message=private$mHandler),
                           exit=noop),
                          silent=TRUE))
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
       #' @field last.value
       #' Value of last evaluated expression.
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
           
           if(private$output_manager$has(c("print","cat"))){
               private$output_manager$activate()
           }
           if(private$condition_manager$has("error")){
               private$condition_manager$activate()
           }
           private$handle_event("enter")
           sink(private$connection,split=FALSE)

       },

       #' @description A function that is called after a set of expressions
       #'     is evaluated (e.g. in a notebook cell).
       exit = function(){
           
           if(private$output_manager$has(c("print","cat"))){
               private$output_manager$suspend()
           }
           if(private$condition_manager$has("error")){
               private$condition_manager$suspend()
           }
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
       #' @description
       #' Add or remove a handler function to be called after every time an expression 
       #' has been evaluated.
       #' @param handler A handler function
       #' @param remove A logical value, whether the handler should be removed or added
       on_eval = function(handler,remove=FALSE){
           if(is.function(handler)){
               private$on_event("eval",handler=handler,remove=remove)
           }
       },
       #' @description
       #' Add or remove a handler function to the results of each evaluated 
       #' expression.
       #' @param handler A handler function. This function must take two arguments,
       #'   the value resulting from the evalualition of the expression
       #'   and a logical value, which indicates whether the value is visible.
       #'   See also \code{\link[base]{withVisible}}.
       #' @param remove A logical value, whether the handler should be removed or added
       on_result = function(handler,remove=FALSE){
           if(is.function(handler)){
               private$on_event("result",handler=handler,remove=remove)
           }
       },
       #' @description
       #' Add or remove a handler function to be called if an error condition is 
       #' raised, e.g. by \code{stop()}.
       #' @param handler A handler function
       #' @param remove A logical value, whether the handler should be removed or added
       on_error = function(handler=NULL,remove=FALSE){
           private$condition_manager$on("error",handler=handler,remove=remove)
       },
       #' @description
       #' Add or remove a handler function to be called if an warning is 
       #' raised, e.g. by \code{stop()}.
       #' @param handler A handler function
       #' @param remove A logical value, whether the handler should be removed or added
       on_warning = function(handler=NULL,remove=FALSE){
           private$condition_manager$on("warning",handler=handler,remove=remove)
       },
       #' @description
       #' Add or remove a handler function to be called if a message is 
       #' shown.
       #' @param handler A handler function
       #' @param remove A logical value, whether the handler should be removed or added
       on_message = function(handler=NULL,remove=FALSE){
           private$condition_manager$on("message",handler=handler,remove=remove)
       },
       #' @description
       #' Add or remove a handler function to be called if an object is
       #' output \code{print()}.
       #' @param handler A handler function to called *before* \code{print()}
       #' @param exit A handler function to called *after* \code{print()}
       #' @param remove A logical value, whether the handler should be removed or added
       on_print = function(handler=NULL,exit=NULL,remove=FALSE){
           if(is.function(handler)){
               private$output_manager$on("before_print",handler=handler,remove=remove)
           }
           if(is.function(exit)){
               private$output_manager$on("print",handler=exit,remove=remove)
           }
       },
       #' @description
       #' Add or remove a handler function to be called if text is output using
       #' \code{cat()}.
       #' @param handler A handler function to called *before* \code{cat()}
       #' @param exit A handler function to called *after* \code{cat()}
       #' @param remove A logical value, whether the handler should be removed or added
       on_cat = function(handler=NULL,exit=NULL,remove=FALSE){
           if(is.function(handler)){
               private$output_manager$on("before_cat",handler=handler,remove=remove)
           }
           if(is.function(exit)){
               private$output_manager$on("cat",handler=exit,remove=remove)
           }
       },
       #' @description
       #' Add or remove a handler function to be called if the structure of an object
       #' is shown using \code{str()}.
       #' @param handler A handler function to called *before* \code{str()}
       #' @param exit A handler function to called *after* \code{str()}
       #' @param remove A logical value, whether the handler should be removed or added
       on_str = function(handler=NULL,exit=NULL,remove=FALSE){
           if(is.function(handler)){
               private$output_manager$on("before_str",handler=handler,remove=remove)
           }
           if(is.function(exit)){
               private$output_manager$on("str",handler=exit,remove=remove)
           }
       },
       #' @description
       #' Get text output produced by the expressions last evaluated.
       get_text = function(){
           # log_out("get_text")
           cat("\n",file=private$connection)
           private$prev_text_output <- private$text_output
           # log_out(sprintf("prev_text_output = %s\n",paste(sQuote(private$prev_text_output),collapse=",")))
           private$text_output <- textConnectionValue(private$connection)
           # log_out(sprintf("text_output = %s\n",paste(sQuote(private$text_output),collapse=",")))
           nlines <- length(private$prev_text_output)
           if(nlines > 0)
               current_text_output <- tail(private$text_output,-nlines)
           else
               current_text_output <- private$text_output
           if(length(current_text_output)){
               current_text_output <- paste(current_text_output,collapse="\n")
              # Ignore empty lines of output
               if(!identical(current_text_output,"\n"))
                   return(current_text_output)
               else return("")
           } else return("")
       }
   ),
   private = list(

       condition_manager = NULL,
       output_manager = NULL,
       
       handlers = list(),
       saved_handlers = list(),

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

       handle_event = function(type,...){
           # log_out(sprintf("Context: Handling event type \"%s\"",type))
           if(length(private$handlers[[type]])){
               prh <- private$handlers[[type]]
               if(inherits(prh,"CallbackDispatcher"))
                   prh$run(...)
           }
       },

       mHandler = function(m) {
           message_text <- conditionMessage(m)
           message_text <- paste(message_text,collapse="\n")
           # log_out(message_text)
           # log_out(m,use.print=TRUE)
           private$condition_manager$send("message",m)
           # textio_hooks$message$run()
           invokeRestart("muffleMessage")
       },
       wHandler = function(w){
           warning_text <- conditionMessage(w)
           warning_text <- paste(warning_text,collapse="\n")
           # log_warning(warning_text)
           # log_out(w,use.print=TRUE)
           # textio_hooks$warning$run()
           private$condition_manager$send("warning",w)
           if (getOption("warn") >= 2) return()
           invokeRestart("muffleWarning")
       },
       eHandler = function(e) {
           # textio_hooks$error$run()
           error_text <- conditionMessage(e)
           error_text <- paste(error_text,collapse="\n")
           # log_error(error_text)
           private$condition_manager$send("error",e)
           errOpt <- getOption("error",NULL)
           # log_out(errOpt,use.print=TRUE)
           # log_out(errOpt,use.str=TRUE)
           # log_out(mode(errOpt))
           if(is.call(errOpt))
               eval(errOpt)
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
#' @param ... Other arguments, ignored.
#' @export
with.Context <- function(data,expr,enclos=parent.frame(),...){
    # log_out("with.Context")
    # log_out(enclos,use.print=TRUE)
    # log_out(ls(enclos),use.print=TRUE)
    data$eval(substitute(expr),enclos=enclos)
}

noop <- function(...) {
    return(invisible(NULL))
}
