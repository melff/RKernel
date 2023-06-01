#' A Manager for Comms
#'
#' @description Objects of this class are used internally to manage events, they
#'     are not meant to be used by end-users.
#' @name EventManager
NULL

#' @rdname EventManager
#' @export
EventManagerClass <- R6Class("EventManager",
 public = list(
     #' @description 
     #' Initialize an event manager
     #' @param type A string, the type of event (e.g. "print")
     initialize = function(type){
         private$type = type
     },
     #' @description Send an event
     #' @param event A string, the name of an event
     #' @param ... Other arguments, sent to the event handler(s)
     send = function(event,...){
         # log_out(sprintf("eventmanager$send(\"%s\",...)",event))
         # log_out(private$dispatchers,use.str=TRUE)
         if(private$active && event %in% names(private$dispatchers)){
             ed <- private$dispatchers[[event]]
             # log_out(ed,use.print=TRUE)
             if(inherits(ed,"CallbackDispatcher")){
                 res <- ed$run(...)
                 if(length(res))
                     return(res)
             }
         }
      },
     #' @description Install a handler for an event
     #' @param event A string, the name of an event
     #' @param handler A function 
     #' @param remove A logical value, whether the handler is to be removed
     on = function(event,handler,remove=FALSE){
         # log_out(sprintf("eventmanager$on(\"%s\",...)",event))
         ed <- private$dispatchers[[event]]
         if(!inherits(ed,"CallbackDispatcher")){
             # log_out(sprintf("Creating dispatcher for %s",event))
             ed <- CallbackDispatcher()
             private$dispatchers[[event]] <- ed
         }
         #if(!remove)
             # log_out(sprintf("Adding handler for %s",event))
         ed$register(handler=handler,remove=remove)
     },
     #' @description Activate handlers
     #' @param event A string, the name of an event, ignored if 'all' is TRUE.
     #' @param all A logical value, if TRUE, all handlers that belong to 
     #'   the event type of the event manager are activated.
     activate = function(event=NULL,all=TRUE){
         if(all){
             if(!private$active){
                 # log_out(sprintf("Activating event manager of type '%s",private$type))
                 private$active <- TRUE
                 if(length(eventmanagers[[private$type]]) &&
                    !identical(eventmanagers[[private$type]],self)){
                     eventmanagers[[private$type]]$suspend()
                     private$saved <- eventmanagers[[private$type]]
                 }
                 eventmanagers[[private$type]] <- self
             }
         }
         else if(length(event) && private$active){
             if(event %in% names(private$dispatchers)) {
                 ed <- private$dispatchers[[event]]
                 if(inherits(ed,"CallbackDispatcher"))
                     ed$activate_handlers()
             }
         }
     },
     #' @description Suspend handlers
     #' @param event A string, the name of an event, ignored if 'all' is TRUE.
     #' @param all A logical value, if TRUE, all handlers that belong to 
     #'   the event type of the event manager are suspended.
     suspend = function(event=NULL,all=TRUE){
         if(all && private$active){
             # log_out(sprintf("Activating event manager of type '%s",private$type))
             private$active <- FALSE
             if(length(private$saved)){
                 eventmanagers[[private$type]] <- private$saved
                 private$saved <- NULL
                 eventmanagers[[private$type]]$resume()
             }
         }
         else if(private$active && length(event) && event %in% names(private$dispatchers)){
             ed <- private$dispatchers[[event]]
             if(inherits(ed,"CallbackDispatcher"))
                 ed$suspend_handlers()
         }
     },
     #' @description Clear (i.e. remove) handlers for an event
     #' @param event A string, the name of an event
     clear = function(event){
         if(private$active && length(event) && event %in% names(private$dispatchers)){
             ed <- private$dispatchers[[event]]
             if(inherits(ed,"CallbackDispatcher"))
                 ed$clear()
         }
     },
     #' @description Resume (i.e. reactivate) an event handler
     resume = function(){
         private$active <- TRUE
     },
     #' @description Check whether the event manager has handlers for the
     #'   given events
     #' @param events A character vector with names of events
     has = function(events){
         if(length(private$dispatchers))
             all(events %in% names(private$dispatchers))
         else FALSE
     }
   ),
   private = list(
       dispatchers = list(),
       active = FALSE,
       saved = NULL,
       type = NULL
 )
)

#' @describeIn EventManager The constructor function, returns an Object of Class "EventManagerClass" 
#' @param ... Arguments passed to the inializer
#' @export
EventManager <- function(...) EventManagerClass$new(...)

eventmanagers <- new.env()

installed_hooks <- new.env()
