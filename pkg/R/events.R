EventManagerClass <- R6Class("EventManager",
 public = list(
     initialize = function(type){
         private$type = type
     },
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
     clear = function(event){
         if(private$active && length(event) && event %in% names(private$dispatchers)){
             ed <- private$dispatchers[[event]]
             if(inherits(ed,"CallbackDispatcher"))
                 ed$clear()
         }
     },
     resume = function(){
         private$active <- TRUE
     },
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

EventManager <- function(...) EventManagerClass$new(...)

eventmanagers <- new.env()

installed_hooks <- new.env()
