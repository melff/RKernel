EventManagerClass <- R6Class("EventManager",
 public = list(
     send = function(event,...){
         # log_out(sprintf("eventmanager$send(\"%s\",...)",event))
         if(event %in% names(private$dispatchers)){
             edlist <- private$dispatchers[[event]]
             n <- length(edlist)
             if(n > 0){
                 ed <- edlist[[n]]
                 if(inherits(ed,"CallbackDispatcher"))
                     ed$run(...)
             }
         }
      },
     on = function(event,handler,remove=FALSE){
         # log_out(sprintf("eventmanager$on(\"%s\",...)",event))
         self$assure_handlers(event)
         edlist <- private$dispatchers[[event]]
         n <- length(edlist)
         ed <- edlist[[n]]
         if(!inherits(ed,"CallbackDispatcher")){
             # log_out(sprintf("Creating dispatcher for %s",event))
             ed <- CallbackDispatcher()
             edlist[[n]] <- ed
             private$dispatchers[[event]] <- edlist
         }
         #if(!remove)
             # log_out(sprintf("Adding handler for %s",event))
         ed$register(handler=handler,remove=remove)
     },
     suspend = function(event){
         if(event %in% names(private$dispatchers)){
             edlist <- private$dispatchers[[event]]
             n <- length(edlist)
             if(n > 0){
                 ed <- edlist[[n]]
                 if(inherits(ed,"CallbackDispatcher"))
                     ed$suspend_handlers()
             }
         }
     },
     activate = function(event){
         if(event %in% names(private$dispatchers)){
             edlist <- private$dispatchers[[event]]
             n <- length(edlist)
             if(n > 0){
                 ed <- edlist[[n]]
                 if(inherits(ed,"CallbackDispatcher"))
                     ed$activate_handlers()
             }
         }
     },
     init_handlers = function(event){
         private$dispatchers[[event]] <- list(list())
     },
     assure_handlers = function(event){
         if(!(event %in% names(private$dispatchers))){
             self$init_handlers(event)
         }
     },
     push_handlers = function(event){
         if(!(event %in% names(private$dispatchers))){
             self$init_handlers(event)
         }
         else {
             edlist <- private$dispatchers[[event]]
             n <- length(edlist)
             edlist[[n + 1]] <- list()
             private$dispatchers[[event]] <- edlist
         }
     },
     pop_handlers = function(event){
         ed <- NULL
         if(event %in% names(private$dispatchers)){
             edlist <- private$dispatchers[[event]]
             n <- length(edlist)
             if(n > 0) {
                 ed <- edlist[[n]]
                 edlist[[n]] <- NULL
             }
             private$dispatchers[[event]] <- edlist
         }
         return(ed)
     }
   ),
   private = list(
       dispatchers = list()
 )
)

EventManager <- function(...) EventManagerClass$new(...)

eventmanager <- new.env()

get_current_event_manager <- function(){
    if(!inherits(eventmanager$current,"EventManager"))
        eventmanager$current <- EventManager()
    return(eventmanager$current)
}
