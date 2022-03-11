#' A Dispatcher for Callbacks
#'
#' @description Objects in this class are collections of callbacks functions
#'     usually related to certain events
#'
#' @export
CallbackDispatcherClass <- R6Class("CallbackDispatcher",
    public = list(
      #' @field callbacks List of callback functions
      callbacks = list(),
      #' @description
      #' Register a function as call back
      #' @param handler A function
      #' @param remove A logical value; whether the function
      #'   is added or removed from the list of callbacks
      register = function(handler,remove){
        old <- self$callbacks
        new <- list()
        for(cb in old){
          if(!identical(cb,handler))
            new <- append(new,cb)
        }
        if(!remove){
          new <- append(new,handler)
        }
        self$callbacks <- new
      },
      #' @description
      #' Run all registered callback functions
      #' @param ... Aruments passed on to the handler functions
      run = function(...){
        # log_out(self$callbacks,use.print=TRUE)
        for(cb in self$callbacks){
            if(is.function(cb))cb(...)
        }
      }
    )
)

#' @rdname CallbackDispatcherClass 
#' @export
CallbackDispatcher <- function(...) CallbackDispatcherClass$new(...)
