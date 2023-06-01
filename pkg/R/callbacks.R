#' A Dispatcher for Callbacks
#'
#' @description Objects in this class are collections of callbacks functions
#'     usually related to certain events. The function \code{CallbackDispachter} can be
#'     used as an constructor
#' @name CallbackDispatcher
NULL

#' @rdname CallbackDispatcher
#' @export
CallbackDispatcherClass <- R6Class("CallbackDispatcher",
    public = list(
      #' @description
      #' Register a function as a callback
      #' @param handler A function
      #' @param remove A logical value; whether the function
      #'   is added or removed from the list of callbacks
      register = function(handler,remove){
        old <- private$callbacks
        new <- list()
        for(cb in old){
          if(!identical(cb,handler))
            new <- append(new,cb)
        }
        if(!remove){
          new <- append(new,handler)
        }
        private$callbacks <- new
      },
      #' @description
      #' Remove all callback functions
      clear = function(){
          private$callbacks <- list()
      },
      #' @description
      #' Suspend registered callback functions
      suspend_handlers = function(){
          private$suspended <- TRUE
      },
      #' @description
      #' (Re-)activate registered callback functions
      activate_handlers = function(){
          private$suspended <- FALSE
      },
      #' @description
      #' Run all registered callback functions
      #' @param ... Aruments passed on to the handler functions
      run = function(...){
        # log_out(private$callbacks,use.print=TRUE)
          res <- NULL
          if(!private$suspended){
              for(cbkfun in private$callbacks){
                  if(is.function(cbkfun))
                      res <- cbkfun(...)
              }
          }
          invisible(res)
      }
    ),
    private = list(
        callbacks = list(),
        data = list(),
        suspended = FALSE
    )
)

#' @describeIn CallbackDispatcher The constructor function, returns an Object of Class "CallbackDispatcherClass" 
#' @param ... Arguments passed to the inializer
#' @export
CallbackDispatcher <- function(...) CallbackDispatcherClass$new(...)
