#' A Dispatcher for Callbacks
#'
#' @description Objects in this class are collections of callbacks functions
#'     usually related to certain events. The function \code{CallbackDispachter} can be
#'     used as an constructor
#'
#' @export
CallbackDispatcherClass <- R6Class("CallbackDispatcher",
    public = list(
      #' @description
      #' Register a function as call back
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
      clear = function(){
          private$callbacks <- list()
      },
      suspend_handlers = function(){
          private$suspended <- TRUE
      },
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

#' @describeIn CallbackDispatcherClass The constructor function, returns an Object of Class "CallbackDispatcherClass" 
#' @export
CallbackDispatcher <- function(...) CallbackDispatcherClass$new(...)
