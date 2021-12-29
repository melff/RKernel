#' @export
CallbackDispatcherClass <- R6Class("CallbackDispatcher",
    public = list(
      callbacks = list(),
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
      run = function(...){
        # log_out(self$callbacks,use.print=TRUE)
        for(cb in self$callbacks){
            if(is.function(cb))cb(...)
        }
      }
    )
)


#' @export
CallbackDispatcher <- function(...) CallbackDispatcherClass$new(...)
