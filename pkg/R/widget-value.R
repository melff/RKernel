#' Value widgets
#'
#' @description A base class for widgets that are connected with values
#' @include widget-description.R
#' @export
ValueWidgetClass <- R6Class_("ValueWidget",
  inherit = DescriptionWidgetClass,
  public = list(
      #' @field value A list or any other vector of values
      value = structure(list(),sync=TRUE),
      #' @description A generic initializer function
      #' @param value A value to initialize instance with
      #' @param ... Any other arguments, ignored.
      initialize = function(value,...){
          mycall <- match.call()
          super$initialize(...)
          if(!missing(value)){
              self$value <- value
              mycall$value <- value
          } else mycall$value <- NULL
          mycall[[1]] <- as.symbol(class(self)[1])
          self$call <- paste(deparse(mycall), collapse="\n")
      },
      #' @description Add handler function to be called when value is changed
      #' @param handler A handler function
      #' @param remove A logical value, whether the handler should be removed
      #'    or added.
      on_change = function(handler,remove=FALSE){
          self$observe("value",handler,remove)
      }
  )
)
