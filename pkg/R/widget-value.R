#' @include widget-description.R

#' @export
ValueWidgetClass <- R6Class_("ValueWidget",
  inherit = DescriptionWidgetClass,
  public = list(
      value = structure(list(),sync=TRUE),
      initialize = function(value,...){
          super$initialize(...)
          if(!missing(value))
              self$value <- value
      },
      on_change = function(handler,remove=FALSE){
          self$observe("value",handler,remove)
      }
  )
)
