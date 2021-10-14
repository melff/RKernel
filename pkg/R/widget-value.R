#' @export
Value <- function(self){
  function(value){
      if(missing(value)){
        return(self$state$value)
      } 
      self$state$value <- value
      cat("value changed")
      self$notify_change("value")
    }
}

ValueWidgetClass <- R6Class("ValueWidget",
  inherit = WidgetClass,
  public = list(
    new_trait = trait(1L,sync=TRUE),
    initialize = function(...){
      super$initialize(...)
    }
  ),
  active = list(
    value = Value(self) 
  ),
)

#' @export
ValueWidget <- function(...) ValueWidgetClass$new(...)
