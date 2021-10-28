#' @include traitlets.R
#'
#' @export
IntegerClass <- R6Class_("Integer",
   inherit=TraitClass,
   public=list(
        initial=integer(0),
        value = integer(0),
        validator=function(value){
                if(!is.integer(value)) stop("wrong type")
                value
        },
        initialize = function(initial=0L){
            initial <- as.integer(initial)
            initial <- self$validator(initial)
            self$initial <- initial
            self$value <- initial
        }
   )
)
#' @export
Integer <- function(...)IntegerClass$new(...)

as.integer.Integer <- function(x,...) x$value

#' @export
BoundedIntClass <- R6Class_("BoundedInteger",
   inherit=TraitClass,
   public=list(
        initial=integer(0),
        value = integer(0),
        min = integer(0),
        max = integer(0),
        validator=function(value){
                if(!is.integer(value)) stop("wrong type")
                if(value > self$max || value < self$min)
                    stop("value out of range")
                value
        },
        initialize = function(initial=0L,
                              range){
            self$min <- range[1]
            self$max <- range[2]
            initial <- as.integer(initial)
            initial <- self$validator(initial)
            self$initial <- initial
            self$value <- initial
        }
   )
)
#' @export
BoundedInteger <- function(...)BoundedIntClass$new(...)
