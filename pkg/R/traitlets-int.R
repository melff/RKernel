#' @include traitlets.R
#'
#' @export
IntegerClass <- R6Class_("Integer",
   inherit=TraitClass,
   public=list(
        initial=integer(0),
        value = integer(0),
        validator=function(value){
                if(!is.numeric(value)) stop("wrong type")
                as.integer(value)
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
Integer <- function(...)TraitInstance(...,Class=IntegerClass)

as.integer.Integer <- function(x,...) x$value

#' @export
BoundedIntClass <- R6Class_("BoundedInteger",
   inherit=IntegerClass,
   public=list(
        min = integer(0),
        max = integer(0),
        validator=function(value){
                value <- super$validator(value)
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
BoundedInteger <- function(...)TraitInstance(Class=BoundedIntClass,...)
