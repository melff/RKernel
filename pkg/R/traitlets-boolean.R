#' @include traitlets.R

#' @export
BooleanClass <- R6Class_("Boolean",
    inherit=TraitClass,
    public=list(
        initial= logical(0),
        value = logical(0),
        optional = FALSE,
        validator=function(value){
            if(!is.logical(value)) stop("wrong type")
            if(!self$optional && length(value) == 0) stop("logical value required")
            if(length(value) > 1) stop("scalar logical required")
            value
        },
        initialize = function(initial=FALSE,optional=length(initial)==0){
                           self$optional <- optional
                           initial <- self$validator(initial)
                           self$initial <- initial
                           self$value <- initial
                       }
))
#' @export
Boolean <- function(...)BooleanClass$new(...)
