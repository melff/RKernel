#' @include traitlets.R
#'
#' @export
IntegerClass <- R6Class_("Integer",
    inherit=TraitClass,
    public=list(
        value = integer(0),
        optional = FALSE,
        coerce = TRUE,
        length = 1,
        validator=function(value){
            if(self$coerce){
                value <- as.integer(value)
            }
            else {    
                if(!is.integer(value))
                    stop("Integer: integer value required")
            }
            if(length(value) == 0 && !self$optional || 
               length(value) > 0 && is.finite(self$length) && length(value) != self$length) {
                e_msg <- sprintf("Integer: expected length is %d but the value has length %d",
                                 self$length,length(value))
                stop(e_msg)
            }
            value
        },
        initialize=function(initial=integer(0),coerce=TRUE,
                            optional=length(initial) == 0,
                            length=1
                            ){
            self$optional <- optional
            self$coerce <- coerce
            if(missing(length) && !missing(initial))
                self$length <- max(length(initial),1L)
            else
                self$length <- length
            super$initialize(initial)
        }))

#' @export
Integer <- function(...)TraitInstance(...,Class=IntegerClass)

#' @export
as.integer.Integer <- function(x,...) x$value
#' @export
as.numeric.Integer <- function(x,...) as.numeric(x$value)


#' @export
to_json.Integer <- function(x) {
    len <- x$length
    if(is.finite(len) && len <= 1) {
        value <- x$get()
        if(!length(value))
            integer(0)
        else
            value
    }
    else {
        value <- x$get()
        as.list(value)
    }
}


#' @export
FloatClass <- R6Class_("Float",
    inherit=TraitClass,
    public=list(
        value = numeric(0),
        optional = FALSE,
        coerce = TRUE,
        length = 1,
        validator=function(value){
            if(self$coerce){
                value <- as.numeric(value)
            }
            else {    
                if(!is.numeric(value))
                    stop("Float: numeric value required")
            }
            if(length(value) == 0 && !self$optional || 
               length(value) > 0 && is.finite(self$length) && length(value) != self$length) {
                e_msg <- sprintf("Float: expected length is %d but the value has length %d",
                                 self$length,length(value))
                stop(e_msg)
            }
            value
        },
        initialize=function(initial,coerce=TRUE,
                            optional=length(initial) == 0,
                            length=1){
            self$optional <- optional
            self$coerce <- coerce
            if(missing(length))
                self$length <- max(length(initial),1L)
            else
                self$length <- length
            super$initialize(initial)
        }))

#' @export
Float <- function(...)TraitInstance(...,Class=FloatClass)

#' @export
as.integer.Float <- function(x,...) as.integer(x$value)
#' @export
as.numeric.Float <- function(x,...) x$value
