#' @include traitlets.R

#' @export
BooleanClass <- R6Class_("Boolean",
    inherit=TraitClass,
    public=list(
        value = logical(0),
        optional = FALSE,
        coerce = TRUE,
        length = 1,
        validator=function(value){
            if(self$coerce){
                value <- as.logical(value)
            }
            else {    
                if(!is.logical(value))
                    stop("Boolean: logical value required")
            }
            if(length(value) == 0 && !self$optional || 
               length(value) > 0 && is.finite(self$length) && length(value) != self$length) {
                e_msg <- sprintf("Boolean: expected length is %d but the value has length %d",
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
        }
))

#' @export
Boolean <- function(...)TraitInstance(Class=BooleanClass,...)
