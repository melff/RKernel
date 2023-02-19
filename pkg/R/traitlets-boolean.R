#' Boolean Traitlets
#'
#' @description A class and a constructor function to create boolean train(let)s.
#'
#' @include traitlets.R
#' @name Boolean

#' @rdname Boolean
#' @export
BooleanClass <- R6Class_("Boolean",
    inherit=TraitClass,
    public=list(
        #' @field value A logical vector, usually of length 1
        value = logical(0),
        #' @field optional Logical, whether an initializing logical value must be provided
        optional = FALSE,
        #' @field coerce Logical, whether 'as.logical()' is implicitely used when a value is
        #'   assigned to the trait
        coerce = TRUE,
        #' @field length Integer, the length of the logical vector that poses as the value of the traitlet.
        length = 1,
        #' @description
        #' A validator method
        #' @param value A value to be checked for validity
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
        #' @description
        #' The initializing method
        #' @param initial A value with which the traitlet is initialized
        #' @param coerce Logical, used to initialize the 'coerce' field
        #' @param optional Logical, used to initialize the 'optional' field
        #' @param length Integer, used to initialize the 'length' field
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

#' @rdname Boolean
#' @param ... Arguments that are passed to the initialize method of 'BooleanClass'
#' @export
Boolean <- function(...)TraitInstance(Class=BooleanClass,...)
