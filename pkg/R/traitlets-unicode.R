#' A Unicode String Vector Trait
#' 
#' @include traitlets.R
#' @export
UnicodeClass <- R6Class_("Unicode",
   inherit=TraitClass,
   public=list(
       #' @field coerce Logical value, whether values should be coerced to character strings.
       coerce=TRUE,
       #' @field length Length of the unicode character vector
       length = 1,
       #' @field value The value of the unicode character vector
       value = character(0),
       #' @description A validator function
       #' @param value The value to be assigned
       validator=function(value){
            if(self$coerce){
                value <- as.character(value)
            }
            else {    
                if(!is.character(value))
                    stop("Unicode: character string required")
            }
            if(length(value) > 0 && !all(validUTF8(value))) 
                stop("Unicode: valid utf8 required")
            if(length(value) == 0 && !self$optional || 
               length(value) > 0 && is.finite(self$length) && length(value) != self$length) {
                e_msg <- sprintf("Unicode: expected length is %d but the value has length %d",
                                 self$length,length(value))
                #print(value)
                stop(e_msg)
            }
            value
       },
       #' @description Initialize an object
       #' @param initial An initial value
       #' @param coerce Logical value, whether values should be coerced to character stringes
       #' @param optional Logical value, whether the value may be empty
       #' @param length Integer, the intended length of the unicode vector
       initialize=function(initial=character(0),
                           coerce=TRUE,
                           optional=length(initial) == 0,
                           length=1L
                           ){
            self$optional <- optional
            self$coerce <- coerce
            if(missing(length))
                self$length <- max(length(initial),1L)
            else
                self$length <- length
            super$initialize(initial)
        }
   )
)
#' A Unicode String Trait Constructor
#' @param ... Arguments passed to the trait instance initializer
#' @export
Unicode <- function(...)TraitInstance(Class=UnicodeClass,...)

#' @describeIn UnicodeClass Coerce a unicode string trait to a character vector
#' @param x A Unicode traitlet
#' @param ... Other arguments, ignored.
#' @export
as.character.Unicode <- function(x,...) x$value
