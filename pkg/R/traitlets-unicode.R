#' @include traitlets.R

#' @export
UnicodeClass <- R6Class_("Unicode",
   inherit=TraitClass,
   public=list(
        coerce=TRUE,
        length = 1,
        value = character(0),
        validator=function(value){
            if(self$coerce){
                value <- as.character(value)
            }
            else {    
                if(!is.character(value))
                    stop("Unicode: character string required")
            }
            if(length(value) > 0 && !validUTF8(value)) 
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
       initialize=function(initial=character(0),
                           coerce=TRUE,
                           optional=length(initial) == 0,
                            length=1
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
#' @export
Unicode <- function(...)TraitInstance(Class=UnicodeClass,...)
#' @export
as.character.Unicode <- function(x,...) x$value
