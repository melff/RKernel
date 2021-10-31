#' @include traitlets.R

#' @export
UnicodeClass <- R6Class_("Unicode",
   inherit=TraitClass,
   public=list(
        initial=character(0),
        optional = FALSE,
        value = character(0),
        validator=function(value){
                if(!is.character(value)) stop("wrong type")
                if(!self$optional && length(value) == 0) stop("initial value has length zero")
                if(length(value) > 1) stop("scalar string required")
                if(length(value) > 0 && !validUTF8(value)) stop("valid utf8 required")
                value
        },
        initialize = function(initial=character(0),optional=length(initial)==0){
            initial <- as.character(initial)
            self$optional <- optional
            initial <- self$validator(initial)
            self$initial <- initial
            self$value <- initial
        }
   )
)
#' @export
Unicode <- function(...)TraitInstance(Class=UnicodeClass,...)
#' @export
as.character.Unicode <- function(x,...) x$value
