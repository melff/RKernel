#' @include traitlets.R traitlets-unicode.R

#' @export
StrEnumClass <- R6Class_("StrEnum",
   inherit=UnicodeClass,
   public=list(
       enum=character(0),
       validator=function(value){
           value <- super$validator(value)
           if(length(value) > 0 && !(value %in% self$enum)) stop("incorrect value")
           value
       },
       initialize = function(enum,default=character(0),optional=FALSE){
           self$optional <- optional
           self$enum <- as.character(enum)
           super$initialize(default,optional=optional)
        }
   )
)
#' @export
StrEnum <- function(...)TraitInstance(Class=StrEnumClass,...)
