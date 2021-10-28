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
       initialize = function(enum,
                             initial=character(0),
                             optional=length(initial)==0){
           self$enum <- as.character(enum)
           super$initialize(initial,optional)
        }
   )
)
#' @export
StrEnum <- function(...)StrEnumClass$new(...)
