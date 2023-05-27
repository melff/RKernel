#' An Enumerated Strings Trait
#'
#' @include traitlets.R traitlets-unicode.R
#' @export
StrEnumClass <- R6Class_("StrEnum",
   inherit=UnicodeClass,
   public=list(
       #' @field enum a character vector
       enum=character(0),
       #' @field optional A logical value, whethe value can be empty.
       optional=FALSE,
       #' @description Check whether the assigned vector is one of the allowed enumerated strings.
       #' @param value A value to be assigned to the trait
       validator=function(value){
           value <- super$validator(value)
           if(length(value) > 0 && !(value %in% self$enum)) stop("incorrect value")
           value
       },
       #' @description Initialize the trait.
       #' @param enum A character vector of permitted enumerated strings.
       #' @param default The default value
       #' @param optional Logical can the value be empty?
       initialize = function(enum,default=character(0),optional=FALSE){
           self$optional <- optional
           self$enum <- as.character(enum)
           if(length(default))
               super$initialize(default)
        }
   )
)

#' An Enumerated String Constructor
#' @param ... Arguments passed to the trait instance initializer
#' @export
StrEnum <- function(...)TraitInstance(Class=StrEnumClass,...)
