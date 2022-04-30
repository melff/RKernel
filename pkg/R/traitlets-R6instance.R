#' A Base Class for Traits that are R6 Objects
#'
#' @include traitlets.R
#' @export
R6TraitClass <- R6Class_("R6Trait",
    inherit=TraitClass,
    public=list(
        #' @field value An R6 object
        value=NULL,
        #' @field class The R6 class of \code{value}
        class=NULL,
        #' @description Checks wether \code{value} has the corret class
        #' @param value A value about to be assigned to the trait.
        validator=function(value){
            if(!isa(value,self$class)) stop("wrong class")
            value
        },
        #' @description Initialize an object
        #' @param Class Class of the object
        #' @param ... Values used for initialization
        initialize=function(Class,...){
            args <- list(...)
            initial <- Class$new(...)
            self$value <- initial
            self$class <- class(initial)
        }
    )
)

#' A Generic Constructor for R6 Object Traits 
#' @export
R6Instance <- function(...)TraitInstance(Class=R6TraitClass,...)
