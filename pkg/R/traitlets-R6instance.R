#' @include traitlets.R

#' @export
R6TraitClass <- R6Class_("R6Trait",
    inherit=TraitClass,
    public=list(
        value=NULL,
        class=NULL,
        init_args=NULL,
        validator=function(value){
            if(!isa(value,self$class)) stop("wrong class")
            value
        },
        initialize=function(Class,...){
            args <- list(...)
            initial <- Class$new(...)
            self$value <- initial
            self$class <- class(initial)
        }
    )
)
#' @export
R6Instance <- function(...)TraitInstance(Class=R6TraitClass,...)
