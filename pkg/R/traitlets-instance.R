#' @include traitlets.R

#' @export
R6TraitClass <- R6Class_("R6Trait",
    inherit=TraitClass,
    public=list(
        initial=NULL,
        value=NULL,
        class=NULL,
        init_args=NULL,
        validator=function(value){
            if(!isa(value,self$class)) stop("wrong class")
        },
        initialize=function(Class,...){
            initial <- Class$new(...)
            self$initial <- initial
            self$value <- initial
            self$class <- class(initial)
        }
    )
)
#' @export
R6Instance <- function(...)TraitInstance(Class=R6TraitClass,...)
