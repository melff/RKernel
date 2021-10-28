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
        initialize=function(klass,...){
            self$class <- klass
            self$init_args <- list(...)
        },
        setup = function(){
            str(self$class)
            initial <- do.call(self$class$new,self$init_args)
            self$initial <- initial
            self$value <- initial
            self$class <- class(initial)
        }
    )
)
#' @export
R6Instance <- function(...)R6TraitClass$new(...)
