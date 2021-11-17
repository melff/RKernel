#' @include traitlets.R
#'
#' @export
VectorClass <- R6Class_("Vector",
    inherit=TraitClass,
    public=list(
        value = list(),
        class = NULL,
        validator=function(value){
            if(!is.list(value)) stop("value must be a list")
            self$class <- class(value[[1]])
            if(length(value)>1){
                n <- length(value)
                for(i in 2:n){
                    if(!identical(self$class,class(value[[i]])))
                        stop("all values must have the same class")
                }
            }
            value
        }
))

#' @export
Vector <- function(...)TraitInstance(...,Class=VectorClass)

#' @export
to_json.Vector <- function(x){
    unlist(lapply(x$get(),to_json))
}


#' @export
DictClass <- R6Class_("Dict",
    inherit=VectorClass,
    public=list(
        validator=function(value){
            value <- super$validator(value)
            nms <- names(value)
            if(!length(nms) || anyDuplicated(nms))
                stop("names must be unique")
            value
        }
))

#' @export
Dict <- function(...)TraitInstance(...,Class=DictClass)


#' @export
ListClass <- R6Class_("List",
    inherit=TraitClass,
    public=list(
        value = list(),
        validator=function(value){
            if(!is.list(value)) stop("value must be a list")
            value
        }
))

#' @export
List <- function(...)TraitInstance(...,Class=ListClass)

#' @export
to_json.List <- function(x){
    value <- lapply(x$get(),to_json)
    # if(length(value) == 1)
    #     value <- list(value)
    value
}
