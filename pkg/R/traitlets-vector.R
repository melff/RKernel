#' A Generic Vector Trait
#' 
#' @include traitlets.R
#' @export
VectorClass <- R6Class_("Vector",
    inherit=TraitClass,
    public=list(
        #' @field value A list
        value = list(),
        #' @field class A character string, the common class of the vector elements
        class = NULL,
        #' @description A function that checks the validity of an assigned value, 
        #'       i.e. whether the assigned value is a list with all elements of the same class
        #' @param value A value to be assigned as the traitlet value
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

#' A Constructor for VectorClass Objects
#' @export
Vector <- function(...)TraitInstance(...,Class=VectorClass)

#' @describeIn to_json S3 method for 'VectorClass' objects
#' @export
to_json.Vector <- function(x,...){
    unlist(lapply(x$get(),to_json))
}

#' A List Trait Class
#' @export
ListClass <- R6Class_("List",
    inherit=TraitClass,
    public=list(
        #' @field value A list
        value = list(),
        #' @description A function that checks the validity of an assigned value, 
        #'       i.e. whether the assigned value is a list
        #' @param value A value to be assigned as the traitlet value
        validator=function(value){
            if(!is.list(value)) stop("value must be a list")
            value
        }
))

#' A Constructor for ListClass Objects
#' @export
List <- function(...)TraitInstance(...,Class=ListClass)

#' @export
to_json.List <- function(x,auto_unbox=TRUE,...){
    value <- lapply(x$get(),to_json)
    # if(length(value) == 1)
    #     value <- list(value)
    value
}


#' A Dictionary Class, i.e. of Lists with Unique Element Names
#' @export
DictClass <- R6Class_("Dict",
    inherit=ListClass,
    public=list(
        #' @description A function that checks the validity of an assigned value, 
        #'       i.e. whether the assigned value is a list with unique names
        #' @param value A value to be assigned as the traitlet value
        validator=function(value){
            value <- super$validator(value)
            nms <- names(value)
            if(!length(nms) || anyDuplicated(nms))
                stop("names must be unique")
            value
        }
))

#' A Constructor for DictClass Objects
#' @export
Dict <- function(...)TraitInstance(...,Class=DictClass)
