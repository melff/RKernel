#' Generic Vector Traitles
#'
#' @description A class and a constructor function to create generic vector trait(let)s.
#'
#' @include traitlets.R
#' @name Vector

#' @rdname Vector
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

#' @rdname Vector
#' @param ... Arguments that are passed to the initialize method of 'VectorClass'
#' @export
Vector <- function(...)TraitInstance(...,Class=VectorClass)

#' @describeIn to_json S3 method for 'VectorClass' objects
#' @export
to_json.Vector <- function(x,...){
    unlist(lapply(x$get(),to_json))
}

#' List Traitlets
#'
#' @description A class and a constructor function to create list trait(let)s.
#'
#' @include traitlets.R
#' @name List

#' @rdname List
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

#' @rdname List
#' @param ... Arguments that are passed to the initialize method of 'ListClass'
#' @export
List <- function(...)TraitInstance(...,Class=ListClass)

#' @describeIn to_json S3 method for 'ListClass' objects
#' @export
to_json.List <- function(x,auto_unbox=TRUE,...){
    value <- lapply(x$get(),to_json)
    # if(length(value) == 1)
    #     value <- list(value)
    value
}

#' Dictionary Traitlets
#'
#' @description A class and a constructor function to dictionary trait(let)s.
#'    These are lists with unique element names.
#'
#' @include traitlets.R
#' @name Dict


#' @rdname Dict
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

#' @rdname Dict
#' @param ... Arguments that are passed to the initialize method of 'DictClass'
#' @export
Dict <- function(...)TraitInstance(...,Class=DictClass)
