#' A Genereric Converter to JSON
#'
#' @description The function \code{to_json} is a generic and idempotent interface
#'     to \code{\link[jsonlite]{toJSON}}.
#'
#' @param x An object to be converted into JSON
#' @param auto_unbox A logical value, whether one-element a JSON list should be
#'     changed into JSON scalar.
#' @param ... Other arguments, passed on to  \code{\link[jsonlite]{toJSON}}.
#' @importFrom jsonlite fromJSON toJSON
#' @export
to_json <- function(x,auto_unbox=TRUE,...) UseMethod("to_json")

#' @describeIn to_json Default S3 method
#' @export
to_json.default <- function(x,auto_unbox=TRUE,...) {
    if(is.null(x) || length(x) == 0) {
        if(auto_unbox) structure("null",class="json")
        else  structure("[]",class="json")
    }
    else tryCatch(toJSON(x,auto_unbox=auto_unbox,json_verbatim=TRUE,...),
                  error=toJSON(unclass(x),auto_unbox=auto_unbox,json_verbatim=TRUE,...))
}

#' @describeIn to_json S3 method for lists.
#' @export
to_json.list <- function(x,auto_unbox=TRUE,...) {
    y <- lapply(x,to_json,auto_unbox=auto_unbox,...)
    toJSON(y,...,json_verbatim=TRUE) 
}

#' @describeIn to_json S3 method for JSON character strings. Returns its
#'     argument as is, making \code{to_json} idempotent.
#' @export
to_json.json <- function(x,...) x

as_json <- function(x) structure(as.character(x),class="json")
