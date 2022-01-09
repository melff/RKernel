#' @importFrom jsonlite fromJSON toJSON

#' @export
to_json <- function(x,auto_unbox=TRUE,...) UseMethod("to_json")

#' @export
to_json.default <- function(x,auto_unbox=TRUE,...) {
    if(is.null(x) || length(x) == 0) {
        if(auto_unbox) structure("null",class="json")
        else  structure("[]",class="json")
    }
    else toJSON(x,auto_unbox=auto_unbox,json_verbatim=TRUE,...)
}

#' @export
to_json.list <- function(x,auto_unbox=TRUE,...) {
    y <- lapply(x,to_json,auto_unbox=auto_unbox,...)
    toJSON(y,...,json_verbatim=TRUE) 
}

#' @export
to_json.json <- function(x,...) x

as_json <- function(x) structure(as.character(x),class="json")
