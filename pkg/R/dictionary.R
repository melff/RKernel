#' @title A data type analogous to Python dictionaries
#'
#' @description Objects of class "dictionary" behave similar to dictionaries. They can
#'    contain any other kind of objects, but like with Python dictionaries, only scalar
#'    indices are allowed. Unlike with Python dictionaries, numeric indices can be used
#'    well as character indices.
#' @name dictionary
NULL

#' @describeIn dictionary A dictionary constructor
#' @param ... Arbitrary objects. Should be tagged, yet currently name tags are
#'     not yet checked for.
#' @export
dictionary <- function(...){
    structure(list(...),class="dictionary")
}

#' @describeIn dictionary Get an element from a dictionary
#' @param x A dictionary object
#' @param i A scalar integer or character string
#' @export
"[.dictionary" <- function(x,i){
    if(length(i)!=1) {
        stop("Only scalar indices allowed")
    }
    if(is.numeric(i) && i > length(x)) return(NULL)
    x[[i]]
}

#' @describeIn dictionary Set an element in a dictionary
#' @param x A dictionary object
#' @param i A scalar integer or character string
#' @param value An arbitrary object
#' @export
"[<-.dictionary" <- function(x,i,value){
    if(length(i)!=1) {
        stop("Only scalar indices allowed")
    }
    x[[i]] <- value
    keys <- names(x)
    i <- seq_along(x)
    nnz <- !nzchar(keys)
    keys[nnz] <- i[nnz]
    names(x) <- keys
    x
}

#' @describeIn dictionary Print a dictionary
#' @param x A dictionary object
#' @param force A logical scalar, if TRUE, each element of the dictionary is printed,
#'       if FALSE, just a brief summary is printed.
#' @export
print.dictionary <- function(x,force=FALSE,...){
    if(!force)
        cat(sprintf("< dictionary with %d elements >",length(x)))
    else{
        if(length(x)){
            keys <- names(x)
            if(length(keys)){
                i <- seq_along(x)
                nnz <- !nzchar(keys)
                keys[nnz] <- i[nnz]
            } else {
                keys <- seq_along(x)
            }
            for(k in keys){
                cat(paste0(k,":\n"))
                orig_func$print(x[k],...)
            }
            
        }
    }
}
