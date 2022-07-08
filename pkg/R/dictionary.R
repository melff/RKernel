#' @export
dictionary <- function(...){
    structure(list(...),class="dictionary")
}

#' @export
"[.dictionary" <- function(x,i){
    if(length(i)!=1) {
        stop("Only scalar indices allowed")
    }
    if(is.numeric(i) && i > length(x)) return(NULL)
    x[[i]]
}

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
                print_(x[k],...)
            }
            
        }
    }
}
