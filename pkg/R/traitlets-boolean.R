#' @export
Boolean <- function(initial=FALSE,
                   observe=FALSE,
                   sync=TRUE,
                   optional=length(initial)==0,
                   notifier=function(n,v) print(sprintf("Trait '%s' changed to value '%d'",n,v)))
    structure(
        list(
        initial=as.logical(initial),    
        validator=function(value){
                if(!is.logical(value)) stop("wrong type")
                if(!optional && length(value) == 0) stop("logical value required")
                if(length(value) > 1) stop("scalar logical required")
                value
        },
        observe=observe,
        notifier=notifier,
        sync=sync
    ),class=c("Boolean","Trait"))
