#' @export
Unicode <- function(initial=character(0),
                   observe=FALSE,
                   sync=TRUE,
                   optional=length(initial)==0,
                   notifier=function(n,v) print(sprintf("Trait '%s' changed to value '%s'",n,v)))
    structure(
        list(
        initial=as.character(initial),    
        validator=function(value){
                if(!is.character(value)) stop("wrong type")
                if(!optional && length(value) == 0) stop("character string required")
                if(length(value) > 1) stop("scalar string required")
                if(!validUTF8(value)) stop("valid utf8 required")
                value
        },
        observe=observe,
        notifier=notifier,
        sync=sync
    ),class=c("Unicode","Trait"))
