#' @export
StrEnum <- function(enum,
                    initial=character(0),
                    observe=FALSE,
                    sync=TRUE,
                    optional=FALSE,
                    notifier=function(n,v) print(sprintf("Trait '%s' changed to value '%s'",n,v)))    
    structure(
        list(
        initial=as.character(initial),    
        validator=function(value){
                if(!is.character(value)) stop("wrong type")
                if(!optional && length(value) == 0) stop("character string required")
                if(length(value) > 1) stop("scalar string required")
                if(!(value %in% enum)) stop("incorrect value")
                value
        },
        observe=observe,
        notifier=notifier,
        sync=sync
    ),class=c("StrEnum","Trait"))
