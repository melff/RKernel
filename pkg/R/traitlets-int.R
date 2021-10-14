#' @export
BoundedInteger <- function(initial=0L,
                           range,
                           observe=FALSE,
                           sync=TRUE,
                           notifier=function(n,v) print(sprintf("Trait '%s' changed to value %d",n,v)))
    structure(
        list(
        initial=as.integer(initial),    
        validator=function(value){
                min <- as.integer(range[1])
                max <- as.integer(range[2])
                if(!is.integer(value)) stop("wrong type")
                if(value > max || value < min)
                    stop("value out of range")
                value
        },
        observe=observe,
        notifier=notifier,
        sync=sync
    ),class=c("BoundedInt","Trait"))

# HasBoundedInt <- R6Class_("HasBoundedInt",
#   inherit=HasTraits,
#   public=list(
#       value=BoundedInt(0L,c(0L,100L),TRUE)
#   )
# )
