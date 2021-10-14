#' @export
Instance <- function(klass,
                   observe=FALSE,
                   sync=TRUE,
                   ...
                   )
    structure(
        list(
        initial=klass$new(...),    
        validator=function(value){
            if(length(initial$validator))
                value <- initial$validator(value)
            value
        },
        observe=observe,
        notifier=notifier,
        sync=sync
    ),class=c("Unicode","Trait"))
