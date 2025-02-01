runHooks <- function(name){
    for(FUN in getHook(name)){
        if(is.character(FUN))
            FUN <- get(FUN)
        try(FUN())
    }
}
