#' @export
# Adapted from the utils package
dump.frames <- function(dumpto = "last.dump", 
                        to.file = FALSE, 
                        include.GlobalEnv = FALSE, 
                        drop.kernel.frames = TRUE,
                        drop.last = 4) 
{
    calls <- sys.calls()
    last.dump <- sys.frames()
    if(drop.kernel.frames)
        depth <- which(sapply(last.dump,identical,.GlobalEnv))
    else
        depth <- 0
    if(depth > 0)
        calls <- tail(calls,-depth)
    calls <- head(calls,-drop.last)
    if(depth > 0)
        last.dump <- tail(last.dump,-depth)
    last.dump <- head(last.dump,-drop.last)
    
    names(last.dump) <- limitedLabels(calls)
    if (include.GlobalEnv) {
        last.dump <- c(.GlobalEnv = as.environment(as.list(.GlobalEnv, 
            all.names = TRUE)), last.dump)
    }
    last.dump <- last.dump[-length(last.dump)]
    attr(last.dump, "error.message") <- geterrmessage()
    class(last.dump) <- "dump.frames"
    if (dumpto != "last.dump") 
        assign(dumpto, last.dump)
    if (to.file) 
        save(list = dumpto, file = paste0(dumpto, ".rda"))
    else assign(dumpto, last.dump, envir = .GlobalEnv)
    invisible()
}
