saved_options <- new.env()

save_options <- function(...){
    last <- saved_options$last
    opts  <- options(...)
    if(length(last)){
        last[names(opts)] <- opts
        saved_options$last <- last
    }
    else
        saved_options$last <- opts
}

is_option_changed <- function(n){
    !identical(saved_options$last[[n]],
               getOption(n))
}

any_option_changed <- function(...){
    nms <- c(...)
    any(sapply(nms,is_option_changed))
}

