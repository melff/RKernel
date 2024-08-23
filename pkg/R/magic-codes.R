#' @include display.R

magic_handlers <- new.env()

dispatch_magic_handler <- function(magic,code,args) {
    log_out("dispatch_magic_handler")
    magic <- tolower(magic)
    handler <- get0(magic,envir=magic_handlers,inherits=FALSE)
    if(is.null(handler)) stop("Unsupported cell magic")
    d <- handler(code,args)
    log_out(d,use.str=TRUE)
    if(!is.null(d))
        display(d)
}


#' Register a handler for magics
#'
#' Similar to '%%' magics in Jupyter/python it is possible to use such masics with this R kernel.
#' There are pre-defined magics for LaTeX math, CSS, Javascrippt, HTML, and iframes.
#'
#' @param magic A character string that selects a handler
#' @param handler A function that takes at least the argument 'code' and more '...' arguments.
#'    The latter are constructed from the arguments of the percentage magic.
#' @export
register_magic_handler <- function(magic,handler){
    if(is.function(handler))
        assign(magic,handler,envir=magic_handlers)
    else
        remove(list=as.character(magic),envir=magic_handlers)
}

iframe_cell_handler <- function(code,args){
    args$code <- code
    args$class <- "rkernel-iframe-magic"
    text_html <- do.call(str2iframe,args)
    raw_html(text_html)
}

register_magic_handler("math",function(code,...)LaTeXMath(code))
register_magic_handler("css",function(code,...)CSS(text=code))
register_magic_handler("javascript",function(code,...)Javascript(text=code))
register_magic_handler("html",function(code,...)raw_html(text=code))
register_magic_handler("iframe",function(code,args,...)iframe_cell_handler(code,args))
