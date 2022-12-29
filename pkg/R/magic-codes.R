#' @include display.R

magic_handlers <- new.env()

dispatch_magic_handler <- function(magic,code,args) {
    if(exists(magic,magic_handlers))
        handler <- get(magic,envir=magic_handlers)
    else stop("Unsupported cell magic")
    handler(code,args)
}

register_magic_handler <- function(magic,handler){
    assign(magic,handler,envir=magic_handlers)
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
