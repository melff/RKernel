#' @include display.R

magic_handlers <- new.env()

dispatch_magic_handler <- function(magic,code) {
    if(exists(magic,magic_handlers))
        handler <- get(magic,envir=magic_handlers)
    else stop("Unsupported cell magic")
    handler(code)
}

register_magic_handler <- function(magic,handler){
    assign(magic,handler,envir=magic_handlers)
}

iframe_cell_handler <- function(code){
    e <- get_evaluator()
    text_html <- e$str2iframe(code,class="rkernel-iframe-magic")
    raw_html(text_html)
}

register_magic_handler("math",LaTeXMath)
register_magic_handler("css",CSS)
register_magic_handler("javascript",Javascript)
register_magic_handler("html",raw_html)
register_magic_handler("iframe",iframe_cell_handler)
