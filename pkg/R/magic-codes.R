#' @include display.R

magic_handlers <- new.env()

dispatch_magic_handler <- function(magic,code,args) {
    if(exists(magic,magic_handlers))
        handler <- get(magic,envir=magic_handlers)
    else stop("Unsupported cell magic")
    handler(code,args)
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

parse_magic_args <- function(perc_line){
   line_match <- getMatch(perc_line,regexec("^%%[a-zA-Z0-9]+\\s*(.*?)\n",perc_line))
   if(length(line_match)>1){
     tryCatch({
       magic_args <- line_match[2]
       magic_args <- unlist(strsplit(magic_args,",\\s*"))
       magic_args <- strsplit(magic_args,"\\s*=\\s*")
       magic_arg_names <- unlist(lapply(magic_args,"[",1))
       magic_arg_values <- lapply(magic_args,"[",2)
       structure(magic_arg_values,names=magic_arg_names)   
     },error=function(e)stop("Error in parsing arguments"))
   } else NULL
}

parse_magic <- function(code){
    perc_match <- getMatch(code,regexec("^%%([a-zA-Z0-9]+).*?\n",code))
    if(length(perc_match) > 1){
        magic <- perc_match[2]
        perc_line <- perc_match[1]
        args <- parse_magic_args(perc_line)
        code <- gsub("^%%.+?\n","",code)
        list(magic=magic,args=args,code=code)
    } else NULL
}

