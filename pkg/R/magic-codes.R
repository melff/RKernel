#' @include display.R

magic_handlers <- new.env()

dispatch_magic_handler <- function(magic,code,args) {
    # log_out("dispatch_magic_handler")
    magic <- tolower(magic)
    handler <- get0(magic,envir=magic_handlers,inherits=FALSE)
    if(is.null(handler)) stop("Unsupported cell magic")
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

getMatch <- function(x,match){
    if(length(match) < 0) return(character(0))
    if(is.list(match))
        match <- match[[1]]
    lens <- attr(match,"match.length")
    n <- length(match)
    res <- character(n)
    for(i in 1:n){
        start <- match[i]
        end <- start + lens[i] - 1
        res[i] <- substr(x,start=start,stop=end)
    }
    res
}

get_match <- function(x, pattern) {
  getMatch(x,regexec(pattern,x))
}

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
    if(!grepl("\n",code,fixed=TRUE)) code <- paste0(code,"\n")
    perc_match <- getMatch(code,regexec("^%%([a-zA-Z0-9]+).*?\n",code))
    if(length(perc_match) > 1){
        magic <- perc_match[2]
        perc_line <- perc_match[1]
        args <- parse_magic_args(perc_line)
        code <- gsub("^%%.+?\n","",code)
        list(magic=magic,args=args,code=code)
    } else NULL
}

special_regex <- "^#!([a-zA-Z0-9_]+):\\s*(\\S.*)?$"
comment_handlers <- new.env()

dispatch_special_comment_handler <- function(opcode,args) {
    # log_out("dispatch_special_comment_handler")
    opcode <- tolower(opcode)
    handler <- get0(opcode,envir=comment_handlers,inherits=FALSE)
    if(is.null(handler)) stop("Unsupported special comment")
    handler(args,opcode)
}

get_special_comment1 <- function(line) {
    m <- getMatch(line,regexec(special_regex,line))
    list(opcode=m[2], args=m[3])
}

get_special_comments <- function(code) {
    # log_out("get_special_comments")
    lines <- split_lines1(code)
    sel <- grepl(special_regex,lines)
    lines <- lines[sel]
    lapply(lines,get_special_comment1)
}

register_comment_handler <- function(opcode,handler){
    if(is.function(handler))
        assign(opcode,handler,envir=comment_handlers)
    else
        remove(list=as.character(opcode),envir=comment_handlers)
}

opt_comment_handler <- function(args, ...) {
    args <- sprintf("options(%s)",args)
    expr <- try(str2expression(args))
    if(!inherits(expr,"try-error")) {
        eval(expr)
    }
}

cell_opt_comment_handler <- function(args, ...) {
    args <- sprintf("cell_options(%s)",args)
    expr <- try(str2expression(args))
    if(!inherits(expr,"try-error")) {
        eval(expr)
    }
}


register_comment_handler("opt", opt_comment_handler)
register_comment_handler("cell_opt", cell_opt_comment_handler)

cell_save_env <- new.env()

cell_options <- function(...) {
    opts <- options(...)
    
    for(n in names(opts)) {
        assign(n,opts[[n]],envir=cell_save_env)
    }
    invisible()
}

restore_options <- function() {
    n <- ls(cell_save_env)
    opts <- lapply(n,get0,envir=cell_save_env)
    names(opts) <- n
    options(opts)
}