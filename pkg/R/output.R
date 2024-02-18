## From R base
cat_ <- function (..., file = "", sep = " ", fill = FALSE, labels = NULL, 
    append = FALSE) 
{
    if (is.character(file)) 
        if (file == "") 
            file <- stdout()
        else if (startsWith(file, "|")) {
            file <- pipe(substring(file, 2L), "w")
            on.exit(close(file))
        }
        else {
            file <- file(file, ifelse(append, "a", "w"))
            on.exit(close(file))
        }
    .Internal(cat(list(...), file, sep, fill, labels, append))
}

cat_with_hooks  <- function (..., file = "", sep = " ", fill = FALSE, labels = NULL, 
    append = FALSE) 
{
    if (is.character(file)) 
        if (file == "") 
            file <- stdout()
        else if (startsWith(file, "|")) {
            file <- pipe(substring(file, 2L), "w")
            on.exit(close(file))
        }
        else {
            file <- file(file, ifelse(append, "a", "w"))
            on.exit(close(file))
        }
    run_output_hooks()
    .Internal(cat(list(...), file, sep, fill, labels, append))
}

print_ <- getFromNamespace("print","base")

print_with_hooks <- function(x,...){
    run_output_hooks()
    if(any(class(x) %in% getOption("rkernel_displayed_classes")))
        display(x)
    else
        print_(x,...)
}

str_ <- getFromNamespace("str","utils")

str_with_hooks <- function(object,...){
    run_output_hooks()
    suspend_output_hooks()
    str_(object,...)
    restore_output_hooks()
}

message_ <- getFromNamespace("message","base")
message_stdout <- function (..., domain = NULL, appendLF = TRUE) 
{
    cond <- if (...length() == 1L && inherits(..1, "condition")) {
        if (nargs() > 1L) 
            warning("additional arguments ignored in message()")
        ..1
    }
    else {
        msg <- .makeMessage(..., domain = domain, appendLF = appendLF)
        call <- sys.call()
        simpleMessage(msg, call)
    }
    defaultHandler <- function(c) {
        cat_(conditionMessage(c), file = stdout(), sep = "")
    }
    withRestarts({
        signalCondition(cond)
        defaultHandler(cond)
    }, muffleMessage = function() NULL)
    invisible()
}

#' @export
install_output_hooks <- function() {
    replace_in_package("base","cat",cat_with_hooks)
    replace_in_package("base","print",print_with_hooks)
    replace_in_package("utils","str",str_with_hooks)
    replace_in_package("base","message",message_stdout)
    add_displayed_classes("iframe")
}

output_hooks <- new.env()
attr(output_hooks,"suspended") <- FALSE

#' @export
add_output_hook <- function(FUN,name,...){
    output_hooks[[name]] <- FUN
}

#' @export
remove_output_hook <- function(name){
    output_hooks[[name]] <- NULL
}

run_output_hooks <- function(...){
    nms <- sort(names(output_hooks))
    if(!isTRUE(attr(output_hooks,"suspended"))){
        for(n in nms){
            FUN <- output_hooks[[n]]
            FUN(...)
        }
    }
}

suspend_output_hooks <- function(){
    attr(output_hooks,"suspended") <- TRUE
}

restore_output_hooks <- function(){
    attr(output_hooks,"suspended") <- FALSE
}




# add_output_hook(function(...){
#     cat_("[Salut, vieux Jules!] ")
# },"ave")
