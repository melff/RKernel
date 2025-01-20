
print_orig <- getFromNamespace("print","base")

RKernel_print <- function(x,...) {
    if(any(class(x) %in% getOption("rkernel_displayed_classes")))
        display(x)
    else
        print_orig(x,...) # Original 'print' from package "base"
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
        cat(conditionMessage(c), file = stdout(), sep = "")
    }
    withRestarts({
        signalCondition(cond)
        defaultHandler(cond)
    }, muffleMessage = function() NULL)
    invisible()
}

#' @include View.R
#' @export
install_output_hooks <- function() {
    replace_in_package("base","print",RKernel_print)
    replace_in_package("utils","View",View)    
    replace_in_package("base","message",message_stdout)
    add_displayed_classes("iframe")
    add_displayed_classes("html_elem")
    add_displayed_classes("shiny.tag")
    add_displayed_classes("shiny.tag.list")
    add_displayed_classes("htmlTable")
    add_displayed_classes("tableHTML")
    add_displayed_classes("dataTable")
    add_displayed_classes("htmlwidget")
    add_displayed_classes("svg")
}

