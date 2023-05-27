#' @include evaluator.R

#' @importFrom utils head tail limitedLabels

#' @describeIn Debugger An adaptation of the function with the same name from
#'   the 'utils' package
#'
#' @param dumpto Character string; name of the object or file that contains the dumped frames
#' @param to.file Logical; whether to dump to a file.
#' @param include.GlobalEnv Logical; see \code{\link[utils]{dump.frames}}
#' @param drop.kernel.frames Logical; whether frames belonging to the R Kernel should be dropped
#' @param drop.last Integer; how many of the last frames should be dropped (which are specific to the
#'            RKernel evaluator)
#' 
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
    if(drop.kernel.frames){
        depth <- which(sapply(last.dump,identical,.GlobalEnv))
        depth <- min(depth)
    }
    else
        depth <- 0
    names(last.dump) <- limitedLabels(calls)
    if(depth > 0)
        last.dump <- tail(last.dump,-depth)
    last.dump <- head(last.dump,-drop.last)
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
register_export(dump.frames)

#' @importFrom utils head tail limitedLabels

return.frames <- function(drop.kernel.frames = TRUE,drop.last=0){
    calls <- sys.calls()
    dump <- sys.frames()
    names(dump) <- limitedLabels(calls)
    if(drop.kernel.frames){
        depth <- which(sapply(dump,identical,.GlobalEnv))
        depth <- min(depth)
    }
    else
        depth <- 0
    names(dump) <- limitedLabels(calls)
    if(depth > 0)
        dump <- tail(dump,-depth)
    if(drop.last > 0)
       dump <- head(dump,-drop.last)
    class(dump) <- "dump.frames"
    return(dump)
}

#' @importFrom utils head tail

get.call.stack <- function(drop.kernel.frames = TRUE,drop.last=0){
    status <- sys.status()
    calls <- status$sys.calls
    frames <- status$sys.frames
    if(drop.kernel.frames){
        depth <- which(sapply(frames,identical,.GlobalEnv))
        depth <- min(depth)
    }
    else
        depth <- 0
    if(depth > 0)
        calls <- tail(calls,-depth)
    if(drop.last > 0)
       calls <- head(calls,-drop.last)
    if(depth > 0)
        frames <- tail(frames,-depth)
    if(drop.last > 0)
       frames <- head(frames,-drop.last)
    return(list(calls=calls,frames=frames))
}

#' @title Interactive Debugger
#'
#' @description Provides a Jupyter notebook compatible variant of
#'     \code{\link[utils]{debugger}}
#' @param dump An R dump object created by \code{dump.frames}
#' @export
Debugger <- function(dump=last.dump){
    widgets <- Map(dbgWidget,names(dump),dump,seq_along(dump))
    if(length(widgets) > 1){
        w <- do.call(VBoxClass$new,list(children=widgets))
        w$add_class("debugger-widget")
        return(w)
    }
    else return(widgets[[1]])
}

dbgWidget <- function(name,envir,depth=NA){
    style <- HTML('<style>
    button.dbgwidget-button {
      width: auto;
      height: auto;
      background: unset;
      line-height: 15px;
      text-align: left;
      border: 1px solid #cfcfcf;
      padding: 6px;
    }
    button.dbgwidget-button.dbgwidget-button-expanded {
      border-bottom-style: none;
    }
    .monospace {
        font-family: monospace;
    }
    .margin-0 {
        margin: 0;
    }
    .debugger-subwidget ~ .debugger-subwidget {
        margin-top: -1px;
    }
    .debugger-subwidget > .widget-html {
        margin-top: 0;
        margin-bottom: 0;
    }
    .widget-checkbox.nolabel label.widget-label {
        display: none;
    }
    .debug-box .widget-textarea {
        width: 100%
    }
    .no-display {
        display: none;
    }
    button.details-button {
        width: calc(100% - 30px);
    }
    button.console-button {
        width: 30px;
        text-align: center;
    }
    .width-auto,
    .widget-text.widget-auto {
        width: auto;
    }
    button.dbgwidget-button-collapsed::before {
        content: "\\2BC8";
    }
    button.dbgwidget-button-expanded::before {
        content: "\\2BC6";
    }
    .debugger-console {
        border: 1px solid #cfcfcf;
    }
    .debugger-console .widget-text input[type="text"] {
        border: 1px dotted #cfcfcf;
    }
    </style>')
    label <- if(is.finite(depth)) paste(sprintf("%3d:",depth),name) else name
    b_details <- Button(description=label)
    b_details$add_class(c("dbgwidget-button","monospace","margin-0",
                          "details-button"))
    details <- HTML("")
    details$add_class("margin-0")
    toggle_details <- function(){
        if(details$value==""){
            table <- env_browser_table(envir=envir,
                                       all.names=TRUE,
                                       mode="any",
                                       include_css=TRUE)
            text_html <- paste(
                    table,
                    sep="\n")
            details$value <- text_html
            #b_details$description <- paste(sprintf("%3d:",depth),"[-]",name)
            b_details$add_class("dbgwidget-button-expanded")
            b_details$remove_class("dbgwidget-button-collapsed")
        } else {
            details$value <- ""
           # b_details$description <- paste(sprintf("%3d:",depth),"[+]",name)
            b_details$remove_class("dbgwidget-button-expanded")
            b_details$add_class("dbgwidget-button-collapsed")
        }
    }
    b_details$add_class("dbgwidget-button-collapsed")
    b_details$on_click(toggle_details)
    refresh_details <- function() {
        if(details$value!=""){
            table <- env_browser_table(envir=envir,
                                       all.names=TRUE,
                                       mode="any",
                                       include_css=TRUE)
            text_html <- paste(
                    table,
                    sep="\n")
            details$value <- text_html
        }
    }
    # VBox(style,b_details,details)
    b_console <- Button(icon="keyboard-o")
    b_console$add_class(c("dbgwidget-button","monospace","margin-0",
                          "console-button"))
    buttons <- HBox(b_details,b_console)
    vb <- VBox(style,buttons,details)
    vb$add_class("debugger-subwidget")
    # b_console <- Button(description=fa_icon("keyboard-o"))
    toggle_console <- function(){
        if(length(vb$children) < 4){
            console <- dbgConsole(envir=envir,
                          callback=refresh_details)
            console$add_class("no-display")
            console$add_class("debugger-console")
            vb$children[[4]] <- console
        }
        else {
            console <- vb$children[[4]] 
        }
        if(console$has_class("no-display")){
            console$remove_class("no-display")
            b_console$icon <- "close"
        }
        else{
            console$add_class("no-display")
            b_console$icon <- "keyboard-o"
        }
    }
    b_console$on_click(toggle_console)
    vb
}

dbgConsole <- function(envir,callback=NULL,
                       use_area=FALSE){

    output <- OutputWidget(append_output=TRUE,
                           envir=envir,
                           use_display=TRUE)
    if(use_area){
        input <- Textarea(rows=5)
        input$add_class("monospace")
        run_btn <- Button(description="Run")

        run_on_click <- function(){
            expr <- parse(text=input$value)
            output$clear_output()
            output$context$eval(expr)
            if(is.function(callback))
                callback()
            invisible()
        }
        run_btn$on_click(run_on_click)

        input_hbox <- HBox(input,run_btn)
        input_hbox$add_class("debug-box")
        VBox(input_hbox,output)
    } else {
        input <- TextWidget()
        input$add_class("monospace")
        input$add_class("width-auto")
        run_on_value <- function(tn,tlt,value){
            expr <- parse(text=input$value)
            output$clear_output()
            output$context$eval(expr)
            if(is.function(callback))
                callback()
            invisible()
        }
        input$observe("value",run_on_value)
        VBox(input,output)
    }
}

fa_icon <- function(text) sprintf('<i class="fa fa-%s"></i>',text)
   
get_expressions <- function(fun){
    the_body <- body(fun)
    if(deparse(the_body[[1]])!="{") return(list(the_body))
    as.list(the_body[-1])
}

find_calls <- function(expressions,name){
    calls <- lapply(expressions,"[[",1)
    calls <- sapply(calls,deparse)
    which(calls == name)
}

find_call <- function(expressions,name){
    if(length(expressions)<1) return(0)
    for(i in seq_along(expressions)){
        expr <- as.list(expressions[[i]])
        if(length(expr) > 0 && deparse(expr[[1]])==name)
            return(i)
    }
}

rkernel_readline <- function(){
    evaluator$current$readline()
}

rkernel_stream <- function(text,
                           stream=c("stdout","stderr")){
    evaluator$current$stream(text,stream=stream)
}

add_prompts <- function(text){
    prompts <- rep("+",length(text))
    prompts[1] <- ">"
    paste(prompts,text)
}

clone_env <- function(old_env){
    new_env <- new.env(parent=parent.env(old_env))
    nms <- ls(old_env)
    for(n in nms)
        delayedAssign(n,get(n,old_env),eval.env=old_env,assign.env=new_env)
    new_env
}


#' @title Breakpoints
#'
#' @description The function \code{BreakPoint} can be used to set a breakpoint in
#'   a function or a script. Instead of a terminal input and output, the thus
#'   created breakpoint can be interacted with using a Widget.
#' @export
BreakPoint <- function(){
    parent <- parent.frame()
    envir <- new.env(parent=parent)
    the_call <- sys.call(-1)
    the_function <- sys.function(-1)
    expressions <- NULL
    deparsed <- NULL
    if(deparse(the_call[[1]])=="eval"){
        the_calls <- get.call.stack(drop.last=2)
        source_calls_idx <- find_calls(the_calls$calls,"source")
        trace_calls_idx <- find_calls(the_calls$calls,".doTrace")
        if(length(source_calls_idx)){
            i <- max(source_calls_idx)
            source_call <- the_calls$calls[[i]]
            source_frame <- the_calls$frames[[i]]
            expressions <- as.list(source_frame$exprs)
            srcfile <- source_frame$srcfile
            the_message <- sprintf("Breakpoint in file \"%s\"",srcfile)
         }
         else
            the_message <- "Breakpoint not in a function"
        in_function <- FALSE
    }
    else {
        the_message <- sprintf("Breakpoint in %s",deparse(the_call))
        expressions <- get_expressions(the_function)
        in_function <- TRUE
    }
    bp_position <- find_call(expressions,"BreakPoint")
    #print(deparsed)
    eb <- envBrowser(envir=envir,parent=parent)
    display(eb)
    message(the_message)
    while(TRUE){
        rkernel_stream("\n")
        input <- rkernel_readline()
        input <- trimws(input)
        if(input == "c" || !nzchar(input)) {
            rkernel_stream("Continuing ...")
            break
        }
        else if(input == "Q"){
            rkernel_stream("Leaving ...")
            invokeRestart("exit")
        }
        else if(nzchar(trimws(input))){
            expr <- parse(text=input)
            res <- withVisible(tryCatch(eval(expr,envir=envir,enclos=parent),
                                        error=function(e){
                                            rkernel_stream(conditionMessage(e),"stderr")
                                            return(invisible(NULL))
                                        }))
            eb$refresh()
            if(res$visible)
                rkernel_stream(capture.output(print(res$value)))
        }
    }
}

tracers <- new.env()

Tracer <- R6Class("Tracer",
    public = list(
        envir = NULL,
        expressions = NULL,
        label = NULL,
        w = NULL,
        eb = NULL,
        i = 1,
        n = 0,
        complete = FALSE,
        use_sandbox = TRUE,
        sandbox = NULL,
        style = "
.R-source.compact > .widget-html-content > pre {
   padding-top: 0px;
   padding-bottom: 0px;
   line-height: 1;
}
.R-source.active-code > .widget-html-content > pre {
   background-color: #f7f7f7;
}
.tracer-srcbox {
    padding: 0;
    margin: 2px;
    border: 1px solid #cfcfcf;
}

.tracer-srcbox pre,
.output_area .tracer-srcbox pre {
    padding: 6px;
    line-height: 1.3;
}

.tracer-srcbox details summary {
    display: block;
}

.tracer-srcbox summary,
.output_area .tracer-srcbox summary {
    padding: 6px;
    font-weight: bold;
    background-color: #f7f7f7;
    border-bottom: 1px solid #cfcfcf;
    line-height: 1.3;
}

.tracer-srcbox details summary::before {
    content: \"\\2BC8\";
}

.tracer-srcbox details[open] summary::before {
    content: \"\\2BC6\";
}
",
    initialize = function(envir,label,expressions,
                      src,use_sandbox=getOption("trace_use_sandbox",TRUE)){
            eb <- envBrowser(envir=envir)
            self$eb <- eb
            self$label <- label
            self$expressions <- expressions
            self$n <- length(expressions)
            self$envir <- envir
            self$use_sandbox <- use_sandbox
            if(use_sandbox){
                self$sandbox <- new.env(parent=envir) 
            }
            if(self$n > 1){
                w <- VBox()
                style <- HTML(paste0("<style>",self$style,"</style>"))
                src_text <- c("<details>",
                              "<summary>",
                              " Source code",
                              "</summary>",
                              "<pre>",
                              src)
                src_text <- c(src_text,"</pre>","</details>")
                src_text <- paste(src_text,collapse="\n")
                srcbox <- HTML(src_text)
                w$children <- list(style,eb,srcbox)
                srcbox$add_class("tracer-srcbox")
                self$w <- w
            }
            else {
                self$w <- self$eb
            }
            
        },
        display = function(){
            display(self$w)
        },
        step = function(i){
            if(!self$complete) {
                if(missing(i))
                    self$i <- self$i + 1
                else
                    self$i <- i + 1
                if(self$i <= self$n){
                    expr <- self$expressions[[self$i]]
                    dep_expr <- deparse(expr)
                    dep_expr <- paste(dep_expr,collapse="\n")
                    dep_expr <- paste0(dep_expr,"\t")
                    rkernel_stream(dep_expr)
                }
                self$eb$refresh()
                while(TRUE){
                    input <- rkernel_readline()
                    if(input == "c") {
                        self$complete <- TRUE
                        break
                    }
                    else if(input == "Q"){
                        invokeRestart("exit")
                    } else if(input == "n" || trimws(input) == ""){
                        break
                    }
                    else {
                        expr <- parse(text=input)
                        if(self$use_sandbox)
                            envir <- self$sandbox
                        else
                            envir <- self$envir
                        res <- withVisible(tryCatch(eval(expr,
                                                         envir=envir),
                                                    error=function(e){
                                                        rkernel_stream(conditionMessage(e),"stderr")
                                                        return(invisible(NULL))
                                                    }))
                        self$eb$refresh()
                        if(res$visible)
                            rkernel_stream(paste0(capture.output(print(res$value)),
                                          "\t"))
                    }
                }
            }
        },
        finalize = function(){
            self$envir <- NULL
            self$eb <- NULL
        }
    )
)

tracer <- function(...) {
    e <- parent.frame()
    cl <- deparse(sys.call(sys.parent()))
    #cat(cl)
    if(!(cl %in% names(tracers))){
        #cat("not found")
        fun <- sys.function(sys.parent())
        fun_orig <- fun@original
        fun_src <- deparse(fun_orig)
        expressions <- as.list(body(fun_orig))
        trc <- Tracer$new(envir=e,
                          label=cl,
                          expressions=expressions,
                          src=fun_src)
        tracers[[cl]] <- trc
        trc$display()
    }
    else{
        #cat("found")
        trc <- tracers[[cl]]
        trc$step()
    }
}
register_export(tracer)

exit_tracer <- function(...) {
    cl <- deparse(sys.call(sys.parent()))
    #cat(cl)
    if(cl %in% names(tracers)){
        trc <- tracers[[cl]]
        trc$finalize()
        rm(list=cl,envir=tracers)
    }    
}
register_export(exit_tracer)


#' @title Step through a function
#'
#' @description The function \code{Trace} markes a function to be traced
#'   so that it will be evaluated step-by-step when called.
#'
#' @details \code{Trace} differs
#'   in semantics from the function \code{\link[base]{trace}} which
#'   allows to insert expressions at given positions within a function.
#'   Nevertheless, it uses the function \code{\link[base]{trace}} internally.
#'   Therefore the effect of \code{Trace(FUN)} can be undone by the call
#'   \code{unrace(FUN)}
#'
#' @param FUN the function to be traced.
#' @export
Trace <- function(FUN){
     cls <- class(FUN)
     if("functionWithTrace" %in%cls){
         warning("Function is already being traced")
         return(invisible(NULL))
     }
         
     bd <- as.list(body(FUN))
     trace_at <- seq_along(bd)
     FUN <- substitute(FUN)
     tryCatch(trace(FUN,
                    tracer=tracer,
                    exit=exit_tracer,
                    at=trace_at,print=FALSE),
              error=function(e)stop("Is the function already being traced?"))
}


src_tracer_init <- function(filename,expressions){
    src <- readLines(filename)
    parent <- parent.frame()
    trc <- Tracer$new(envir=parent,
                      label=filename,
                      expressions=expressions,
                      src=src)
    tracers[[filename]] <- trc
    trc$display()
}
register_export(src_tracer_init)

src_tracer_step <- function(filename,i){
    trc <- tracers[[filename]]
    trc$step()
}
register_export(src_tracer_step)

src_tracer_exit <- function(filename){
    trc <- tracers[[filename]]
    trc$finalize()
    rm(list=filename,envir=tracers)
}
register_export(src_tracer_exit)

#' @title Step through a scripted being sourced
#'
#' @description The function parses the contents of a script
#'   and evaluates each expression within it step-by-step.
#'
#' @param filename A string; the name of the script file.
#' @export
tracing_source <- function(filename){
    parsed <- parse(filename)
    parsed <- c(quote(invisible()),parsed)
    n <- length(parsed)
    expr <- expression()
    for(i in 1:n){
        parsed_i <- parsed[[i]]
        if(i==1)
        expr_i <- call("src_tracer_init",filename,parsed)
        else
            expr_i <- call("src_tracer_step",filename,i)
        expr_i <- call(".doTrace",expr_i)
        expr_i <- call("{",expr_i,parsed_i)
        expr[[i]] <- expr_i
    }
    expr_i <- call("src_tracer_exit",filename)
    expr_i <- call(".doTrace",expr_i)
    expr_i <- call("{",expr_i,quote(invisible()))
    expr[[n+1]] <- expr_i
    for(i in seq_along(expr)){
        expr_i <- expr[[i]]
        res <- withVisible(eval.parent(expr_i))
        if(res$visible)
            rkernel_stream(paste0(capture.output(print(res$value)),
                          "\t"))
    }
}
