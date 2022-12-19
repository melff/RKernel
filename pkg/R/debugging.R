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

        run <- function(){
            expr <- parse(text=input$value)
            output$clear_output()
            output$context$eval(expr)
            if(is.function(callback))
                callback()
            invisible()
        }
        run_btn$on_click(run)

        input_hbox <- HBox(input,run_btn)
        input_hbox$add_class("debug-box")
        VBox(input_hbox,output)
    } else {
        input <- TextWidget()
        input$add_class("monospace")
        input$add_class("width-auto")
        run <- function(tn,tlt,value){
            expr <- parse(text=input$value)
            output$clear_output()
            output$context$eval(expr)
            if(is.function(callback))
                callback()
            invisible()
        }
        input$observe("value",run)
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

#' @export
BreakPoint <- function(){
    envir <- parent.frame()
    eb <- envBrowser(envir=envir)
    display(eb)
    next_step <- TRUE
    repeat{
        input <- rkernel_readline()
        if(input == "c") {
            stream("Continuing ...")
            break
        }
        else if(input == "Q"){
            stream("Leaving ...")
            invokeRestart("exit")
        }
        else if(nzchar(trimws(input))) {
            expr <- parse(text=input)
            do_echo <- TRUE
            res <- withVisible(tryCatch(eval(expr,envir=envir),
                                        error=function(e){
                                            stream(conditionMessage(e),"stderr")
                                            return(invisible(NULL))
                                        }))
            if(do_echo && res$visible)
                print(res$value)
        }
        eb$refresh()
    }
}


# Unfortunately tracing does not work as I intended as long as there is no way to jump out of a function and 
BreakPoint_with_tracing <- function(){
    envir <- parent()
    the_call <- sys.call(-1)
    the_function <- sys.function(-1)
    expressions <- NULL
    deparsed <- NULL
    if(deparse(the_call[[1]])=="eval"){
        the_calls <- get.call.stack(drop.last=2)
        source_calls_idx <- find_calls(the_calls$calls,"source")
        trace_calls_idx <- find_calls(the_calls$calls,".doTrace")
        if(length(trace_calls_idx)>0) {
            warning("Step-wise debugging with 'trace' not (yet?) supported")
        }
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
    if(length(bp_position) > 0) {
        if(length(bp_position) > 1) stop("Multiple breakpoints not supported")
        to_eval <- tail(expressions,-bp_position)
        expressions <- expressions[-bp_position]
    }
    else stop("Breakpoint not found")
    deparsed <- lapply(expressions,deparse)       
    #print(deparsed)
    eb <- envBrowser(envir=envir)
    n_expressions <- length(expressions)
    n_to_eval <- length(to_eval)
    if(n_expressions > 0){
        w <- VBox()
        style <- HTML("<style>
.R-source.compact > .widget-html-content > pre {
   padding-top: 0px;
   padding-bottom: 0px;
   line-height: 1;
}
.R-source.active-code > .widget-html-content > pre {
   background-color: #f7f7f7;
}
.dbg-srcbox {
    padding: 0;
    margin: 2px;
    border: 1px solid #cfcfcf;
}

.output_area .dbg-srcbox pre {
    padding: 6px;
    line-height: 1.3;
}

.output_area .dbg-srcbox summary {
    padding: 6px;
    font-weight: bold;
    background-color: #f7f7f7;
    border-bottom: 1px solid #cfcfcf;
    line-height: 1.3;
}

.dbg-srcbox details summary::before {
    content: \"\\2BC8\";
}

.dbg-srcbox details[open] summary::before {
    content: \"\\2BC6\";
}

</style>")
        

        # for(i in 1:n_expressions){
        #     dep_i <- deparsed[[i]]
        #     dep_i <- paste(c("<pre>",dep_i,"</pre>"),collapse="\n")
        #     html_i <- HTML(dep_i)
        #     html_i$add_class("R-source")
        #     html_i$add_class("compact")
        #     if(i == bp_position) html_i$add_class("active-code")
        #     srcbox$children[[i]] <- html_i
        # }
        # #cat("bp_pos",bp_position)
        src_text <- c("<details>","<summary>"," Source code","</summary>","<pre>")
        for(i in 1:n_expressions){
             dep_i <- deparsed[[i]]
             dep_i <- paste(dep_i,collapse="\n")
             src_text <- c(src_text,dep_i)
        }
        src_text <- c(src_text,"</pre>","</details>")
        src_text <- paste(src_text,collapse="\n")
        srcbox <- HTML(src_text)
        w$children <- list(style,eb,srcbox)
        srcbox$add_class("dbg-srcbox")
    }
    else 
        w <- eb
    display(w)
    i <- 1
    j <- bp_position
    message(the_message)
    next_step <- TRUE
    repeat{
        do_echo <- FALSE
        do_step <- FALSE
        if(next_step && n_to_eval > 0 && j <= n_expressions){
            dep_j <- deparsed[[j]]
            prompt <- sprintf("%0d:",j)
            if((nn <- length(dep_j)) > 1){
                prompt[2:nn] <- paste(rep(" ",nchar(prompt)),collapse="")
            }
            dep_j <- paste(prompt,dep_j)
            stream(paste(dep_j,collapse="\n"))
            next_step <- FALSE
            do_step <- TRUE
        }
        input <- rkernel_readline()
        if(input == "c") {
            stream("Continuing ...")
            break
        }
        else if(input == "Q"){
            stream("Leaving ...")
            invokeRestart("exit")
        }
        else if(do_step && (input == "n" || trimws(input) == "")){
            expr <- expressions[[j]]
            j <- j + 1
            next_step <- TRUE
        }
        else if(nzchar(trimws(input))) {
            expr <- parse(text=input)
            do_echo <- TRUE
        }
        else {
            stream("Completed ...")
            break
        }
        res <- withVisible(tryCatch(eval(expr,envir=envir),
                            error=function(e){
                                stream(conditionMessage(e),"stderr")
                                return(invisible(NULL))
                            }))
        if(do_echo && res$visible)
            print(res$value)
        eb$refresh()
    }
}

Debug <- function(FUN){
    FUN <- substitute(FUN)
    if(!exists(FUN,mode="function")){
        stop(sprintf("Function '%s' not found",FUN))
    }
    FUNname <- deparse(FUN)
    FUN <- get(FUN)
    body(FUN) <- as.call(c(as.name("{"),c(call("BreakPoint"),as.list(body(FUN)[-1]))))
    assign(FUNname,FUN,DebugEnv())
}

DebugEnv <- function(){
    if(!("RKernel-helper:debug" %in% search())){
        e <- attach(NULL,name="RKernel-helper:debug")
        return(e)
    } else as.environment("RKernel-helper:debug")
}

Undebug <- function(FUN){
    FUN <- substitute(FUN)
    if(!exists(FUN,mode="function")){
        stop(sprintf("Function '%s' not found",FUN))
    }
    FUNname <- deparse(FUN)
    denv <- DebugEnv()
    msg <- sprintf("Function '%s' is not being debugged",FUNname)
    tryCatch(rm(list=FUNname,envir=denv),
             warning=function(w) warning(msg,call.=FALSE,immediate.=TRUE),
             error=function(e) warning(msg,call.=FALSE,immediate.=TRUE))
        
}
