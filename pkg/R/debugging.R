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

#' @export
Debugger <- function(dump=last.dump){
    widgets <- Map(dbgWidget,seq_along(last.dump),names(last.dump),last.dump)
    w <- do.call(VBoxClass$new,list(children=widgets))
    w$add_class("debugger-widget")
    w
}

dbgWidget <- function(depth,name,envir){
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
    #b_details <- Button(description=paste(sprintf("%3d:",depth),"[+]",name))
    b_details <- Button(description=paste(sprintf("%3d:",depth),name))
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
    console <- dbgConsole(envir=envir,
                          callback=refresh_details)
    console$add_class("no-display")
    console$add_class("debugger-console")
    # b_console <- Button(description=fa_icon("keyboard-o"))
    b_console <- Button(icon="keyboard-o")
    b_console$add_class(c("dbgwidget-button","monospace","margin-0",
                          "console-button"))
    toggle_console <- function(){
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
    buttons <- HBox(b_details,b_console)
    vb <- VBox(style,buttons,details,console)
    vb$add_class("debugger-subwidget")
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
   
