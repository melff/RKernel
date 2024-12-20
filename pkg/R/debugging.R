#' @include traitlets.R widget-button.R widget-output.R

dbgWidget <- function(name="dbgWidget",envir=parent.frame(),depth=NA){
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
    .monospace,
    .widget-text.monospace,
    .monospace input[type="text"],
    .widget-text.monospace input[type="text"] {
        font-family: monospace !important;
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
    b_details <- Button(description=name)
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

dbgConsole <- function(session, envname = ".GlobalEnv",
                       use_area=FALSE, clear=FALSE){

    output <- OutputWidget(append_output=TRUE,
                           use_display=TRUE)
    if(use_area){
        input <- Textarea(rows=5)
        input$add_class("monospace")
        run_btn <- Button(description="Run")

        run_on_click <- function(){
            if(clear) output$clear()
            else {
              output$stdout(add_prompts(input$value))
            }
            expr <- parse(text=input$value)
            output$clear()
            code <- input$value
            res <- session$run_cmd(code)
            if(length(res$stdout)) output$stdout(res$stdout)
            if(length(res$stderr)) output$stdout(res$stderr)
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
            if(clear) output$clear()
            else {
              output$stdout(add_prompts(input$value))
            }
            code <- input$value
            res <- try(session$run_cmd(code))
            if(length(res$stdout)) output$stdout(res$stdout)
            if(length(res$stderr)) output$stderr(res$stderr)
            invisible()
        }
        input$observe("value",run_on_value)
        VBox(input,output)
    }
}

fa_icon <- function(text) sprintf('<i class="fa fa-%s"></i>',text)

eval_capture <- function(expr,envir,enclos,stdout,stderr) {
    sout <- NULL
    serr <- NULL
    sout_con <- textConnection("sout","w",local=TRUE)
    serr_con <- textConnection("serr","w",local=TRUE)
    sink(serr_con,type="message")
    sink(sout_con,type="output")
    r <- withVisible(eval(expr,envir=envir,enclos=enclos))
    if(r$visible)
        print(r$value)
    sink(type="message")
    sink(type="output")
    close(serr_con)
    close(sout_con)
    if(length(sout)) {
      sout <- paste(c(sout,""), collapse="\n")
      stdout(sout)
    }
    if(length(serr)) {
      serr <- paste(c(serr,""), collapse="\n")
      stderr(serr)
    }
    # log_out(sout,use.print=TRUE)
}

add_prompts <- function(txt) {
  txt <- paste(txt,collapse="\n")
  txt <- unlist(strsplit(txt,"\n"))
  log_out(txt, use.print=TRUE)
  if(length(txt)) {
    txt[1] <- paste(">",txt[1])
    if(length(txt) > 1)
      txt[-1] <- paste("+",txt[-1])
  }
  txt <- paste(c(txt,""),collapse="\n")
  txt
}