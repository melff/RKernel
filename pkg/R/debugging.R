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

dbgConsole_style <- '<style>
.monospace,
.widget-text.monospace,
.monospace input[type="text"],
.widget-text.monospace input[type="text"] {
    font-family: monospace !important;
}
.debug-box .widget-textarea {
    width: 100%
}
button.console-button {
    width: 30px;
    text-align: center;
}
.width-auto,
.widget-text.widget-auto {
    width: auto;
}
.debugger-console {
    border: 1px solid #cfcfcf;
}
.debugger-console .widget-text input[type="text"] {
    border: 1px dotted #cfcfcf;
}
.debugger-console .invisible {
    display: none;
}
</style>'


dbgConsoleWidgetClass <- R6Class("dbgConsoleWidget",
    public = list(
        style = NULL,
        session = NULL,
        repl = NULL,
        input = NULL,
        output = NULL,
        continue_loop = TRUE,
        initialize = function(session) {
            self$session <- session
            self$output <- OutputWidget(append_output=TRUE,
                           use_display=TRUE)
            self$style <- HTML(dbgConsole_style)
            self$continue_loop <- TRUE
            self$repl <- RSessionAdapter$new(
                session = session,
                stdout_callback = self$output$stdout,
                stderr_callback = self$output$stderr,
                browser_callback = self$browser_callback,
                prompt_callback = self$prompt_callback,
                echo = TRUE
                )
        },
        browser_callback = function(prompt) {
            self$output$stdout(prompt)
            # log_out("browser_callback")
            return(TRUE)
        },
        prompt_callback = function(...) {
            self$continue_loop <- FALSE
            return(TRUE)
        },
        run_on_click = function(){
            code <- self$input$value
            status <- code_status(code)
            if(status == "complete") {
                try(self$repl$run_code(code, echo = TRUE))
                self$input$value <- ""
            }
            invisible()
        },
        run_on_submit = function(...){
            code <- self$input$value
            if(!length(code)) {
                code <- ""
            }
            r <- try(self$repl$run_code(code, echo = TRUE))
            if(inherits(r,"try-error")) {
                log_error(r)
                kernel <- self$session$kernel
                kernel$stderr(r)
            }
            self$input$clear()
            invisible()
        },
        main_widget = NULL,
        run = function(prompt) {
            kernel <- self$session$kernel
            use_area <- FALSE
            if(use_area) { # Currently not recommended - Textarea widgets do no
                           # work, only one line can be used ...
                self$input <- Textarea(
                    placeholder="Enter expression, 'c', 'n', or 'Q'"
                )
                self$input$add_class("monospace")
                run_btn <- Button(description="Run")
                run_btn$on_click(self$run_on_click)
                input_hbox <- HBox(self$input,run_btn)
                input_hbox$add_class("debug-box")
                self$main_widget <- VBox(self$style,input_hbox,self$output)
            }
            else {
                self$input <- TextWidget(
                    placeholder="Enter expression, 'c', 'n', or 'Q'",
                    continuous_update = TRUE
                )
                self$input$add_class("monospace")
                self$input$add_class("width-auto")
                self$input$on_submit(self$run_on_submit)
                self$main_widget <- VBox(self$style,self$output,self$input)
            }
            d <- display_data(self$main_widget)
            saved_parent <- kernel$save_shell_parent()
            kernel$display_send(d)
            self$output$stdout(prompt)
            while(self$continue_loop) {
                self$session$yield(1000)
            }
            self$input$disabled <- TRUE
            self$input$placeholder <- "--"
            self$input$add_class("invisible")
            self$main_widget <- VBox(self$output)
            d <- update(d,self$main_widget)
            d$data[["text/plain"]] <- "dbgConsole()"
            d$data[["text/html"]] <- "<pre>dbgConsole()</pre>"
            log_out(d,use.str=TRUE)
            kernel$restore_shell_parent(saved_parent)
            kernel$display_send(d)
        }
    )
)

dbgSimpleConsoleClass <- R6Class("dbgSimpleConsole",
    public = list(
        session = NULL,
        repl = NULL,
        continue_loop = TRUE,
        initialize = function(session) {
            kernel <- session$kernel
            self$session <- session
            self$continue_loop <- TRUE
            self$repl <- RSessionAdapter$new(
                session = session,
                stdout_callback = kernel$stdout,
                stderr_callback = kernel$stderr,
                browser_callback = self$browser_callback,
                prompt_callback = self$prompt_callback,
                echo = TRUE
                )
        },
        browser_callback = function(prompt) {
            # log_out("browser_callback")
            return(TRUE)
        },
        prompt_callback = function(...) {
            self$continue_loop <- FALSE
            return(TRUE)
        },
        run = function(prompt) {
            kernel <- self$session$kernel
            while(self$continue_loop) {
                kernel$input_request(prompt = prompt)
                input <- kernel$read_stdin()
                r <- try(self$repl$run_code(input, echo = TRUE))
                if(inherits(r,"try-error")) {
                    kernel$stderr(unclass(r))
                }
            }
        }
    )
)

debugging_state <- new.env()
debugging_state$depth <- 0L

dbgConsole <- function(session, prompt, use_widgets = TRUE) {
    debugging_state$depth <- debugging_state$depth + 1L
    # log_out("dbgConsole")
    # log_out(sprintf("Depth: %d",debugging_state$depth))
    # log_out(prompt)
    # log_out(use_widgets)
    if(use_widgets) {
        # log_out("creating a dbgConsoleWidgetClass object")
        cons <- dbgConsoleWidgetClass$new(session)
    }
    else {
        # log_out("creating a dbgSimpleConsoleClass object")
        cons <- dbgSimpleConsoleClass$new(session)
    }
    cons$run(prompt)
    debugging_state$depth <- debugging_state$depth - 1L
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

debugger_orig <- getFromNamespace("debugger","utils")
recover_orig <- getFromNamespace("recover","utils")


debugger_ <- function(dump = last.dump) {
    debugger_look <- function(.index) { #adapted from utils::debugger
        .this_dump <- dump[[.index]]
        for (.thing in ls(envir = .this_dump, all.names = TRUE)) {
            tryCatch(assign(.thing, get(.thing, envir = .this_dump)), 
                error = function(e) {})
        }
        rm(.thing, .index, .this_dump)
        eval(substitute(browser()),envir=dump[[.index]])
    }
    if(get_config("use_widgets")) {
        if (!inherits(dump, "dump.frames")) { #adapted from utils::debugger
        cat(gettextf("'dump' is not an object of class %s\n", 
            dQuote("dump.frames")))
        return(invisible())
        }
        n <- length(dump)
        if (!n) {
            cat(gettextf("'dump' is empty\n"))
            return(invisible())
        }
        err.action <- getOption("error")
        on.exit(options(error = err.action))   
        err_msg <- attr(dump, "error.message")
        calls <- names(dump)
        ind <- request_menu_widget(calls,title="Select a frame")
        if(ind > 0) eval(substitute(browser()),envir=dump[[ind]])
    } else {
        debugger_orig(dump)
    }
}

recover_ <- function() {
    log_out("recover_")
    if(get_config("use_widgets")) {
        calls <- sys.calls()
        call_labels <- limitedLabels(calls)
        ind <- request_menu_widget(call_labels,title="Select a frame")
        if(ind > 0L) {
            eval(substitute(browser()), envir = sys.frame(ind))
        }
    } else {
        recover_orig()
    }
}

install_debugging <- function() {
    replace_in_package("utils", "debugger", debugger_)
    replace_in_package("utils", "recover", recover_)
}