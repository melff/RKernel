#' @include traitlets.R widget-button.R widget-output.R

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
        env_browser = NULL,
        continue_loop = TRUE,
        show_prompt = FALSE,
        initialize = function(session) {
            self$session <- session
            self$style <- HTML(dbgConsole_style)
            self$output <- OutputWidget(append_output=TRUE,
                           use_display=TRUE)
            self$env_browser <- HTML("")
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
        in_browser = FALSE,
        browser_callback = function(prompt) {
            if(self$show_prompt) self$output$stdout(prompt)
            # log_out("browser_callback")
            self$in_browser <- TRUE
            return(TRUE)
        },
        prompt_callback = function(...) {
            self$continue_loop <- FALSE
            self$in_browser <- FALSE
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
            if(self$in_browser) self$update_env_browser()
            invisible()
        },
        update_env_browser = function() {
            # log_out("update_env_browser")
            title <- self$repl$eval(browserText(), safe = TRUE)
            if(!nzchar(title)) title <- NULL
            table <- env_browser_table(repl = self$repl, title = title)
            self$env_browser$value <- table
            # log_out("update_env_browser done")
        },
        on_click_next = function() {
            # log_out("on_click_next")
            self$repl$run_code("")
            if(self$in_browser) self$update_env_browser()
            invisible()
        },
        on_click_quit = function() {
            # log_out("on_click_quit")
            self$repl$run_code("Q")
            invisible()
        },
        main_widget = NULL,
        run = function(prompt) {
            kernel <- self$session$kernel
            use_area <- FALSE
            next_btn <- Button(description="Next")
            quit_btn <- Button(description="Quit")
            next_btn$on_click(self$on_click_next)
            quit_btn$on_click(self$on_click_quit)
            button_box <- HBox(next_btn,quit_btn)
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
                self$main_widget <- VBox(self$style,
                                         input_hbox,
                                         self$output,
                                         self$env_browser)
            }
            else {
                self$input <- TextWidget(
                    placeholder="Enter expression, 'c', 'n', or 'Q'",
                    continuous_update = TRUE
                )
                self$input$add_class("monospace")
                self$input$add_class("width-auto")
                self$input$on_submit(self$run_on_submit)
                self$main_widget <- VBox(self$style,
                                         self$output,
                                         self$input,
                                         self$env_browser,
                                         button_box)
            }
            self$update_env_browser()
            d <- display_data(self$main_widget)
            saved_parent <- kernel$save_shell_parent()
            kernel$display_send(d)
            if(self$show_prompt) self$output$stdout(prompt)
            while(self$continue_loop) {
                self$session$yield(1000)
            }
            self$input$disabled <- TRUE
            self$input$placeholder <- "--"
            close_btn <- Button(description="Close")
            self$main_widget$children <- list(self$output,
                                              self$env_browser,
                                              close_btn)
            d <- update(d,self$main_widget)
            d$data[["text/plain"]] <- "dbgConsole()"
            d$data[["text/html"]] <- "<pre>dbgConsole()</pre>"
            kernel$restore_shell_parent(saved_parent)
            kernel$display_send(d)
            on_click_close_btn <- function() { 
                # log_out("on_click_close_btn")
                self$main_widget$children <- list()
                d <- display_data(self$main_widget)
                d <- update(d,self$main_widget)
                d$data[["text/plain"]] <- "dbgConsole()"
                d$data[["text/html"]] <- "<pre>dbgConsole()</pre>"
                kernel$display_send(d)
            }
            close_btn$on_click(on_click_close_btn)
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
  # log_out(txt, use.print=TRUE)
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
        call_labels <- names(dump)
        ind <- request_menu_widget(call_labels,title="Select a frame")
        if(ind > 0) {
            title <- paste("Variables in frame of call",call_labels[ind])
            eval(substitute(browser(text=title),list(title=title)), 
                 envir=dump[[ind]])
        }
    } else {
        debugger_orig(dump)
    }
}

recover_ <- function() {
    # log_out("recover_")
    if(get_config("use_widgets")) {
        calls <- sys.calls()
        call_labels <- limitedLabels(calls)
        ind <- request_menu_widget(call_labels,title="Select a frame",
                                   file=stderr())
        if(ind > 0L) {
            title <- paste("Variables in frame of call",call_labels[ind])
            eval(substitute(browser(text=title),list(title=title)), 
                 envir = sys.frame(ind))
        }
    } else {
        recover_orig()
    }
}

install_debugging <- function() {
    replace_in_package("utils", "debugger", debugger_)
    replace_in_package("utils", "recover", recover_)
    add_sync_options(c(
        "rkernel_stop_on_error",
        "rkernel_show_traceback",
        "browser_show_prompt"))
}