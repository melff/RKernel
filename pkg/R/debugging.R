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
        kernel = NULL,
        repl = NULL,
        runner = NULL,
        input = NULL,
        output = NULL,
        env_browser = NULL,
        button_box = NULL,
        continue_loop = TRUE,
        show_prompt = TRUE,
        initialize = function(runner) {
            self$runner <- runner
            session <- runner$session
            self$session <- session
            self$style <- HTML(dbgConsole_style)
            self$kernel <- runner$kernel
            self$env_browser <- HTML("")
            self$continue_loop <- TRUE

            self$repl <- RSessionAdapter$new(
                session = session,
                stdout_callback = self$stdout_callback,
                stderr_callback = self$stderr_callback,
                browser_callback = self$browser_callback,
                prompt_callback = self$prompt_callback,
                echo = TRUE
                )
        },
        parent_msg = NULL,
        stdout_callback = function(text) {
            saved_parent <- self$kernel$save_shell_parent()
            runner_parent <- self$runner$get_shell_parent()
            self$kernel$restore_shell_parent(runner_parent)
            self$runner$handle_stdout(text)
            self$kernel$restore_shell_parent(saved_parent)
        },
        stderr_callback = function(text) {
            saved_parent <- self$kernel$save_shell_parent()
            runner_parent <- self$runner$get_shell_parent()
            self$kernel$restore_shell_parent(runner_parent)
            self$runner$handle_stderr(text)
            self$kernel$restore_shell_parent(saved_parent)
        },
        in_browser = FALSE,
        bprompt = character(0),
        browser_callback = function(prompt) {
            # log_out("dbgConsoleWidgetClass$browser_callback")
            if(self$show_prompt) self$stdout_callback(prompt)
            self$in_browser <- TRUE
            self$bprompt <- prompt
        },
        prompt_callback = function(...) {
            # log_out("dbgConsoleWidgetClass$prompt_callback")
            self$continue_loop <- FALSE
            self$in_browser <- FALSE
        },
        handle_input = function(txt) {
            # log_out("handle_input")
            if(!length(txt)) {
                txt <- ""
            }
            # log_out(sprintf("txt='%s'",txt))
            r <- try(self$repl$run_code(txt, echo = TRUE,
                                            until_prompt = FALSE))
            if(inherits(r,"try-error")) {
                log_error(r)
                kernel <- self$session$kernel
                kernel$stderr(unclass(r))
            }
            self$input$clear()
        },
        run_on_submit = function(...){
            txt <- self$input$value
            self$handle_input(txt)
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
            self$handle_input("n")
            if(self$in_browser) self$update_env_browser()
            invisible()
        },
        on_click_continue = function() {
            # log_out("on_click_continue")
            self$handle_input("c")
            if(self$in_browser) self$update_env_browser()
            invisible()
        },
        on_click_quit = function() {
            # log_out("on_click_quit")
            self$handle_input("Q")
            invisible()
        },
        main_widget = NULL,
        display_id = NULL,
        display = function() {
            # log_out("dbgConsoleWidget$display()")
            kernel <- self$session$kernel
            next_btn <- Button(icon="play",style=ButtonStyle(font_size="70%"))
            continue_btn <- Button(icon="forward",style=ButtonStyle(font_size="70%"))
            quit_btn <- Button(icon="stop",style=ButtonStyle(font_size="70%"))
            next_btn$on_click(self$on_click_next)
            continue_btn$on_click(self$on_click_continue)
            quit_btn$on_click(self$on_click_quit)
            self$button_box <- HBox(next_btn,continue_btn,quit_btn)
            self$input <- TextWidget(
                placeholder="Enter expression, a debugging command, or 'help' for available commands ",
                continuous_update = TRUE
            )
            self$input$add_class("monospace")
            self$input$add_class("width-auto")
            self$input$on_submit(self$run_on_submit)
            if(length(self$button_box)) {
                self$main_widget <- VBox(self$style,
                                        self$button_box,
                                        self$input,
                                        self$env_browser)
            }
            else {
                self$main_widget <- VBox(self$style,
                                        self$input,
                                        self$env_browser)
            }
            self$update_env_browser()
            # log_out("display_data(self$main_widget)")
            d <- display_data(self$main_widget)
            self$display_id <- display_id(d)
            # log_out(display_id(d))
            saved_parent <- kernel$save_shell_parent()
            runner_parent <- self$runner$get_shell_parent()
            kernel$restore_shell_parent(runner_parent)
            kernel$display_send(d)
            kernel$restore_shell_parent(saved_parent)
        },
        run = function(prompt) {
            # log_out("dbgConsole$run()")
            if(self$show_prompt) self$stdout_callback(prompt)
            repeat {
                self$session$yield(10000)
                if(!self$continue_loop) {
                    break
                }
            }
            d <- display_data(self$main_widget)
            # log_out("loop ended")
            # log_out(display_id(d))
            d <- display_data("text/plain"="",
                              "text/html"="",
                              id = self$display_id,
                              update = TRUE)
            # log_out(display_id(d))
            kernel <- self$session$kernel
            runner_parent <- self$runner$get_shell_parent()
            kernel$restore_shell_parent(runner_parent)
            kernel$display_send(d)
            # log_out("dbgConsole$run - finished")
        }
    )
)

dbgSimpleConsoleClass <- R6Class("dbgSimpleConsole",
    public = list(
        runner = NULL,
        repl = NULL,
        continue_loop = TRUE,
        initialize = function(runner) {
            self$runner <- runner
            self$continue_loop <- TRUE
            self$repl <- RSessionAdapter$new(
                session = runner$session,
                stdout_callback = runner$handle_stdout,
                stderr_callback = runner$handle_stderr,
                browser_callback = self$browser_callback,
                prompt_callback = self$prompt_callback,
                input_callback = runner$readline,
                echo = TRUE
                )
        },
        browser_callback = function(prompt) {
            # log_out("dbgSimpleConsoleClass: browser_callback")
            # log_out(sprintf("prompt = '%s'",prompt))
            self$bprompt <- prompt
        },
        prompt_callback = function(...) {
            # log_out("dbgSimpleConsoleClass: prompt_callback")
            self$continue_loop <- FALSE
        },
        bprompt = character(),
        run = function(prompt) {
            kernel <- self$runner$kernel
            kernel$stdout("\n")
            self$bprompt <- prompt
            while(self$continue_loop) {
                kernel$input_request(prompt = self$bprompt)
                input <- kernel$read_stdin()
                self$repl$run_code(input, echo = TRUE,
                                   until_prompt = FALSE)
            }
        }
    )
)

dbgConsole <- function(runner, prompt, use_widgets = TRUE) {
    # log_out("dbgConsole")
    # log_out("dbgConsole")
    # log_out(prompt)
    # log_out(use_widgets)
    if(use_widgets) {
        # log_out("creating a dbgConsoleWidgetClass object")
        cons <- dbgConsoleWidgetClass$new(runner)
        cons$display()
    }
    else {
        # log_out("creating a dbgSimpleConsoleClass object")
        cons <- dbgSimpleConsoleClass$new(runner)
    }
    cons$run(prompt)
    # log_out("dbConsole done")
    return(TRUE)
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
        browser()
    }
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
    repeat {
        if(get_config("use_widgets")) {
            ind <- request_menu_widget(call_labels,
                                        title="Enter an environment or quit",
                                        buttons = c("Select","Quit"))
        }
        else {
            ind <- menu(title=gettext("Enter an environment number, or 0 to exit"),
                        choices = call_labels)
        }
        if(ind > 0) {
            debugger_look(ind)
        }
        else break
    } 
}

#' @importFrom utils head limitedLabels
recover_ <- function() {
    # log_out("======= recover_ ===========")
    if(get_config("use_widgets")) {
        calls <- sys.calls()
        call_labels <- limitedLabels(head(calls,-1))
        recover_look <- function(index, title)  {
            browser_call <- substitute(browser(text=title),list(title=title))
            eval(browser_call,sys.frame(index))
        }
        repeat {
            ind <- request_menu_widget(call_labels,
                                    title = "Enter an environment or quit",
                                    buttons = c("Select","Quit"),
                                    file=stderr())
            # log_out(sprintf("ind = %d",ind))
            if(ind > 0L) {
                title <- paste("Variables in frame of call",call_labels[ind])
                recover_look(ind,title)
                # log_out("returned from 'recover_look()'")
            } 
            else break
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

CellTracer <- R6Class("CellTracer",
    public = list(
        runner = NULL,
        initialize = function(runner) {
            self$next_btn <- Button(icon="play",style=ButtonStyle(font_size="70%"))
            self$continue_btn <- Button(icon="forward",style=ButtonStyle(font_size="70%"))
            self$quit_btn <- Button(icon="stop",style=ButtonStyle(font_size="70%"))
            self$button_box <- HBox(self$next_btn,
                                    self$continue_btn,
                                    self$quit_btn)
            self$runner <- runner
            self$next_btn$on_click(self$on_next_btn)
            self$continue_btn$on_click(self$on_continue_btn)
            self$quit_btn$on_click(self$on_quit_btn)
        },
        continue_loop = TRUE,
        line_no = 1,
        code_lines = character(0),
        run = function(code_lines) {
            runner <- self$runner
            session <- runner$session
            kernel <- runner$kernel
            shell_parent <- kernel$get_shell
            d <- display_data(self$button_box)
            runner$display_send(d)
            self$code_lines <- code_lines
            self$continue_loop <- TRUE
            save_parent <- kernel$save_shell_parent()
            while(self$continue_loop) {
                session$yield(1000)
                if(kernel$errored && kernel$stop_on_error) break
            }
            d <- update.display.data(d,"text/plain"="",
                          "text/html"="")
            kernel$restore_shell_parent(save_parent)
            kernel$display_send(d)
            self$line_no <- 1
        },
        next_btn = NULL,
        continue_btn = NULL,
        quit_btn = NULL,
        button_box = NULL,
        on_next_btn = function() {
            line <- self$code_lines[self$line_no]
            n_lines <- length(self$code_lines)
            self$run_(line)
            self$line_no <- self$line_no + 1
            if(self$line_no > n_lines) self$continue_loop <- FALSE
        },
        on_continue_btn = function() {
            line <- self$code_lines[self$line_no]
            n_lines <- length(self$code_lines)
            ii <- seq(from=self$line_no,to=n_lines)
            lines <- self$code_lines[ii]
            self$run_(lines)
            self$continue_loop <- FALSE
        },
        on_quit_btn = function() {
            self$continue_loop <- FALSE
        },
        run_ = function(lines) {
            runner <- self$runner
            session <- runner$session
            kernel <- runner$kernel
            repl <- runner$repl
            parent_save <- kernel$save_shell_parent()
            kernel$restore_shell_parent(parent_save)
            repl$run_code( 
                            lines, 
                            io_timeout=10, 
                            echo = TRUE,
                            until_prompt = FALSE,
                            prompt_callback = self$prompt_callback
                        )
            self$runner$display_changed_graphics()
            kernel$restore_shell_parent(parent_save)
        },
        prompt_callback = function() {
            self$runner$stdout("> ")
        }
    )
)