#' @importFrom callr r_session_options r_session
#' @importFrom processx poll conn_read_chars
#' @importFrom crayon red

#' @export
RSessionBase <- R6Class("RSessionBase",
  inherit = r_session,
  public = list(
    prompt = "> ",
    banner = "",
    waiting = FALSE,
    initialize = function(options = r_session_options(
                            stdout = "|",
                            stderr = "|",
                            cmdargs = c(
                              "--interactive",
                              "--no-readline",
                              "--no-save",
                              "--no-restore"
                            ),
                            env = c(R_CLI_NUM_COLORS="16777216")),
                          prompt = "> "
                          ) {
      # log_out("Starting session ...")
      super$initialize(
        options = options,
        wait = FALSE
      )
      resp <- self$receive_all_output(timeout = 1000)
      banner <- resp$stdout
      banner <- strsplit(banner, self$prompt, fixed = TRUE)
      banner <- unlist(banner)[1]
      banner <- remove_prefix(banner,"\n") |> remove_suffix("\n")
      self$banner <- banner
      # log_out("Done.")
    },
    sleeping = function() {
      self$get_status() == "sleeping"
    },
    drop_last_input = FALSE,
    last_input = "",
    send_input = function(text, drop_echo = FALSE) {
      if(!length(text) || !is.character(text)) return(invisible())
      if(length(text) > 1) text <- paste(text,collapse="\n")
      if(!endsWith(text, "\n")) text <- paste0(text, "\n")
      self$drop_last_input <- drop_echo
      if(drop_echo) {
        self$last_input <- text
      }
      while (TRUE) {
        text <- self$write_input(text)
        if (!length(text)) {
          break
        } 
      }
    },
    read_output = function(n = -1) {
      res <- super$read_output(n = n)
      if(self$drop_last_input) {
          res <- remove_prefix(res,self$last_input)
          self$drop_last_input <- FALSE
          self$last_input <- ""
      }
      res
    },
    receive_output = function(timeout = 1){
      self$waiting <- FALSE
      sleeping <- self$get_status() == "sleeping"
      poll_res <- self$poll_io(timeout)
      res <- list()
      if (poll_res[1] == "ready") {
        res$stdout <- self$read_output()
      }
      if (poll_res[2] == "ready") {
        res$stderr <- self$read_error()
      }
      if (poll_res[3] == "ready") {
        msg <- self$read()
        res$msg <- msg
        res$stdout <- paste0(res$stdout, msg$stdout)
        res$stderr <- paste0(res$stderr, msg$stderr)
      }
      if(all(poll_res == "timeout") && sleeping)
        self$waiting <- TRUE
      return(res)
    },
    receive_all_output = function(timeout = 1){
      # log_out("receive_all_output")
      while(!self$sleeping()) Sys.sleep(.001)
      poll_res <- self$poll_io(timeout)
      resp <- list()
      while (any(poll_res == "ready")) {
        if (poll_res[1] == "ready") {
          sout <- self$read_output()
          resp$stdout <- paste0(resp$stdout, sout)
        }
        if (poll_res[2] == "ready") {
          serr <- self$read_error()
          resp$stderr <- paste0(resp$stderr, serr)
        }
        if (poll_res[3] == "ready") {
          msg <- self$read()
          resp$msg <- msg
          resp$stdout <- paste0(resp$stdout, msg$stdout)
          resp$stderr <- paste0(resp$stderr, msg$stderr)
        }
        poll_res <- self$poll_io(timeout)
      }
      return(resp)
    },
    send_receive = function(text, timeout = 100) {
      self$send_input(text)
      self$receive_all_output(timeout = timeout)
    }
  )
)

RKernelSession <- R6Class("RKernelSession",
  inherit = RSessionBase,
  public = list(
    connect = function(yield, kernel) {
      self$yield <- yield
      self$kernel <- kernel
    },
    setup = function() {
      # log_out("Setting up the session ...")
      self$help_port <- random_open_port()
      self$send_input(sprintf("RKernel:::setup_session(%d)",self$help_port))
      self$receive_all_output(timeout = 1000)
      # log_out("Done.")
  },
  help_port = NULL,
  start_graphics = function() {
      self$send_input("RKernel::start_graphics()")
      self$send_input("httpgd::hgd()")
      self$receive_all_output(timeout = 1000)
      gd <- self$send_receive("dput(httpgd::hgd_details())")
      gd <- drop_echo(gd$stdout) |> drop_prompt(prompt=self$prompt)
      gd <- str2expression(gd)
      eval(gd)
  },
  yield = NULL, # Optional function to service kernel requests
  kernel = NULL, # Reference to the controlling kernel
  gdetails = NULL
  )
)
split_lines1 <- function(x) {
  unlist(strsplit(x, "\n", fixed = TRUE))
}

drop_echo <- function(txt, n = 1) {
  if(length(txt) && nzchar(txt)){
      out_lines <- split_lines1(txt)
      ii <- 1:n
      out_lines <- out_lines[-ii]
      drop <- startsWith(out_lines, "+")
      out_lines <- out_lines[!drop]
      txt <- paste(out_lines, collapse = "\n")
  }
  txt
}

drop_prompt <- function(txt, prompt="> ") {
  if(endsWith(txt, prompt))
    txt <- remove_suffix(txt,prompt)
  txt
}


XON <- '\x11'
XOFF <- '\x13'
XOFFXON <- paste0(XOFF,XON)


#' @export
RSessionAdapter <- R6Class("RSessionAdapter",
 public = list(
    session = NULL,
    prompt = NULL,
    browse_prompt = "Browse\\[([0-9]+)\\]> $",
    co_prompt = NULL,
    io_timeout = 0,
    stdout = character(0),
    stderr = character(0),
    stdout_callback = NULL,
    stderr_callback = NULL,
    browser_callback = NULL,
    prompt_callback = NULL,
    input_callback = NULL,
    echo = FALSE,
    aggreg_stdout = function(txt, ...) {
      self$stdout <- paste0(self$stdout,txt)
    },
    aggreg_stderr = function(txt, ...) {
      self$stderr <- paste0(self$stderr,txt)
    },
    collect = function(clear = TRUE) {
      res <- list(stdout = self$stdout,
                  stderr = self$stderr)
      if(clear) {
        self$stdout <- character(0)
        self$stderr <- character(0)
      }
      return(res)
    },
    initialize = function(
      session,
      stdout_callback = self$aggreg_stdout,
      stderr_callback = self$aggreg_stderr,
      browser_callback = NULL,
      prompt_callback = NULL,
      input_callback = NULL,
      prompt = "> ",
      co_prompt = "+ ",
      echo = FALSE
    ) {
      self$session <- session
      self$prompt <- prompt
      self$co_prompt <- co_prompt
      self$stdout_callback <- stdout_callback
      self$stderr_callback <- stderr_callback
      self$browser_callback <- browser_callback
      self$prompt_callback <- prompt_callback
      self$input_callback <- input_callback
      self$echo <- echo
    },
    run_code = function(
        code,
        io_timeout = 1,
        stdout_callback = self$stdout_callback,
        stderr_callback = self$stderr_callback,
        browser_callback = self$browser_callback,
        prompt_callback = self$prompt_callback,
        input_callback = self$input_callback,
        until_prompt = TRUE,
        echo = self$echo
      ) {
      if(!is.character(code) || 
         length(code) < 1) return()
      if(length(code) > 1) {
        code <- paste(code, collapse="\n")
      }
      code_lines <- split_lines1(code) 
      output_complete <- TRUE
      for(line in code_lines) {
        # log_out(sprintf("Sending input '%s'",line))
        self$session$send_input(line)
        output_complete <- self$process_output(
                              io_timeout = 1,
                              stdout_callback = stdout_callback,
                              stderr_callback = stderr_callback,
                              browser_callback = browser_callback,
                              input_callback = input_callback,
                              prompt_callback = prompt_callback,
                              until_prompt = FALSE,
                              drop_echo = !echo)
        #output <- self$collect()
        #stderr_callback(output$stderr)
        #stdout_callback(output$stdout)
      }
      while(!output_complete){
          output_complete <- self$process_output(
                              io_timeout = io_timeout,
                              stdout_callback = stdout_callback,
                              stderr_callback = stderr_callback,
                              browser_callback = browser_callback,
                              input_callback = input_callback,
                              prompt_callback = prompt_callback,
                              until_prompt = until_prompt,
                              drop_echo = FALSE)
      }
    },
    interrupt = function() {
      # log_out("REPL interrupt")
      counter <- 1
      repeat {
        self$session$interrupt()
        # log_out("interrupt sent")
        self$session$send_input("")
        # log_out("receiving output")
        res <- try(self$session$receive_all_output(),silent=TRUE)
        if(length(res)) {
          # log_out(res, use.print = TRUE)
          # log_out("finished ...")
          return(TRUE)
        }
        counter <- counter + 1
        if(counter > 15) {
          kernel <- self$session$kernel
          # self$session$close()
          kernel$restore_execute_parent()
          log_warning("R process cannot be interrupted, restarting ...\n")
          kernel$stderr("R process cannot be interrupted, restarting ...\n")
          kernel$restart()
          kernel$stderr("Restart done.")
          return(TRUE)
        }
        # log_out("trying again ...")
      }
    },
    found_prompt = FALSE,
    found_browse_prompt = character(0),
    process_output = function(
        io_timeout = 1,
        stdout_callback = self$stdout_callback,
        stderr_callback = self$stderr_callback,
        browser_callback = self$browser_callback,
        input_callback = self$input_callback,
        prompt_callback = self$prompt_callback,
        until_prompt = TRUE,
        drop_echo = FALSE
      ) {
        stopifnot(is.function(stdout_callback))
        stopifnot(is.function(stderr_callback))
        session <- self$session
        resp <- session$receive_output(timeout = io_timeout)
        self$found_browse_prompt <- character(0)
        self$found_prompt <- FALSE
        output_complete <- FALSE
        if(!length(resp)) {
          output_complete <- !until_prompt
        }
        if (!is.null(resp$stderr) 
              && nzchar(resp$stderr)
              && grepl('\\S',resp$stderr)) {
              stderr_callback(resp$stderr)
        }
        if (!is.null(resp$stdout)) {
          # log_out("======== process_output ==========")
          # log_out(resp$stdout)
          if(drop_echo) {
            resp$stdout <- drop_echo(resp$stdout)
          }
          if(grepl(BEL, resp$stdout)) {
            resp$stdout <- self$handle_BEL(resp$stdout,
                                           input_callback,
                                           stdout_callback,
                                           stderr_callback)
          }
          if(grepl(self$browse_prompt, resp$stdout)) {
            # log_out("Found browser prompt")
            self$found_browse_prompt <- getlastmatch(self$browse_prompt, 
                                                     resp$stdout)
            resp$stdout <- gsub(self$browse_prompt,"",resp$stdout)
          } else if (endsWith(resp$stdout, self$prompt)) {
            # log_out("Found main prompt")
            # log_out(self$status)
            self$found_prompt <- TRUE
            resp$stdout <- remove_suffix(resp$stdout, self$prompt)
          } 
          if(nzchar(resp$stdout)) {
            stdout_callback(resp$stdout)
          } 
          if(self$found_prompt) {
            if(is.function(prompt_callback)) {
              output_complete <- prompt_callback()
            } else {
              output_complete <- TRUE
            }
          } else if(length(self$found_browse_prompt)) {
            if (is.function(browser_callback)) {
              output_complete <- browser_callback(prompt=self$found_browse_prompt)
            } else {
              session$send_input("Q")
              output_complete <- !until_prompt
            }
          }
        }
        return(output_complete)
    },
    run_cmd = function(cmd) {
      # Runs a one-line command without checking(!) and returns the 
      # output
      # log_out(sprintf("Run cmd '%s'",cmd))
      self$run_code(cmd,
                    io_timeout = 1,
                    stdout_callback = self$aggreg_stdout,
                    stderr_callback = self$aggreg_stderr,
                    browser_callback = TrueFunc,
                    input_callback = NULL,
                    until_prompt = TRUE,
                    echo = FALSE
                    )
      res <- self$collect()
      return(res)
    },
    getOption = function(n, default = NULL) {
      cmd <- sprintf("dput(getOption(\"%s\",NULL))",n)
      res <- self$run_cmd(cmd)
      res <- eval(str2expression(res$stdout))
      if(is.null(res)) res <- default
      res
    },
    eval = function(expr, safe = FALSE) {
      # log_out("session$eval()")
      code <- deparse(substitute(expr))
      self$eval_code(code, safe = safe)
    },
    eval_code = function(code, safe = FALSE) {
      if(safe) {
        code <- sprintf("try(dput(%s),silent=TRUE)",code)
      } else {
        code <- sprintf("dput(%s)",code)
      }
      res <- self$run_cmd(code)
      if(length(res$stderr))
        res$stderr
      else 
        eval(str2expression(res$stdout))
    },
    ls = function() {
      res <- self$run_cmd("dput(ls())")
      eval(str2expression(res$stdout))
    },
    get = function(n) {
      cmd <- sprintf("dput(get0(\"%s\"))",n)
      res <- self$run_cmd(cmd)
      eval(str2expression(res$stdout))
    },
    assign = function(n, value) {
      val <- paste(deparse(value), collapse = "\n")
      cmd <- paste(n,"<-",val)
      self$run_cmd(cmd)
      invisible(NULL)
    },
    setOption = function(n, value) {
      val <- paste(deparse(value), collapse = "\n")
      cmd <- sprintf("options(%s = %s)", n, val)
      self$run_cmd(cmd)
      invisible(NULL)
    },
    importOption = function(n) {
      opt <- list(self$getOption(n))
      names(opt) <- n
      do.call("options", opt)
    },
    errored = FALSE,
    handle_BEL = function(txt,
                          input_callback,
                          stdout_callback,
                          stderr_callback) {
      if(grepl(READLINE_PROMPT,txt)) {
        self$handle_readline(txt,
                         input_callback,
                         stdout_callback,
                         stderr_callback)
      } else if(grepl(SCAN_BEGIN, txt)) {
        self$handle_scan(txt,
                         input_callback,
                         stdout_callback,
                         stderr_callback)
      }
    },
    handle_readline = function(txt, 
                           input_callback, 
                           stdout_callback,
                           stderr_callback) {
        # log_out("Found readline prompt")
        # log_out(self$status)
        txt <- remove_suffix(txt, BEL)
        splt <- split_string1(txt, READLINE_PROMPT)
        prefix <- splt[1]
        if(nzchar(prefix)) stdout_callback(prefix)
        if(length(splt) > 1) {
          prompt <- splt[2]
        } else {
          prompt <- ""
        }
        inp <- input_callback(prompt = prompt)
        self$session$send_input(inp, drop_echo = TRUE)
        return("")
    },
    handle_scan = function(txt, 
                           input_callback, 
                           stdout_callback,
                           stderr_callback) {
      session <- self$session
      txt <- split_string1(txt, SCAN_BEGIN)
      stdout_callback(txt[1])
      if(length(txt) > 1) {
        prompt <- txt[2]
      } else {
        resp <- session$receive_output(1)
        prompt <- resp$stdout
      }
      repeat {
        inp <- input_callback(prompt = prompt)
        session$send_input(inp, drop_echo = TRUE)
        resp <- session$receive_output(1)
        txt <- resp$stdout
        mes <- resp$stderr
        if(length(mes)) stderr_callback(mes)
        if(grepl(SCAN_END, txt)) {
          txt <- split_string1(txt, SCAN_END)
          if(nzchar(txt[1])) stdout_callback(txt[1])
          if(length(txt) > 1) {
            ret <- split_string1(txt[2],"> ")
            if(length(ret) > 1) {
              ret <- tail(ret, 1)
            }
            ret <- paste0(ret, "> ")
            return(ret) 
          } else {
            return("")
          }
        } else {
          prompt <- txt
        }
      }
    } 
 )) 

TrueFunc <- function(...) TRUE

getlastmatch <- function(pattern, txt) {
  m <- regexpr(pattern, txt)
  tail(regmatches(txt,m), n = 1L)
}

