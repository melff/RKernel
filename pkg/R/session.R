#' @importFrom callr r_session_options r_session
#' @importFrom processx poll conn_read_chars
#' @importFrom crayon red

#' @export
RKernelSession <- R6Class("RKernelSession",
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
                          yield = NULL,
                          kernel = NULL,
                          prompt = "> "
                          ) {
      super$initialize(
        options = options,
        wait = FALSE
      )
      resp <- self$receive_all_output(timeout = 1000)
      banner <- resp$stdout
      banner <- strsplit(banner, self$prompt, fixed = TRUE)
      banner <- unlist(banner)[1]
      banner <- remove_prefix(banner,"\n") |> remove_suffix("\n")
      self$yield <- yield
      self$banner <- banner
      self$kernel <- kernel
    },
    drop_last_input = FALSE,
    last_input = "",
    send_input = function(text, drop_echo = FALSE) {
      if (!endsWith(text, "\n")) text <- paste0(text, "\n")
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
    yield = NULL, # Optional function to service kernel requests
    kernel = NULL # Reference to the controlling kernel
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
      txt <- paste(out_lines, collapse = "\n")
      # log_out(resp, use.print = TRUE)
  }
  txt
}

XON <- '\x11'
XOFF <- '\x13'

#' @export
RSessionAdapter <- R6Class("RSessionAdapter",
 public = list(
    session = NULL,
    suspended = FALSE,
    prompt = NULL,
    browse_prompt = "Browse\\[([0-9]+)\\]> $",
    co_prompt = NULL,
    run_timeout = 0,
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
        run_timeout = 10,
        wait_callback = NULL,
        stdout_callback = self$stdout_callback,
        stderr_callback = self$stderr_callback,
        browser_callback = self$browser_callback,
        prompt_callback = self$prompt_callback,
        input_callback = self$input_callback,
        until_prompt = FALSE,
        echo = self$echo
      ) {
        if(any(nzchar(code))) {
          lines <- split_lines1(code)
        } else {
          lines <- code
        }
        n_lines <- length(lines)
        for (i in 1:n_lines) {
          line <- lines[i]
          if(!length(line) || !is.character(line) || is.na(line)) break
          # log_out(sprintf("Sending line '%s'",line))
          self$session$send_input(line)
          tryCatch(self$process_output(
                          io_timeout = io_timeout,
                          run_timeout = run_timeout,
                          stdout_callback = stdout_callback,
                          stderr_callback = stderr_callback,
                          wait_callback = wait_callback,
                          browser_callback = browser_callback,
                          prompt_callback = prompt_callback,
                          input_callback = input_callback,
                          until_prompt = until_prompt,
                          echo = echo),
            interrupt = function(e) {
              self$session$interrupt()
              self$process_output()
            }
          )
        }
    },
    found_prompt = FALSE,
    process_output = function(
        io_timeout = 1,
        run_timeout = 100,
        wait_callback = NULL,
        stdout_callback = self$stdout_callback,
        stderr_callback = self$stderr_callback,
        browser_callback = self$browser_callback,
        input_callback = self$input_callback,
        prompt_callback = self$prompt_callback,
        until_prompt = TRUE,
        echo = FALSE
      ) {
        stopifnot(is.function(stdout_callback))
        stopifnot(is.function(stderr_callback))
        session <- self$session
        output_complete <- FALSE
        loop_count <- 0
        self$found_prompt <- FALSE
        while(!output_complete){
          loop_count <- loop_count + 1
          resp <- session$receive_output(timeout = io_timeout)
          if(!length(resp) && run_timeout > 0) {
            while(session$get_status() == "running") {
                Sys.sleep(run_timeout/1000)
                if(is.function(wait_callback))
                    wait_callback()
            } 
          }
          if(!length(resp) && session$waiting && !self$suspended) {
            # log_out("Session waiting for input(?)")
            if(is.function(input_callback)) {
              inp <- input_callback()
              session$send_input(inp, drop_echo = TRUE)
            }
          }
          if (!is.null(resp$stderr) 
              && nzchar(resp$stderr)) {
              stderr_callback(resp$stderr)
          }
          if (!is.null(resp$stdout)) {
            if (loop_count == 1 && !echo) {
              resp$stdout <- drop_echo(resp$stdout)
            }
            # so <- resp$stdout
            # if(grepl(XOFF,so) || grepl(XON,so)) {
            #   so <- sub(XOFF,"[XOFF]",so)
            #   so <- sub(XON,"[XON]",so)
            #   log_out(so)
            # }
            if(startsWith(resp$stdout,XON) && !endsWith(resp$stdout,XOFF)) {
              self$suspended <- FALSE
              # log_out("Output restarted")
            }
            else if(endsWith(resp$stdout,XOFF)) {
              self$suspended <- TRUE
              # log_out("Output suspended")
            } 
            resp$stdout <- gsub(XON,"",resp$stdout)
            resp$stdout <- gsub(XOFF,"",resp$stdout)
            if (grepl(self$browse_prompt, resp$stdout)) {
              # log_out("Found browser prompt")
              prompt <- getlastmatch(self$browse_prompt, resp$stdout)
              resp$stdout <- gsub(self$browse_prompt,"",resp$stdout)
              if (nzchar(resp$stdout)) {
                stdout_callback(resp$stdout)
              } 
              if (is.function(browser_callback)) {
                output_complete <- browser_callback(prompt=prompt)
              } else session$send_input("Q")
            } else if (endsWith(resp$stdout, self$co_prompt)) {
              # log_out("Found continuation prompt")
              if(echo) {
                #resp$stdout <- remove_suffix(resp$stdout, self$co_prompt)
                if (nzchar(resp$stdout)) {
                    stdout_callback(resp$stdout)
                }
              }
              else {
                resp$stdout <- NULL
              }
              output_complete <- !until_prompt
            } else if (endsWith(resp$stdout, self$prompt)) {
              # log_out("Found main prompt")
              # log_out(self$status)
              self$found_prompt <- TRUE
              resp$stdout <- remove_suffix(resp$stdout, self$prompt)
              if (nzchar(resp$stdout)) {
                stdout_callback(resp$stdout)
              }
              if(is.function(prompt_callback)) {
                output_complete <- prompt_callback()
              } else {
                output_complete <- TRUE
              }
            } else {
              # log_out("stdout callback")
              stdout_callback(resp$stdout)
            }
          }
        }
    },
    run_cmd = function(cmd) {
      # Runs a one-line command without checking(!) and returns the 
      # output
      # log_out(sprintf("Run cmd '%s'",cmd))
      self$run_code(cmd,
                    io_timeout = 1,
                    run_timeout = 0,
                    stdout_callback = self$aggreg_stdout,
                    stderr_callback = self$aggreg_stderr,
                    wait_callback = NULL,
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
    eval = function(expr) {
      # log_out("session$eval()")
      code <- deparse(substitute(expr))
      self$eval_code(code)
    },
    eval_code = function(code) {
      code <- sprintf("dput(%s)",code)
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
    errored = FALSE
 )) 

TrueFunc <- function(...) TRUE

getlastmatch <- function(pattern, txt) {
  m <- regexpr(pattern, txt)
  tail(regmatches(txt,m), n = 1L)
}
