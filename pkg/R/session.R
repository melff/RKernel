#' The R Session Base Class
#'
#' @description An object of this class handles the lower-level communication with
#'   an R process. Objects of class \code{\link{RKernelSession}} inherit from
#'   this class.
#' @importFrom callr r_session_options r_session
#' @importFrom processx poll conn_read_chars
#' @importFrom crayon red
#' @export
RSessionBase <- R6Class("RSessionBase",
  inherit = r_session,
  public = list(
    #' @field prompt The command prompt
    prompt = "> ",
    #' @field banner The R startup message used as a session banner in 
    #'    the terminal and info box.
    banner = "",
    #' @field waiting A logical value, whether the R session is waiting 
    #'    for input.
    waiting = FALSE,
    #' @description Initialize the object and start the session
    #' @param options R session objects, see \code{\link[callr]{r_session_options}}.
    #' @param env A character vector with environment variables for the R process
    #' @param prompt The expected prompt string of the R session
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
    #' @description Returns a logical value, indicating whether the R process is sleeping.
    sleeping = function() {
      self$get_status() == "sleeping"
    },
    #' @description Send input text to the R process
    #' @param text A character string
    #' @param drop_echo A logical value, whether to drop the echo from
    #'    stdout.
    send_input = function(text, drop_echo = FALSE) {
      if(!length(text) || !is.character(text)) return(invisible())
      if(length(text) > 1) text <- paste(text,collapse="\n")
      if(!endsWith(text, "\n")) text <- paste0(text, "\n")
      while (TRUE) {
        text <- self$write_input(text)
        if (!length(text)) {
          break
        } 
      }
    },
    #' @description Read output from the R session and drop
    #'    input echo if so requested
    #' @param n The number of characters to read
    read_output = function(n = -1) {
      res <- super$read_output(n = n)
      res
    },
    #' @description Poll R process for output and read it
    #' @param timeout A number, the polling timeout in microseconds
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
    #' @description Receive all output that is available
    #' @param timeout A number, the polling timeout in microseconds
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
    #' @description Send text to R process and receive all output from the
    #'    process
    #' @param text A character string
    #' @param timeout An integer number, the polling timeout
    send_receive = function(text, timeout = 100) {
      self$send_input(text)
      self$receive_all_output(timeout = timeout)
    }
  )
)

#' The KernelSession Class
#'
#' @description An object of this class handles the communication with
#'   an R process. There is usually one main session, but there may
#'   also sessions for detached code cells.
#' @importFrom curl curl_fetch_memory
#' @export
RKernelSession <- R6Class("RKernelSession",
  inherit = RSessionBase,
  public = list(
    #' @description Connect the session with the main Kernel object
    #' @param yield The function that is called when the session objects
    #'   yields back control to the kernel object, for example in order
    #'   to wait for comm message when a widget is active.
    #' @param kernel The main kernel objectd
    connect = function(yield, kernel) {
      self$yield <- yield
      self$kernel <- kernel
    },
    #' @description Set up the R session, by installing hooks etc.
    setup = function() {
      self$http_port <- random_open_port()
      self$send_input(sprintf("RKernel:::setup_session(%d)",self$http_port))
      self$send_input("RKernel:::startup()")
      self$receive_all_output(timeout = 1000)
  },
  #' @field http_port The port number of HTML help.
  http_port = NULL,
  #' @field hostname The hostname.
  hostname = "localhost",
  #' @description Initialize graphics, start device and
  #'    return details
  start_graphics = function() {
      self$send_input("RKernel:::start_graphics()")
      self$dev_new()
  },
  #' @description Start a httpgd device and return the
  #'    graphics details.
  dev_new = function() {
      self$receive_all_output(timeout = 1000)
  },
  #' @description Return the
  #'    graphics details.
  graphics_details = function() {
      gd <- self$send_receive("dput(RKernel:::graphics_details())")
      gd <- drop_echo(gd$stdout) |> drop_prompt(prompt=self$prompt)
      gd <- str2expression(gd)
      eval(gd)
  },
  #' @field yield An optional function to service kernel requests
  yield = NULL, 
  #' @field kernel The reference to the controlling kernel object
  kernel = NULL, 
  http_get = function(slug="",query="") {
      url <- paste0("http://",
                     self$hostname,":",
                     self$http_port,"/",
                     slug)
      url <- paste0(url,"?",query)
      resp <- tryCatch(curl_fetch_memory(url),
                      error = function(e) invokeRestart("continue"),
                      interrupt = function(e) invokeRestart("continue"))
      list(
          content = resp$content,
          type = resp$type
        )
  })
)

# This is convenience form to split a single character
# string and return a character vector (instead of a list).
split_lines1 <- function(x) {
  y <- unlist(strsplit(x, "\n", fixed = TRUE))
  if(endsWith(x,"\n")) {
    y <- c(y,"")
  }
  y
}

# A helper function to drop the echo of R expressions sent
# to the R process.
# @param txt A character string, the output from which the echo
#     is to be dropped
# @param n The number of lines to drop
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

# Remove the command prompt from the output
# @param txt A character string 
# @param prompt A character string
drop_prompt <- function(txt, prompt="> ") {
  if(endsWith(txt, prompt))
    txt <- remove_suffix(txt,prompt)
  txt
}


XON <- '\x11'
XOFF <- '\x13'
XOFFXON <- paste0(XOFF,XON)

#' A Rich R Session Interface
#' 
#' @description
#' Objects from this class handle the "higher-level" interaction 
#' between the frontend and the R session. There can be more such 
#' interfaces to a session. For example, for the main REPL 
#' and for a REPL created by a call to browser().
#' @export
RSessionAdapter <- R6Class("RSessionAdapter",
 public = list(
    #' @field session An RKernelSession object or `NULL`
    session = NULL,
    #' @field prompt The R console prompt or NULL
    prompt = NULL,
    #' @field coprompt The R console continuation prompt or NULL
    coprompt = NULL,
    #' @field browse_prompt The prompt created by a call to `browser()`
    browse_prompt = "Browse\\[([0-9]+)\\]> $",
    #' @field io_timeout An integer number a timeout in microseconds
    io_timeout = 0,
    #' @field stdout Accumulated output via the stdout channel.
    stdout = character(0),
    #' @field stderr Accumulated output via the stderr channel.
    stderr = character(0),
    #' @field stdout_callback A function to be called with stdout text or NULL
    stdout_callback = NULL,
    #' @field stderr_callback A function to be called with stderr text or NULL
    stderr_callback = NULL,
    #' @field browser_callback A function to be called when a browser prompt
    #'    is encountered or NULL
    browser_callback = NULL,
    #' @field prompt_callback A function to be called when a command prompt
    #'    is encountered or NULL
    prompt_callback = NULL,
    #' @field input_callback A function to be called when input is required
    #'    or NULL
    input_callback = NULL,
    #' @field echo A logical value, if TRUE code sent to the R process will
    #'    be echoed
    echo = FALSE, 
    #' @description A potential "stdout_callback" function that aggregates
    #'    output sent from the R process via "stdout" channel to the 
    #'    eponymous "stdout" field
    #' @param txt A character string
    #' @param ... Other arguments, ignored
    aggreg_stdout = function(txt, ...) {
      self$stdout <- paste0(self$stdout,txt)
    },
    #' @description A potential "stderr_callback" function that aggregates
    #'    output sent from the R process via "stderr" channel to the 
    #'    eponymous "stderr" field
    #' @param txt A character string
    #' @param ... Other arguments, ignored
    aggreg_stderr = function(txt, ...) {
      self$stderr <- paste0(self$stderr,txt)
    },
    #' @description Collect the accumulated output from fields "stdout" and
    #'    "stderr" into a list with two elements named "stdout" and "stderr".
    #' @param clear A logical value, whether accumulated output should be
    #'    cleared after being returned.
    collect = function(clear = TRUE) {
      res <- list(stdout = self$stdout,
                  stderr = self$stderr)
      if(clear) {
        self$stdout <- character(0)
        self$stderr <- character(0)
      }
      return(res)
    },
    #' @description Initialize an object 
    #' @param session An object from class "RKernelSession"
    #' @param stdout_callback A callback function for "stdout" output
    #' @param stderr_callback A callback function for "stderr" output
    #' @param browser_callback A callback function for browser prompts (optional)
    #' @param prompt_callback A callback function for command prompts encountered
    #' @param input_callback A callback function for input requests (optional)
    #' @param prompt A character string, the expected command prompt
    #' @param echo A logical value, whether to echo input
    initialize = function(
      session,
      stdout_callback = self$aggreg_stdout,
      stderr_callback = self$aggreg_stderr,
      browser_callback = NULL,
      prompt_callback = NULL,
      input_callback = NULL,
      prompt = "> ",
      coprompt = "+ ",
      echo = FALSE
    ) {
      self$session <- session
      self$prompt <- prompt
      self$coprompt <- coprompt
      self$stdout_callback <- stdout_callback
      self$stderr_callback <- stderr_callback
      self$browser_callback <- browser_callback
      self$prompt_callback <- prompt_callback
      self$input_callback <- input_callback
      self$echo <- echo
    },
    #' @description Run code and pass output to callback functions
    #' @param code A character string or character vector with code lines
    #' @param io_timeout An integer value, the timeout of waiting for output
    #' @param stdout_callback A callback function for "stdout" output
    #' @param stderr_callback A callback function for "stderr" output
    #' @param browser_callback A callback function for browser prompts (optional)
    #' @param prompt_callback A callback function for command prompts encountered
    #' @param input_callback A callback function for input requests (optional)
    #' @param until_prompt A logical value, whether process and wait output
    #'    until a command prompt is encountered.
    #' @param echo A logical value, whether to echo input
    run_code = function(
        code,
        io_timeout = 1,
        stdout_callback = self$stdout_callback,
        stderr_callback = self$stderr_callback,
        browser_callback = self$browser_callback,
        prompt_callback = self$prompt_callback,
        input_callback = self$input_callback,
        until_prompt = TRUE,
        echo = self$echo,
        debug = FALSE
      ) {
      if(debug) {
        log_out("==== run_code() ====")
        log_str(code)
        log_out(code)
      }
      if(!is.character(code) || 
         length(code) < 1) return()
      if(length(code) > 1) {
        code <- paste(code, collapse="\n")
      }
      if(nzchar(code)) {
        code <- split_lines1(code) 
      }
      if(debug) log_out(sprintf("Code has %d lines",length(code)))
      # Make sure that we are at a code input prompt
      while(until_prompt && !self$found_prompt) {
          if(self$poll_output(io_timeout = 1000)) {
              self$process_output(
                       stdout_callback = stdout_callback,
                       stderr_callback = stderr_callback,
                       browser_callback = browser_callback,
                       input_callback = input_callback,
                       prompt_callback = prompt_callback,
                       drop_echo = FALSE,
                       debug = debug)
          }
      }
      for(line in code) {
        if(debug) log_out(sprintf("Sending input '%s'",line))
        self$session$send_input(line)
        if(self$poll_output(io_timeout = 1000)) {
            self$process_output(
                     stdout_callback = stdout_callback,
                     stderr_callback = stderr_callback,
                     browser_callback = browser_callback,
                     input_callback = input_callback,
                     prompt_callback = prompt_callback,
                     drop_echo = !echo,
                     debug = debug)
        }
      }
      while(until_prompt && !self$found_prompt){
          if(self$poll_output(io_timeout = io_timeout)) {
              self$process_output(
                       stdout_callback = stdout_callback,
                       stderr_callback = stderr_callback,
                       browser_callback = browser_callback,
                       input_callback = input_callback,
                       prompt_callback = prompt_callback,
                       drop_echo = FALSE,
                       debug = debug)
          }
      }
    },
    #' @description Send an interrupt signal (SIGINT) to the R process. This
    #'    should stop what the R process is doing without killing it.
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
    current_output = list(),
    poll_output = function(io_timeout = 1) {
        ready <- FALSE
        self$current_output <- list()
        resp <- self$session$receive_output(timeout = io_timeout)
        if(length(resp$stdout) || length(resp$stderr)) {
            self$current_output <- resp
            ready <- TRUE
        }
        ready
    },
    #' @field found_prompt A logical value, whether a prompt has been found
    #'    in the output of the R process
    found_prompt = TRUE,
    #' @field found_browse_prompt The latest instance of the browser prompt
    #'    pattern found in the R process output
    found_browse_prompt = character(0),
    #' @description Process output created by commands sent to the R process
    #' @param io_timeout An integer value, the timeout of waiting for output
    #' @param stdout_callback A callback function for "stdout" output
    #' @param stderr_callback A callback function for "stderr" output
    #' @param browser_callback A callback function for browser prompts (optional)
    #' @param input_callback A callback function for input requests (optional)
    #' @param prompt_callback A callback function for command prompts encountered
    #' @param until_prompt A logical value, whether process and wait output
    #'    until a command prompt is encountered.
    #' @param drop_echo A logical value, whether input echo be dropped
   process_output = function(
        stdout_callback = self$stdout_callback,
        stderr_callback = self$stderr_callback,
        browser_callback = self$browser_callback,
        input_callback = self$input_callback,
        prompt_callback = self$prompt_callback,
        drop_echo = FALSE,
        debug = FALSE
      ) {
        stopifnot(is.function(stdout_callback))
        stopifnot(is.function(stderr_callback))
        resp <- self$current_output
        if(debug) {
            log_out("======== process_output ==========")
            log_out(resp$stdout)
        }
        
        self$found_browse_prompt <- character(0)
        self$found_prompt <- FALSE
        if (!is.null(resp$stderr) 
              && nzchar(resp$stderr)
              && grepl('\\S',resp$stderr)) {
              stderr_callback(resp$stderr)
        }
        if (!is.null(resp$stdout)) {
          if(drop_echo) {
            if(debug) log_out("dropping echo")
            resp$stdout <- drop_echo(resp$stdout)
          }
          if(grepl(BEL, resp$stdout)) {
            if(debug) log_out("handling BEL")
            resp$stdout <- self$handle_BEL(resp$stdout,
                                           input_callback,
                                           stdout_callback,
                                           stderr_callback)
          }
          if(grepl(self$browse_prompt, resp$stdout)) {
            if(debug) log_out("Found browser prompt")
            self$found_browse_prompt <- getlastmatch(self$browse_prompt, 
                                                     resp$stdout)
            resp$stdout <- gsub(self$browse_prompt,"",resp$stdout)
          } else if (endsWith(resp$stdout, self$prompt)) {
            if(debug) log_out("Found main prompt")
            # log_out(self$status)
            self$found_prompt <- TRUE
            resp$stdout <- remove_suffix(resp$stdout, self$prompt)
          } 
          if(nzchar(resp$stdout)) {
            # log_out("Calling stdout_callback")
            # log_out(resp$stdout)
            stdout_callback(resp$stdout)
          } 
          if(self$found_prompt) {
            if(is.function(prompt_callback)) {
              prompt_callback()
            } 
          } else if(length(self$found_browse_prompt)) {
            if (is.function(browser_callback)) {
              if(debug) {
                log_out("Calling browser_callback")
                log_print(browser_callback)
              }
              self$found_prompt <- browser_callback(prompt=self$found_browse_prompt)
            } else {
              session$send_input("Q")
            }
          }
        }
        if(debug) {
          log_out("= DONE = process_output ==========")
        }
    },
    #' @description Run a one-line command without checking and return the 
    #'     output
    #' @param cmd A command string
    run_cmd = function(cmd, debug = FALSE) {
      if(debug) log_out(sprintf("Run cmd '%s'",cmd))
      self$run_code(cmd,
                    io_timeout = 1,
                    stdout_callback = self$aggreg_stdout,
                    stderr_callback = self$aggreg_stderr,
                    browser_callback = TrueFunc,
                    input_callback = NULL,
                    until_prompt = TRUE,
                    echo = FALSE,
                    debug = debug
                    )
      res <- self$collect()
      return(res)
    },
    #' @description Get an option value from the R session
    #' @param n A character string, the name of the requested option value
    #' @param default A default value
    getOption = function(n, default = NULL) {
      cmd <- sprintf("dput(getOption(\"%s\",NULL))",n)
      res <- self$run_cmd(cmd)
      res <- eval(str2expression(res$stdout))
      if(is.null(res)) res <- default
      res
    },
    #' @description Evaluate an expression in the R session and return the
    #'    result.
    #' @param expr An expression
    #' @param safe A logical value, whether errors should be caught
    eval = function(expr, safe = FALSE) {
      # log_out("session$eval()")
      code <- deparse(substitute(expr))
      self$eval_code(code, safe = safe)
    },
    #' @description Evaluate some code in the R session and return the
    #'    result.
    #' @param code A character string of code
    #' @param safe A logical value, whether errors should be caught
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
    #' @description Run `ls()` in the R session and return the result.
    ls = function() {
      res <- self$run_cmd("dput(ls())")
      eval(str2expression(res$stdout))
    },
    #' @description Get the value of a variable (named object) from the
    #'    R session and return it.
    #' @param n The name of the variable
    get = function(n) {
      cmd <- sprintf("dput(get0(\"%s\"))",n)
      res <- self$run_cmd(cmd)
      eval(str2expression(res$stdout))
    },
    #' @description Assign a value to a variable in the R session
    #' @param n A variable name
    #' @param value The value that is assigned to the variable
    assign = function(n, value) {
      val <- paste(deparse(value), collapse = "\n")
      cmd <- paste(n,"<-",val)
      self$run_cmd(cmd)
      invisible(NULL)
    },
    #' @description Set an option in the R session
    #' @param n The name of the option
    #' @param value The intended option value
    setOption = function(n, value) {
      val <- paste(deparse(value), collapse = "\n")
      cmd <- sprintf("options(%s = %s)", n, val)
      self$run_cmd(cmd)
      invisible(NULL)
    },
    #' @description Import an option value from the R session to the 
    #'    kernel
    #' @param n The option value
    importOption = function(n) {
      opt <- list(self$getOption(n))
      names(opt) <- n
      do.call("options", opt)
    },
    #' @field errored A logical value, whether an error occurred in the R
    #'    session
    errored = FALSE,
    #' @description Handle special output from the R session that starts 
    #'    with BEL
    #' @param txt Output string containing BEL
    #' @param input_callback A function to request input from the frontend
    #' @param stdout_callback A function to process output obtained from the
    #'    R session via "stdout" channel
    #' @param stderr_callback A function to process output obtained from the
    #'    R session via "stderr" channel
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
    #' @description Handle special an input request obtained from the 
    #'    R session via output indicated with a special output string
    #' @param txt Output string containing a special readline prompt
    #' @param input_callback A function to request input from the frontend
    #' @param stdout_callback A function to process output obtained from the
    #'    R session via "stdout" channel
    #' @param stderr_callback A function to process output obtained from the
    #'    R session via "stderr" channel
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
    #' @description Handle input request created by the function `scan` the 
    #'    in the R session
    #' @param txt Output string containing a special readline prompt
    #' @param input_callback A function to request input from the frontend
    #' @param stdout_callback A function to process output obtained from the
    #'    R session via "stdout" channel
    #' @param stderr_callback A function to process output obtained from the
    #'    R session via "stderr" channel
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

