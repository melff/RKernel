#' @importFrom callr r_session_options r_session
#' @importFrom processx poll conn_read_chars

#' @export
RKernelSession <- R6Class("RKernelSession",
  inherit = r_session,
  public = list(
    poll_conns = list(),
    prompt = "> ",
    prompt_regex = "> $",
    co_prompt = "+ ",
    co_prompt_regex = "[+] $",
    menu_prompt = ": ",
    browse_prompt = "^Browse\\[([0-9]+)\\]> $",
    prompt_found = FALSE,
    banner = "",
    initialize = function(options = r_session_options(
                            stdout = "|",
                            stderr = "|",
                            cmdargs = c(
                              "--interactive",
                              "--no-readline",
                              "--no-save",
                              "--no-restore"
                            )),
                            callbacks = list(
                              stdout = cat,
                              stderr = function(x){
                                cat(crayon::red(x))
                              },
                              msg = print
                            )
                          ) {
      super$initialize(
        options = options,
        wait = FALSE
      )
      self$poll_conns <- list(
        stdout = self$get_output_connection(),
        stderr = self$get_error_connection(),
        ctrl = self$get_poll_connection()
      )
      self$callbacks <- callbacks
      resp <- self$receive_all_output(timeout = 1000)
      banner <- resp$stdout
      self$banner <- unlist(strsplit(banner, self$prompt))[1]
    },
    run_code = function(code){
      lines <- split_lines1(code)
      n_lines <- length(lines)
      for (i in 1:n_lines) {
        self$prompt_found <- FALSE
        line <- lines[i]
        self$send_input(line)
        self$process_output()
      }
    },
    run_cmd = function(cmd, timeout = 1, drop_echo = TRUE){
      # log_out("Send cmd",cmd)
      self$send_input(cmd)
      self$prompt_found <- FALSE
      resp <- self$receive_to_prompt(timeout = timeout)
      if (drop_echo) {
        resp$stdout <- drop_echo(resp$stdout)
      }
      return(resp)
    },
    send_input = function(text) {
      if (!endsWith(text, "\n")) text <- paste0(text, "\n")
      while (TRUE) {
        text <- self$write_input(text)
        if (!length(text)) {
          break
        } 
      }
    },
    last_stdout = "",
    last_stderr = "",
    last_msg = list(),
    callbacks = list(),
    receive_output = function(timeout = 1){
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
    receive_to_prompt = function(timeout = 1){
      # log_out("receive_to_prompt")
      resp <- list()
      self$prompt_found <- FALSE
      while (!self$prompt_found) {
        poll_res <- self$poll_io(timeout)
        if (poll_res[1] == "ready") {
          sout <- self$read_output()
          if (endsWith(sout, self$prompt)) {
            self$prompt_found <- TRUE
            sout <- self$drop_prompt(sout)
          }
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
      }
      return(resp)
    },
    process_output = function(drop_echo=TRUE){
        # log_out("process_output")
        next_line <- FALSE
        cnt <- 0
        while(!next_line){
          cnt <- cnt + 1
          resp <- self$receive_output(timeout = 1)
          if (!is.null(resp$msg) && is.function(self$callbacks$msg)) {
            self$callbacks$msg(resp$msp)
          }
          if (!is.null(resp$stderr) && is.function(self$callbacks$stderr)) {
            self$callbacks$stderr(resp$stderr)
          }
          if (!is.null(resp$stdout)) {
            if (drop_echo && cnt == 1) {
              resp$stdout <- drop_echo(resp$stdout)
            }
            if (endsWith(resp$stdout, self$co_prompt)) {
              # log_out("Found continuation prompt")
              resp$stdout <- NULL
              next_line <- TRUE
            } else if (endsWith(resp$stdout, self$prompt)) {
              # log_out("Found main prompt")
              self$prompt_found <- TRUE
              resp$stdout <- remove_suffix(resp$stdout, self$prompt)
              if (is.function(self$callbacks$stdout)) {
                self$callbacks$stdout(resp$stdout)
              }
              next_line <- TRUE
            } else if (endsWith(resp$stdout, self$menu_prompt)) {
              # log_out("Found menu prompt")
              # resp$stdout <- remove_suffix(resp$stdout, self$menu_prompt)
              if (is.function(self$callbacks$menu)) {
                self$callbacks$menu(resp$stdout)
              }
            } else if (grepl(self$browse_prompt, resp$stdout)) {
              # log_out("Found browser prompt")
              if (is.function(self$callbacks$browse)) {
                self$callbacks$browse(resp$stdout)
              }
            } else {
              if (is.function(self$callbacks$stdout)) {
                self$callbacks$stdout(resp$stdout)
              }
            }
          }
        }

    },
    receive_and_process_output = function(timeout = 1){
        # log_out("receive_and_process_output")
        resp <- self$receive_all_output(timeout)
        log_out(resp, use.str = TRUE)
        self$process_output(resp, drop_echo = FALSE)
    },
    drop_prompt = function(txt){
      if (length(txt) && nzchar(txt)) {
        if (endsWith(txt, self$prompt)) {
          txt <- gsub(self$prompt_regex, "", txt)
        }
      }
      txt
    },
    drop_co_prompt = function(txt) {
      if (length(txt) && nzchar(txt)) {
        if (endsWith(txt, self$co_prompt)) {
          txt <- gsub(self$co_prompt_regex, "", txt)
        }
      }
      txt
    }
  )
)

split_lines1 <- function(x) {
  y <- strsplit(x, "\n", fixed = TRUE)
  unlist(y)
}

drop_echo <- function(txt) {
  if(length(txt) && nzchar(txt)){
      out_lines <- split_lines1(txt)
      out_lines <- out_lines[-1]
      txt <- paste(out_lines, collapse = "\n")
      # log_out(resp, use.print = TRUE)
  }
  txt
}