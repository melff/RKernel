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
    prompt_found = FALSE,
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
      super$initialize(options = options)
      self$poll_conns <- list(
        stdout = self$get_output_connection(),
        stderr = self$get_error_connection(),
        ctrl = self$get_poll_connection()
      )
      self$callbacks <- callbacks
    },
    run_code = function(code, prompt="> "){
      lines <- split_lines1(code)
      n_lines <- length(lines)
      for (i in 1:n_lines) {
        self$prompt_found <- FALSE
        line <- lines[i]
        self$send_input(line)
        log_out(paste0("Line:", line))
        if (i < n_lines) {
              resp <- self$receive_output(timeout = -1)
              log_out(paste0("Response:", resp$stdout))
              self$process_output(resp, drop_echo = TRUE)
        }
        else {
            drop_echo <- TRUE
            while(!self$prompt_found) {
              resp <- self$receive_output(timeout = -1)
              log_out(paste0("Response:", resp$stdout))
              self$process_output(resp, drop_echo = drop_echo)
              drop_echo <- FALSE
            }
        }
      }
    },
    send_input = function(text) {
      text <- paste0(text, "\n")
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
    receive_output = function(timeout = 0){
      poll_res <- self$poll_io(timeout)
      log_out(poll_res,use.print=TRUE)
      res <- list()
      if (poll_res[1] == "ready") {
        sout <- self$read_output()
        res$stdout <- sout
      }
      if (poll_res[2] == "ready") {
        serr <- self$read_error()
        res$stderr <- serr
      }
      if (poll_res[3] == "ready") {
        msg <- self$read()
        res$msg <- msg
      }
      return(res)
    },
    process_output = function(resp, drop_echo=TRUE){
        if (!is.null(resp$msg) && is.function(self$callbacks$msg)) {
          self$callbacks$msg(resp$msp)
        }
        if (!is.null(resp$stderr) && is.function(self$callbacks$stderr)) {
          self$callbacks$stderr(resp$stderr)
        }
        if (!is.null(resp$stdout)) {
          if (drop_echo) {
            out_lines <- split_lines1(resp$stdout)
            out_lines <- out_lines[-1]
            sout <- paste(out_lines, collapse = "\n")
          }
          else sout <- resp$stdout
          if (endsWith(sout, self$co_prompt)){
            sout <- gsub(self$co_prompt_regex, "", sout)
          }
          if (endsWith(sout, self$prompt)) {
            self$prompt_found <- TRUE
            sout <- gsub(self$prompt_regex, "", sout)
          }
          if (is.function(self$callbacks$stdout))
             self$callbacks$stdout(sout)
        }
    }
  )
)

split_lines1 <- function(x){
  y <- strsplit(x, "\n", fixed = TRUE)
  unlist(y)
}