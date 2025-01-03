
EOT <- "\x04"
DLE <- "\x10"
ETB <- "\x17"

MessageFilter <- R6Class("MessageFilter",
  public = list(
    handle_text = NULL,
    handle_msg = NULL,
    initialize = function(text_handler, msg_handler) {
      self$handle_text <- text_handler
      self$handle_msg  <- msg_handler
    }, 
    msg_incomplete = FALSE,
    msg_frag = "",
    process = function(text) {
      if (grepl(DLE, text)) {
        log_out("DLE found")
        text <- split_string1(text, DLE)
      }
      for(chunk in text){
        if(!length(chunk) || !nzchar(chunk)) next
        # log_out(chunk, use.str = TRUE)
        if (startsWith(chunk, MSG_BEGIN)) {
          log_out("MSG_BEGIN found")
          # log_out(chunk, use.print = TRUE)
          if (endsWith(chunk, MSG_END)) {
            log_out("MSG_END found")
            msg <- remove_prefix(chunk, MSG_BEGIN) |> remove_suffix(MSG_END)
            msg <- msg_unwrap(msg)
            # log_out(msg, use.print = FALSE)
            self$handle_msg(msg)
          } else {
            self$msg_incomplete <- TRUE
            self$msg_frag <- remove_prefix(chunk, MSG_BEGIN)
          }
        }
        else if(endsWith(chunk, MSG_END)){
          log_out("MSG_END found")
          log_out(chunk, use.print = TRUE)
          msg <- paste0(self$msg_frag, remove_suffix(chunk, MSG_END))
          self$msg_incomplete <- FALSE
          self$msg_frag <- ""
          msg <- msg_unwrap(msg)
          self$handle_msg(msg)
        }
        else {
          if(self$msg_incomplete) {
            self$msg_frag <- paste0(self$msg_frag, chunk)
          }
          else if(nzchar(chunk)) {
            self$handle_text(chunk)
          }
        }
      }
    }
  )
)