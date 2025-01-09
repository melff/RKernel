widget_readline <- function(kernel,prompt="") {
  session <- kernel$r_session
  repl <- kernel$r_repl
  parent <- kernel$save_shell_parent()
  input <- TextWidget(
                placeholder="Enter line or hit the quit button to abort/interrupt",
                continuous_update = TRUE,
                layout = Layout(width="100%")
            )
  quit_btn <- Button(icon="stop",
                     style = ButtonStyle(font_size="70%"),
                     layout = Layout(width = "min-content")
                    )
  if(length(prompt) && nzchar(prompt)) {
    label <- Label(prompt)
    hbox <- HBox(label, quit_btn, input, layout = Layout(width="100%"))
  } else {
    hbox <- HBox(quit_btn, input, layout = Layout(width="100%"))
  }
  self <- new.env()
  quit_action <- function() {
      self$retval <- ""
      self$continue_loop <- FALSE
      kernel$r_repl$interrupt()
  }
  submit_action <- function() {
      self$retval <- input$value
      self$continue_loop <- FALSE
  }
  input$on_submit(submit_action)
  quit_btn$on_click(quit_action)
  d <- display_data(hbox)
  kernel$display_send(d)
  self$continue_loop <- TRUE
  while(self$continue_loop) {
      session$yield(1000)
  }
  # log_out("Exited")
  kernel$restore_shell_parent(parent)
  d <- update(d, "text/plain"="", "text/html"="")
  kernel$display_send(d)
  return(self$retval)
}