Menu <- function(kernel, args, use_widgets = TRUE, ...) {
  if(use_widgets) {
    m <- MenuWidgetClass$new(kernel, args, ...)
  } else {
    m <- MenuSimpleClass$new(kernel, args, ...)
  }
  m$run()
}

MenuSimpleClass <- R6Class("MenuSimple", 
  public = list(
    title = NULL,
    choices = NULL,
    opts = NULL,
    nc = NULL,
    kernel = NULL,
    initialize = function(kernel, args, ...) {
      # log_out("MenuSimple$initialize")
      # log_out(args, use.str=TRUE)
      choices <- unlist(args$choices)
      width <- args$width
      nc <- length(choices)
      op <- paste0(format(seq_len(nc)), ": ", choices)
      if (nc > 10L) {
          fop <- format(op)
          nw <- nchar(fop[1L], "w") + 2L
          ncol <- width%/%nw
          if (ncol > 1L) 
              op <- paste0(fop, c(rep.int("  ", min(nc, ncol) - 
                  1L), "\n"), collapse = "")
      }
      self$title <- args$title
      self$choices <- choices
      self$opts <- op
      self$nc <- nc
      self$kernel <- kernel
      # log_out("Done")
    },
    run = function() {
      # log_out("MenuSimple$run")
      title <- self$title
      nc <- self$nc
      kernel <- self$kernel
      op <- self$opts
      # log_out(op, use.str=TRUE)
      if (length(title) && nzchar(title[1L])) 
        kernel$stdout(title[1L], "\n")
      kernel$stdout(paste0(" ", op, "\n", collapse = ""))
      repeat {
        resp <- kernel$readline(gettext("Enter an item from the menu, or 0 to exit\n"))
        if(grepl("[0-9]+",resp)) {
            ind <- as.integer(resp)
            if (ind <= nc) {
              kernel$r_session$send_input(resp, drop_echo = TRUE)
              # log_out("Done")
              # log_out(resp)
              return(TRUE)
            }
        }
      } 
      # log_out("Done")
    }
  ))



send_menu_request <- function(choices,title,...) {
  msg <- list(
    type = "menu",
    content = list(
      choices = choices,
      title = title,
      width = getOption("width"))
  )
  msg_send(msg)
}

menu_orig <- getFromNamespace("menu","utils")

menu_ <- function(choices,title=NULL,...) {
    send_menu_request(choices,title,...)
    #ind <- scan(what=integer(), nmax = 1L)
    ind <- readline_()
    ind <- as.integer(ind)
    return(ind)
}

#' @export
install_menu <- function(){
    replace_in_package("utils", "menu", menu_)
}

MenuWidgetClass <- R6Class("MenuWidget",
    public = list(
        choices = NULL,
        allow_none = NULL,
        title = NULL,
        index = 0,
        value = NULL,
        main_widget = NULL,
        listbox = NULL,
        kernel = NULL,
        continue_loop = TRUE,
        initialize = function(kernel, args) {
            self$choices <- unlist(args$choices)
            self$title <- args$title
            self$kernel <- kernel
        },
        on_ok = function() {
            ind <- self$listbox$index + 1L # widget indices are zero-based
            self$continue_loop <- FALSE
            self$index <- ind
        },
        on_cancel = function() {
            self$continue_loop <- FALSE
            self$index <- 0L
        },
        run = function() {
            kernel <- self$kernel
            choices <- self$choices
            nc <- length(choices)
            labs <- format(1:nc,justify="right")
            ops_labs <- paste0(labs,": ",choices)
            self$listbox <- ListBox(options=ops_labs,value="",rows=nc)
            ok_button <- Button(description="OK")
            ok_button$on_click(self$on_ok)
            cancel_button <- Button(description="Cancel")
            cancel_button$on_click(self$on_cancel)
            if(length(self$title)) {
              label <- Label(self$title)
              self$main_widget <- VBox(label,self$listbox,
                                       HBox(ok_button,cancel_button))
            }
            else {
              label <- NULL
              self$main_widget <- VBox(self$listbox,
                                       HBox(ok_button,cancel_button))
            }
            d <- display_data(self$main_widget)
            kernel$display_send(d)
            while(self$continue_loop) {
                kernel$r_session$yield(1000)
            }
            self$listbox$disabled <- TRUE
            self$main_widget <- TextWidget(value=self$listbox$value,disabled=TRUE)
            d <- update(d,self$main_widget)
            kernel$display_send(d)
            res <- as.character(self$index)
            kernel$r_session$send_input(res, drop_echo = TRUE)
        }
    )
)
