Menu <- function(kernel, args, ...) {
  # log_out("Menu with args:")
  # log_out(args,use.str=TRUE)
  # log_out(sprintf("debugging_state$depth = %d",debugging_state$depth))
  if(debugging_state$depth == 0L) {
    m <- MenuWidgetClass$new(kernel, args, ...)
    m$run()
  } else {
    log_warning("Menu from within browser not supported yet")
    # TODO: Provide support for this ...
    kernel$session$send_input("0L", drop_echo = TRUE)
  }
}

MenuWidgetClass <- R6Class("MenuWidget",
    public = list(
        choices = NULL,
        preselect = NULL,
        multiple = NULL,
        title = NULL,
        button_labels = c("OK","Cancel"),
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
            self$multiple <- args$multiple
            self$preselect <- args$preselect
            self$button_labels <- args$buttons
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
            multiple <- self$multiple
            rows <- min(nc, getOption("rowsListBox",10L))
            if(multiple) {
              self$listbox <- ListBoxMultiple(options=choices,value="",rows=rows)
              if(length(self$preselect)) {
                preselect <- intersect(self$preselect, self$choices)
                ind <- match(preselect,self$choices)
                self$listbox$index <- ind - 1L
                self$index <- ind
              }
            } else {
              self$listbox <- ListBox(options=choices,
                                      value="",
                                      rows=rows,
                                      layout=Layout(width="95%"))
            }
            ok_button <- Button(description=self$button_labels[1])
            ok_button$on_click(self$on_ok)
            cancel_button <- Button(description=self$button_labels[2])
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
            d_id <- display_id(d)
            kernel$display_send(d)
            while(self$continue_loop) {
                kernel$session$yield(1000)
            }
            self$listbox$disabled <- TRUE
            if(any(self$index > 0L)) {
              value <- paste(self$listbox$value, collapse=", ")
            } else {
              value <- "--"
            }
            d <- display_data("text/plain"="",
                              "text/html"="",
                              id = d_id,
                              update = TRUE)
            kernel$display_send(d)
            res <- deparse(self$index)
            kernel$session$send_input(res, drop_echo = TRUE)
        }
    )
)


request_menu_widget <- function(choices,
                                preselect=NULL,
                                multiple=FALSE,
                                title=NULL,
                                buttons = c("OK","Cancel"),
                                file=stdout(),
                                ...) {
  # log_out("request_menu_widget")
  msg <- list(
    type = "menu",
    content = list(
      choices = choices,
      preselect = preselect,
      title = title,
      buttons = buttons,
      multiple = multiple)
  )
  # log_out(msg,use.str=TRUE)
  msg_send(msg, file=file)
  ind <- readline()
  eval(str2expression(ind))
}

menu_orig <- getFromNamespace("menu","utils")

menu_ <- function(choices,graphics=FALSE,title=NULL) {
    if(get_config("use_widgets")) {
      nc <- length(choices)
      labs <- formatC(1:nc,flag="0",format="d",digits=as.integer(nc>=10))
      choices <- paste(labs,choices,sep=": ")
      ind <- request_menu_widget(choices,title=title)
    } else {
      ind <- menu__(choices,graphics=graphics,title=title)
    }
    return(ind)
}

menu__ <- function(choices,title=NULL,...) {
    # Needed because it is impossible to spot menues based on a specific
    # prompt without encountering many false positives.
    nc <- length(choices)
    if (length(title) && nzchar(title[1L])) 
        cat(title[1L], "\n")
    op <- paste0(format(seq_len(nc)), ": ", choices)
    if (nc > 10L) {
        fop <- format(op)
        nw <- nchar(fop[1L], "w") + 2L
        ncol <- getOption("width")%/%nw
        if (ncol > 1L) 
            op <- paste0(fop, c(rep.int("  ", min(nc, ncol) - 
                1L), "\n"), collapse = "")
    }
    cat("", op, "", sep = "\n")
    repeat {
        resp <- readline()
        if(grepl("[0-9]+",resp)) {
            ind <- as.integer(resp)
            if (ind <= nc) 
                return(ind)
        }
        cat(gettext("Enter an item from the menu, or 0 to exit\n"))
    }
}


select_list_orig <- getFromNamespace("select.list","utils")

select_list <- function(choices, preselect = NULL, multiple = FALSE, title = NULL, 
                         graphics = getOption("menu.graphics")) {
    if(get_config('use_widgets')) {
      ind <- request_menu_widget(choices,
                        preselect = preselect,
                        multiple = multiple,
                        title = title
                        )
      value <- choices[ind]
    } else {
      value <- select_list_orig(choices,
                              preselect = preselect,
                              multiple = multiple,
                              title = title,
                              graphics=graphics)
    }
    return(value)
}


#' @export
install_menu <- function(){
    replace_in_package("utils", "menu", menu_)
    replace_in_package("utils", "select.list", select_list)
}
