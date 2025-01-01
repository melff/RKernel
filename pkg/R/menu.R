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
    kernel$r_session$send_input("0L", drop_echo = TRUE)
  }
}

MenuWidgetClass <- R6Class("MenuWidget",
    public = list(
        choices = NULL,
        preselect = NULL,
        multiple = NULL,
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
            self$multiple <- args$multiple
            self$preselect <- args$preselect
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
              self$listbox <- ListBox(options=choices,value="",rows=rows)
            }
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
            if(any(self$index > 0L)) {
              value <- paste(self$listbox$value, collapse=", ")
            } else {
              value <- "--"
            }
            self$main_widget <- TextWidget(value=value,
                                                 disabled=TRUE)
            d <- update(d,self$main_widget)
            kernel$display_send(d)
            res <- deparse(self$index)
            kernel$r_session$send_input(res, drop_echo = TRUE)
        }
    )
)


request_menu_widget <- function(choices,
                                preselect=NULL,
                                multiple=FALSE,
                                title=NULL,
                                file=stdout(),
                                ...) {
  # log_out("request_menu_widget")
  msg <- list(
    type = "menu",
    content = list(
      choices = choices,
      preselect = preselect,
      title = title,
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
      ind <- menu_orig(choices,graphics=graphics,title=title)
    }
    return(ind)
}

select_list_orig <- getFromNamespace("select.list","utils")

select_list_ <- function(choices, preselect = NULL, multiple = FALSE, title = NULL, 
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
    replace_in_package("utils", "select.list", select_list_)
}
