Menu <- function(kernel, args, ...) {
  m <- MenuWidgetClass$new(kernel, args, ...)
  m$run()
}



send_menu_request <- function(choices,title,multiple=FALSE,...) {
  msg <- list(
    type = "menu",
    content = list(
      choices = choices,
      title = title,
      multiple = multiple)
  )
  msg_send(msg)
}

menu_orig <- getFromNamespace("menu","utils")

menu_ <- function(choices,title=NULL,...) {
    if(get_config('use_widgets')) {
      nc <- length(choices)
      choices <- paste(format(1:nc,justify="right"),choices,sep=": ")
      send_menu_request(choices,title,...)
      ind <- readline()
      ind <- as.integer(ind)
    } else {
      ind <- menu_orig(choices,title=title,...)
    }
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
            self$listbox <- ListBox(options=choices,value="",rows=nc)
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

# labs <- format(1:nc,justify="right")
# ops_labs <- paste0(labs,": ",choices)