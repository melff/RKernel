
WidgetClass <- R6Class_("Widget",
  inherit = HasTraits,
  public = list(
    `_model_id` = Unicode(character(0),sync=TRUE),
    `_model_name` = Unicode("WidgetModel",sync=TRUE),
    `_model_module` = Unicode("@jupyter-widgets/base",sync=TRUE),
    `_model_module_version` = Unicode(jupyter_widgets_base_version,sync=TRUE),
    `_view_name` = Unicode(character(0),sync=TRUE),
    `_view_module` = Unicode(character(0),sync=TRUE),
    `_view_module_version` = Unicode(character(0),sync=TRUE),
    `_view_count` = Unicode(integer(0),sync=TRUE),
    initialize = function(...){
      super$initialize()
      add_displayed_classes(class(self)[1])
      self$open()
    },
    open = function(){
      if(is.null(self$model_id)){
        manager <- get_comm_manager()
        if(!manager$has_handlers("jupyter.widget"))
          manager$add_handlers("jupyter.widget",list(self$handle_comm_opened))
        self$comm <- manager$new_comm("jupyter.widget")
        state <- self$get_state()
        data <- list(state=state)
        self$comm$open(data=data)
        state$state[["_model_id"]] <- self$comm$id
      }
    },
    finalize = function(){
      self$close()
    },
    close = function(){
      if(!is.null(self$comm))
        self$comm$close()
    },
    get_state = function(keys=NULL){
      state <- super$get_state()
      if(is.null(keys))
        keys <- names(state)
      for(k in keys){
        state[[k]] <- to_json(state[[k]])
      }
      return(state)
    },
    send_state = function(keys=NULL){
      state <- self$get_state(keys)
      msg <- list(method="update",
                  state=state)
      self$`_send`(msg)
    },
    notify = function(change_name){
      self$send_state(change_name)
    },
    send = function(content){
      msg <- list(method="custom","content"=content)
      self$`_send`(msg)
    },
    display_data = function(){
      data <- list("text/plain" = class(self)[1])
      if(length(self[["_view_name"]]))
        data[["application/vnd.jupyter.widget-view+json"]] <- list(
          version_major = 2,
          version_minor = 0,
          model_id = self$model_id
        )
      return(data)
    },
    handle_comm_opened = function(comm,msg){
      data <- msg$content$data
      state <- data$state
      print(msg)
    },
    `_comm` = NULL,
    `_send` = function(msg){
      if(!is.null(self$`_comm`))
        self$`_comm`$send(msg)
    }
  ),
  active = list(
    comm = function(value){
      if(missing(value)) return(self$`_comm`)
      else self$`_comm` <- value
    },
    model_id = function() self$comm$id
  )
)

#' @export
Widget <- function(...) WidgetClass$new(...)

#' @export
display.Widget <- function(x,...,
                           metadata=NULL,
                           id=d$model_id,
                           update=FALSE){
  d <- list(data=x$display_data())
  d$metadata <- metadata
  d$transient <- list(display_id=id)
  if(update) cl <- "update_display_data"
  else cl <- "display_data"
  structure(d,class=cl)
}

#' @importFrom jsonlite fromJSON toJSON

to_json <- function(x) UseMethod("to_json")
to_json.default <- function(x) {
  attributes(x) <- NULL
  x
}

to_json.Widget <- function(x){
  paste0("IPY_MODEL_",x$model_id)
}

# Local Variables:
# ess-indent-offset: 2
# End:
