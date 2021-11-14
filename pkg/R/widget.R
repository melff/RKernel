#' @include json.R

#' @export
CallbackDispatcherClass <- R6Class("CallbackDispatcher",
    public = list(
      callbacks = list(),
      register = function(handler,remove){
        old <- self$callbacks
        new <- list()
        for(cb in old){
          if(!identical(cb,handler))
            new <- append(new,cb)
        }
        if(!remove){
          new <- append(new,handler)
        }
        self$callbacks <- new
      },
      run = function(...){
        for(cb in self$callbacks){
            cb(...)
        }
      }
    )
)


#' @export
CallbackDispatcher <- function(...) CallbackDispatcherClass$new(...)

#' @export
WidgetClass <- R6Class_("Widget",
  inherit = HasTraits,
  public = list(
    #`_model_id` = structure(Unicode(character(0)),sync=TRUE),
    `_model_id` = character(0),
    `_model_name` = structure(Unicode("WidgetModel"),sync=TRUE),
    `_model_module` = structure(Unicode("@jupyter-widgets/base"),sync=TRUE),
    `_model_module_version` = structure(Unicode(jupyter_widgets_base_version),sync=TRUE),
    `_view_name` = structure(Unicode(character(0)),sync=TRUE),
    `_view_module` = structure(Unicode(character(0)),sync=TRUE),
    `_view_module_version` = structure(Unicode(character(0)),sync=TRUE),
    `_view_count` = structure(Unicode(integer(0)),sync=TRUE),
    traits_to_sync = character(0),
    callbacks = list(),
    initialize = function(...,open=TRUE){
      super$initialize(...)
      for(tn in names(self$traits)){
        if(isTRUE(attr(self$traits[[tn]],"sync"))){
          self$traits_to_sync <- append(self$traits_to_sync,tn)
          handler <- function(tn,trait,value){
            # print(str(list(tn=tn,trait=trait,value=value)))
            self$send_state(tn)
          }
          self$observe(tn,handler)
        }
      }
      self$callbacks <- CallbackDispatcher()
      kernel <- get_current_kernel()
      #browser()
      if(length(kernel)){
        add_displayed_classes(class(self)[1])
        if(open) self$open()
      }
    },
    open = function(){
      if(!length(self[["_model_id"]])){
        manager <- get_comm_manager()
        if(!manager$has_handlers("jupyter.widget"))
          manager$add_handlers("jupyter.widget",list(self$handle_comm_opened))
        self$comm <- manager$new_comm("jupyter.widget")
        self[["_model_id"]] <- self$comm$id
        state <- self$get_state()
        data <- list(state=state)
        metadata <- list(version=jupyter_widgets_protocol_version)
        self$comm$open(data=data,metadata=metadata)
        self$comm$handlers$open <- self$handle_comm_opened
        self$comm$handlers$msg <- self$handle_comm_msg
      } else print(self[["_model_id"]])
    },
    finalize = function(){
      self$close()
    },
    close = function(){
      if(!is.null(self$comm)){
        self$comm$close()
        self[["_model_id"]] <- character(0)
      }
    },
    get_state = function(keys=NULL,drop_defaults=FALSE){
      state <- list()
      if(is.null(keys))
        keys <- names(self$traits)
      for(k in keys){
        if(k %in% self$traits_to_sync &&
          !(drop_defaults && self$traits[[k]]$is_default())){
          state[[k]] <- to_json(self$traits[[k]])
          # state[[k]] <- to_json(self[[k]])
        }
      }
      return(state)
    },
    set_state = function(state){
      keys <- names(state)
      for(k in keys){
        # cat("Updating",k)
        if(k %in% self$traits_to_sync)
          self[[k]] <- state[[k]]
      }
    },
    send_state = function(keys=NULL,drop_defaults=FALSE){
      # cat("send_state")
      state <- self$get_state(keys)
      if(length(state)){
        msg <- list(method="update",
                    state=state)
        self$`_send`(msg)
      }
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
          model_id = self[["_model_id"]]
        )
      return(data)
    },
    handle_comm_opened = function(comm,data){
      state <- data$state
    },
    handle_comm_msg = function(comm,data){
      # print(data)
      # cat("=====================\n")
      method <- data$method
      if(method=="update"){
        # cat("------------------\n")
        state <- data$state
        # print(state)
        self$set_state(state)
      }
      else if(method=="request_state")
        self$send_state()
      else if(method=="custom"){
        if("content" %in% names(data))
          self$handle_custom_msg(data$content)
      }
    },
    handle_custom_msg = function(content){
      value <- self$callbacks$run(self,content)
      if(length(value))
        return(value)
    },
    on_msg = function(handler,remove=FALSE){
      self$callbacks$register(handler,remove)
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
    }
  )
)

#' @export
Widget <- function(...) WidgetClass$new(...)

#' @export
display.Widget <- function(x,...,
                           metadata=NULL,
                           id=uuid::UUIDgenerate(),
                           update=FALSE){
  d <- list(data=x$display_data())
  d$metadata <- metadata
  d$transient <- list(display_id=id)
  if(update) cl <- "update_display_data"
  else cl <- "display_data"
  structure(d,class=cl)
}

#' @export
to_json.Widget <- function(x){
  paste0("IPY_MODEL_",x[["_model_id"]])
}


# Local Variables:
# ess-indent-offset: 2
# End:
