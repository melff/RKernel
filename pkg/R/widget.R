#' @include json.R utils.R callbacks.R

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
    initialize = function(...,open=TRUE){
      super$initialize(...)
      handler <- function(tn,trait,value){
            # print(str(list(tn=tn,trait=trait,value=value)))
            # log_out(sprintf("observed change in trait '%s' to value '%s'",tn,value))
            self$send_state(tn)
      }
      for(tn in names(self$traits)){
        if(isTRUE(attr(self$traits[[tn]],"sync"))){
          self$traits_to_sync <- append(self$traits_to_sync,tn)
          self$observe(tn,handler)
        }
      }
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
        buffer_paths <- attr(state,"buffer_paths")
        buffers <- attr(state,"buffers")
        data <- list(state=state,
                     buffer_paths=buffer_paths)
        metadata <- list(version=jupyter_widgets_protocol_version)
        self$comm$open(data=data,metadata=metadata,buffers=buffers)
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
      buffer_paths <- list()
      buffers <- list()
      if(is.null(keys))
        keys <- names(self$traits)
      for(k in keys){
        if(k %in% self$traits_to_sync &&
          !(drop_defaults && self$traits[[k]]$is_default())){
          auto_unbox <- !isFALSE(attr(self$traits[[k]],"auto_unbox"))
          if(inherits(self$traits[[k]],"Bytes")){
            buffer_paths <- append(buffer_paths,list(list(k)))
            bytes <- self$traits$value$get()
            buffers <- append(buffers,list(bytes))
          }
          else
            state[[k]] <- to_json(self$traits[[k]],auto_unbox=auto_unbox)
        }
      }
      res <- structure(state,
                       buffer_paths=buffer_paths,
                       buffers=buffers)
      return(res)
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
      state <- self$get_state(keys)
      buffer_paths <- attr(state,"buffer_paths")
      buffers <- attr(state,"buffers")
      if(length(state) || length(buffer_paths)){
        msg <- list(method="update",
                    state=state,
                    buffer_paths=buffer_paths)
        # log_out("send_state")
        # log_out(msg,use.print=TRUE)
        # log_out(buffers,use.print=TRUE)
        self$`_send`(msg,buffers=buffers)
      }
    },
    send = function(content,buffers=NULL){
      msg <- list(method="custom","content"=content)
      self$`_send`(msg,buffers=buffers)
    },
    display_data = function(){
      data <- list("text/plain" = class(self)[1])
      if(length(self[["_view_name"]]))
        data[["application/vnd.jupyter.widget-view+json"]] <- list(
          version_major = 2,
          version_minor = 0,
          model_id = self[["_model_id"]]
        )
      self$handle_displayed()
      return(data)
    },
    handle_comm_opened = function(comm,data){
      state <- data$state
    },
    handle_comm_msg = function(comm,data){
      # print(data)
      # log_out("=====================\n")
      # log_out(data,use.print=TRUE)
      # log_out(str(data),use.print=TRUE)
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
      if("event" %in% names(content)){
        event <- content$event
        args <- content[names(content)!="event"]
        self$handle_event(event,args)
      } else
        self$custom_msg_callbacks$run(content)
    },
    custom_msg_callbacks = list(),
    on_msg = function(handler,remove=FALSE){
      if(!length(self$custom_msg_callbacks))
        self$custom_msg_callbacks <- CallbackDispatcher()
      self$custom_msg_callbacks$register(handler,remove)
    },
    event_callbacks = list(),
    on_event = function(event,handler,remove=FALSE){
      if(!event %in% names(self$event_callbacks))
        self$event_callbacks[[event]] <- CallbackDispatcher()
      self$event_callbacks[[event]]$register(handler,remove)
    },
    handle_event = function(event,args){
      event_callbacks <- self$event_callbacks[[event]]
      if(inherits(event_callbacks,"CallbackDispatcher"))
        do.call(event_callbacks$run,args)
    },
    displayed_callbacks = list(),
    on_displayed = function(handler,remove=FALSE){
      if(!length(self$displayed_callbacks))
        self$displayed_callbacks <- CallbackDispatcher()
      self$displayed_callbacks$register(handler,remove)
    },
    handle_displayed = function(){
      if(inherits(self$displayed_callbacks,"CallbackDispatcher"))
        self$displayed_callbacks$run()
    },
    `_comm` = NULL,
    `_send` = function(msg,buffers=NULL){
      if(!is.null(self$`_comm`)){
        # log_out("_send")
        # log_out(msg,use.print=TRUE)
        # log_out(buffers,use.print=TRUE)
        self$`_comm`$send(msg,buffers=buffers)
      }
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
display_data.Widget <- function(x,...,
                           metadata=emptyNamedList,
                           id=uuid(),
                           update=FALSE){
  d <- list(data=x$display_data())
  d$metadata <- metadata
  d$transient <- list(display_id=id)
  if(update) cl <- "update_display_data"
  else cl <- "display_data"
  structure(d,class=cl)
}

#' @export
to_json.Widget <- function(x,...){
  paste0("IPY_MODEL_",x[["_model_id"]])
}


# Local Variables:
# ess-indent-offset: 2
# End:
