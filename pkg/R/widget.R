#' A Widget Base Class
#'
#' @description The base class from which all widget classes are derived
#' 
#' @include json.R utils.R callbacks.R
#' @name Widgets

#' @rdname Widgets
#' @export
WidgetClass <- R6Class_("Widget",
  inherit = HasTraits,
  public = list(
    #' @field _model_id Identifyer of the frontend Javascript object
    `_model_id` = character(0),
    #' @field _model_name Name of the Javascript model in the frontend
    `_model_name` = structure(Unicode("WidgetModel"),sync=TRUE),
    #' @field _model_module Name of the Javascript module with the model
    `_model_module` = structure(Unicode("@jupyter-widgets/base"),sync=TRUE),
    #' @field _model_module_version Version of the module where the model is defined
    `_model_module_version` = structure(Unicode(jupyter_widgets_base_version()),sync=TRUE),
    #' @field _view_name Name of the Javascript model view in the frontend
    `_view_name` = structure(Unicode(character(0)),sync=TRUE),
    #' @field _view_module Version of the module where the view is defined
    `_view_module` = structure(Unicode(character(0)),sync=TRUE),
    #' @field _view_module_version Version of the module where the view is defined
    `_view_module_version` = structure(Unicode(character(0)),sync=TRUE),
    #' @field _view_count Number of views that refer to the same frontend model object
    `_view_count` = structure(Unicode(integer(0)),sync=TRUE),
    #' @field traits_to_sync Names of the traits to be synchronized with the frontend
    traits_to_sync = character(0),
    #' @field sync_suspended Logical value, whether synchronization is suspended
    sync_suspended = FALSE,
    #' @description Initialize an object
    #' @param ... Values used for initialization
    #' @param open Logical, whether a connection with the frontend should be opened
    initialize = function(...,open=TRUE){
      self$check_version()
      super$initialize(...)
      handler <- function(tn,trait,value){
            # print(str(list(tn=tn,trait=trait,value=value)))
            #if(is.environment(value))
            #  value <- capture.output(print(value))
            #log_out(sprintf("observed change in trait '%s' to value '%s'",tn,value))
            if(!self$sync_suspended)
              self$send_state(tn)
      }
      for(tn in names(self$traits)){
        if(isTRUE(attr(self$traits[[tn]],"sync"))){
          self$traits_to_sync <- append(self$traits_to_sync,tn)
          self$observe(tn,handler)
        }
      }
      add_displayed_classes(class(self)[1])
      if(open) self$open()
    },
    #' @description Open a connection to the frontend
    open = function(){
      # log_out("widget$open()")
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
        metadata <- list(version=jupyter_widgets_protocol_version())
        self$comm$open(data=data,metadata=metadata,buffers=buffers)
        self$comm$handlers$open <- self$handle_comm_opened
        self$comm$handlers$msg <- self$handle_comm_msg
      } else print(self[["_model_id"]])
    },
    #' @description Finalize the object
    finalize = function(){
      self$close()
    },
    #' @description Close the connection to the frontend
    close = function(){
      if(!is.null(self$comm)){
        self$comm$close()
        self[["_model_id"]] <- character(0)
      }
    },
    #' @description Prepare synchronized traits for sending them to the frontend
    #' @param keys Keys/names of the traits to be updated in the frontend
    get_state = function(keys=NULL){
      state <- list()
      buffer_paths <- list()
      buffers <- list()
      if(is.null(keys))
        keys <- names(self$traits)
      for(k in keys){
        if(k %in% self$traits_to_sync){
          auto_unbox <- !isFALSE(attr(self$traits[[k]],"auto_unbox"))
          to_json_ <- attr(self$traits[[k]],"to_json")
          if(inherits(self$traits[[k]],"Bytes")){
            buffer_paths <- append(buffer_paths,list(list(k)))
            bytes <- self$traits$value$get()
            buffers <- append(buffers,list(bytes))
          }
          if(is.function(to_json_))
            state[[k]] <- to_json_(self$traits[[k]])
          else
            state[[k]] <- to_json(self$traits[[k]],auto_unbox=auto_unbox)
        }
      }
      res <- structure(state,
                       buffer_paths=buffer_paths,
                       buffers=buffers)
      return(res)
    },
    #' @description Update the synchronized states, usually with information from the frontend
    #' @param state A list of values for the synchronized traits
    set_state = function(state){
      # log_out("widget$set_state")
      keys <- names(state)
      for(k in keys){
        if(!length(state[[k]])) {
          # log_out(paste("Skipping",k))
          next
        }
        # log_out(paste("Updating",k))
        if(k %in% self$traits_to_sync){
          from_json_ <- attr(self$traits[[k]],"from_json")
          if(!is.function(from_json_))
            from_json_ <- I
          self[[k]] <- from_json_(state[[k]])
        }
      }
    },
    #' @description Send updated traits to the frontend
    #' @param keys Keys/names of the traits to be updated in the frontend
    #' @param drop_defaults Logical value, not yet used
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
    #' @description Send content and binary buffers to the fronend
    #' @param content Some user-defined information to be send to the frontend
    #' @param buffers Some raw vector buffers
    send = function(content,buffers=NULL){
      msg <- list(method="custom","content"=content)
      self$`_send`(msg,buffers=buffers)
    },
    #' @description Send display-data of the widget to the frontend
    display_data = function(){
      data <- list("text/plain" = class(self)[1],
                   "text/latex"="")
      if(length(self[["_view_name"]]))
        data[["application/vnd.jupyter.widget-view+json"]] <- list(
          version_major = 2,
          version_minor = 0,
          model_id = self[["_model_id"]]
        )
      self$handle_displayed()
      return(data)
    },
    #' @description Handle a 'comm' opened in the frontend
    #' @param comm The 'comm' object that is opened
    #' @param data Data sent by the frontend
    handle_comm_opened = function(comm,data){
      # log_out("=====================")
      # log_out("handle_comm_opened")
      state <- data$state
    },
    #' @description Handle a message from the frontend
    #' @param comm The 'comm' object via which the message is received
    #' @param msg Message sent by the frontend
    handle_comm_msg = function(comm,msg){
      # print(data)
      # log_out("=====================")
      # log_out("handle_comm_msg")
      # log_out(data,use.print=TRUE)
      # log_out(msg,use.str=TRUE)
      method <- msg$method
      if(method=="update"){
        # cat("------------------\n")
        msg <- self$handle_buffers(msg)
        state <- msg$state
        # print(state
        self$sync_suspended <- TRUE
        self$set_state(state)
        self$sync_suspended <- FALSE
      }
      else if(method=="request_state")
        self$send_state()
      else if(method=="custom"){
        if("content" %in% names(msg))
          self$handle_custom_msg(msg$content)
      }
    },
    #' @description Handle buffers in message.
    #'   This method should be overwritten by inherting classes that actually
    #'   process data in buffer components of messages.
    #' @param msg A comm message
    handle_buffers = function(msg){
      buffers <- msg$buffers
      if(length(buffers)){
        warning("This widget class does not handle buffer data")
      }
      return(msg)
    },
    #' @description Call the custom message handlers
    #' @param content The data received
    handle_custom_msg = function(content){
      if("event" %in% names(content)){
        event <- content$event
        args <- content[names(content)!="event"]
        self$handle_event(event,args)
      } else
        self$custom_msg_callbacks$run(content)
    },
    #' @field custom_msg_callbacks A list of functions to be called on receiving a message
    custom_msg_callbacks = list(),
    #' @description Install a handler for messages being received
    #' @param handler A handler function
    #' @param remove Logical, should the handler be removed?
    on_msg = function(handler,remove=FALSE){
      if(!length(self$custom_msg_callbacks))
        self$custom_msg_callbacks <- CallbackDispatcher()
      self$custom_msg_callbacks$register(handler,remove)
    },
    #' @field event_callbacks A list of functions to be called on an event
    event_callbacks = list(),
    #' @description Install a handler for events in the frontend
    #' @param event A character that describes the event
    #' @param handler A handler function
    #' @param remove Logical, should the handler be removed?
    on_event = function(event,handler,remove=FALSE){
      if(!event %in% names(self$event_callbacks))
        self$event_callbacks[[event]] <- CallbackDispatcher()
      self$event_callbacks[[event]]$register(handler,remove)
    },
    #' @description Call the installed event handlers
    #' @param event A string that describes the event
    #' @param args A list of argument passed on with the event
    handle_event = function(event,args){
      event_callbacks <- self$event_callbacks[[event]]
      if(inherits(event_callbacks,"CallbackDispatcher"))
        do.call(event_callbacks$run,args)
    },
    #' @field displayed_callbacks A list of functions to be called when the widget 
    #         is displayed
    displayed_callbacks = list(),
    #' @description Install a handler to be called when the widget is displayed
    #' @param handler A handler function
    #' @param remove Logical, should the handler be removed?
    on_displayed = function(handler,remove=FALSE){
      if(!length(self$displayed_callbacks))
        self$displayed_callbacks <- CallbackDispatcher()
      self$displayed_callbacks$register(handler,remove)
    },
    #' @description Call the installed display handlers
    handle_displayed = function(){
      if(inherits(self$displayed_callbacks,"CallbackDispatcher"))
        self$displayed_callbacks$run()
    },
    #' @field _comm The 'comm' connecting to the frontend or NULL
    `_comm` = NULL,
    #' @description The internal function to send messages to the frontend
    #' @param msg The message
    #' @param buffers Raw data buffers or NULL
    `_send` = function(msg,buffers=NULL){
      if(!is.null(self$`_comm`)){
        # log_out("_send")
        # log_out(msg,use.print=TRUE)
        # log_out(buffers,use.print=TRUE)
        self$`_comm`$send(msg,buffers=buffers)
      }
    },
    #' @field required_version Minimum required ipywidgets version in which the
    #'        current widget class is supported.
    required_version = list(from=c(7,0,0)),
    #' @description Check whether current widget class is supported by ipywidgets
    check_version = function(){
      req <- self$required_version
      v <- widget_module_versions()
      v <- sum(v*c(100,10,1))
      from_v <- sum(req$from*c(100,10,1))
      supported <- v >= from_v
      if(length(req$before)){
        before_v <- sum(req$before*c(100,10,1))
        supported <- supported && (before_v < v)
      }
      if(!supported) warning("Widget not supported by this version of ipywidgets - expect the unexpected.")
    }
  ),
  active = list(
    #' @field comm The 'comm' connecting the frontend (as an active binding)
    comm = function(value){
      if(missing(value)) return(self$`_comm`)
      else self$`_comm` <- value
    }
  )
)

#' @describeIn Widgets A Widget Constructor Function
#' @param ... Arguments passed to the inializer
#' @export
Widget <- function(...) WidgetClass$new(...)

#' @describeIn display_data Method for jupyter widgets
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

#' @describeIn to_json S3 method for 'WidgetClass' objects, i.e. jupyter widgets
#' @export
to_json.Widget <- function(x,...){
  paste0("IPY_MODEL_",x[["_model_id"]])
}


# Local Variables:
# ess-indent-offset: 2
# End:
