#' A Manager for Comms
#'
#' @description Objects of this class are used internally to manage comms, they
#'     are not meant to be used by end-users.
#' @details  See the documentation 
#'   of \href{https://jupyter-client.readthedocs.io/en/latest/messaging.html#custom-messages}{Jupyter custom messages}.
#' @name CommManager 
NULL

#' @rdname CommManager
#' @export
CommManagerClass <- R6Class("CommManager",
    
    public=list(

        #' @field comms A list of Comms.
        comms   = list(),

        #' @description
        #' Set up internal fields
        #' @param kernel Reference to the relevant kernel
        #' @param evaluator Reference to the relevant evaluator
        initialize = function(kernel,evaluator){
            private$kernel <- kernel
            private$evaluator <- evaluator
        },

        #' @description
        #' Add a handler for a comm target
        #' @param target_name A string, the name of the target.
        #' @param handlers A named list of handlers
        add_handlers = function(target_name,handlers){
            for(n in names(handlers)){
                environment(handlers[[n]]) <- new.env(parent=environment(handlers[[n]]))
                assign("print",base::print,envir=environment(handlers[[n]]))
                assign("cat",base::cat,envir=environment(handlers[[n]]))
            }
            private$handlers[[target_name]] <- handlers
        },
        #' @description
        #' Remove the handlers of a comm target
        #' @param target_name A string, the name of the target
        remove_handlers = function(target_name) {
            private$handlers[[target_name]] <- NULL
        },
        #' @description
        #' Check if handlers for a target exist
        #' @param target_name A string, the name of the target
        has_handlers = function(target_name) target_name %in% names(private$handlers),
        #' @description
        #' Get the handlers for a target
        #' @param target_name A string, the name of the target
        get_handlers = function(target_name) private$handlers[[target_name]],
        #' @description
        #' Get all comms or all comms related to a target
        #' @param target_name A string, the name of the target or NULL. If NULL,
        #     all comms are returned
        get_comms = function(target_name=NULL){
            comms <- list()
            for(c in self$comms){
                if(!length(target_name) || target_name == c$target_name)
                    comms[[c$id]] <- list(target_name=c$target_name)
            }
            return(comms)
        },
        #' @description
        #' Create a new comm related to a target
        #' @param target_name A string, the name of the target
        new_comm = function(target_name){
            if(target_name %in% names(private$handlers)){
                id <- uuid()
                handlers <- private$handlers[[target_name]]
                comm <- Comm(target_name,
                             id,
                             private$kernel,
                             handlers)
                self$comms[[id]] <- comm
                return(comm)
            }
            else warning(sprintf("Comm target '%s' not found",target_name))
        },
        #' @description
        #' Handle a 'comm open' request from the frontend
        #' @param target_name A string, the name of the target
        #' @param id A string, the comm id
        #' @param data Data sent by the frontend
        handle_open = function(target_name,id,data){
            # private$kernel$log_out("handle_open")
            # private$kernel$log_out(data,use.print=TRUE)
            if(target_name %in% names(private$handlers)){
                handlers <- private$handlers[[target_name]]
                comm <- Comm(target_name,id,self,handlers)
                comm$data <- data
                if("open" %in% names(handlers)){
                    text <- tryCatch(capture.output(handlers$open(comm,data)),
                                     error=function(e) {
                                         emsg <- conditionMessage(e)
                                         private$evaluator$stream(
                                                               paste("Error in handle_open:",
                                                                     emsg),
                                                           stream="stderr")
                                         return("")
                                     })
                    text <- paste(text,collapse="\n")
                    private$evaluator$stream(text,stream="stdout")
                }
                self$comms[[id]] <- comm
            }
            else {
                # private$kernel$log_out("send_close")
                self$send_close(id,target_name)
            }
        },
        #' @description
        #' Handle a 'comm close' request from the frontend
        #' @param id A string, the comm id
        #' @param data Data sent by the frontend
        handle_close = function(id,data){
            # private$kernel$log_out("handle_close")
            # private$kernel$log_out(data,use.print=TRUE)
            comm <- self$comms[[id]]
            handlers <- comm$handlers
            comm$data <- data
            if("close" %in% names(handlers)){
                text <- tryCatch(capture.output(handlers$close(comm,data)),
                                 error=function(e) {
                                     emsg <- conditionMessage(e)
                                     private$evaluator$stream(
                                                           paste("Error in handle_close:",
                                                                 emsg),
                                                           stream="stderr")
                                     return("")
                                 })
                text <- paste(text,collapse="\n")
                private$evaluator$stream(text,stream="stdout")
            }
        },
        #' @description
        #' Handle a comm message from the frontend
        #' @param id A string, the comm id
        #' @param data Data sent by the frontend
        handle_msg = function(id,data){
            comm <- self$comms[[id]]
            handlers <- comm$handlers
            comm$data <- data
            if("msg" %in% names(handlers)){
                # handlers$msg(comm,data)
                text <- tryCatch(capture.output(handlers$msg(comm,data)),
                                 error=function(e) {
                                     emsg <- conditionMessage(e)
                                     private$evaluator$stream(
                                                           paste("Error in handle_msg:",
                                                                 emsg),
                                                           stream="stderr")
                                     return("")
                                 })
                text <- paste(text,collapse="\n")
                private$evaluator$stream(text,stream="stdout")
            }
        },
        #' @description
        #' Send data to the frontend
        #' @param id A string, the comm id
        #' @param data A named list
        #' @param metadata A named list
        #' @param buffers A list of raw vectors or NULL
        send = function(id,data,metadata=emptyNamedList,buffers=NULL){
            # log_out("comm_manager$send")
            # log_out(data,use.print=TRUE)
            # log_out(buffers,use.print=TRUE)
            private$kernel$send_comm_msg(id,data,metadata,buffers=buffers)  
        },
        #' @description
        #' Send an 'open' request to the frontend
        #' @param id A string, the comm id
        #' @param target_name A string, the name of the target
        #' @param data A named list
        #' @param metadata A named list
        #' @param buffers A list of raw vectors or NULL
        send_open = function(id,target_name,data,metadata=emptyNamedList,buffers=NULL){
            # log_out("comm_manager$send_open")
            # log_out(data,use.print=TRUE)
            # log_out(buffers,use.print=TRUE)
            private$kernel$send_comm_open(id,target_name,data,metadata,buffers=buffers)  
        },
        #' @description
        #' Send an 'close' request to the frontend
        #' @param id A string, the comm id
        #' @param data A named list
        #' @param metadata A named list
        #' @param buffers A list of raw vectors or NULL
        send_close = function(id,data=emptyNamedList,metadata=emptyNamedList,buffers=NULL){
            private$kernel$send_comm_close(id,data,metadata,buffers=buffers)  
        },
        #' @description
        #' Return a list of targets
        list_targets = function() return(private$handlers)
    ),
    
    private = list(
        kernel  = list(),
        evaluator = list(),
        handlers = list()
    )
)

#' @describeIn CommManager A constructor for objects in the 'CommManagerClass'
#' @param ... Arguments passed to the inializer
#' @export
CommManager <- function(...) CommManagerClass$new(...)


#' Comms - connections between the kernel and the frontend
#'
#' @description This R6 Class provides for bidirectional communication between the R Kernel and
#'    the Jupyter frontend, e.g. a Jupyter notebook
#' @details Objects of this class are used to communicate to the frontend via
#' \href{https://jupyter-client.readthedocs.io/en/latest/messaging.html#custom-messages}{custom messages}.
#' @name Comm
NULL

#' @rdname Comm
#' @export
CommClass <- R6Class("Comm",

    public = list(

        #' @field id A character string, the comm id
        id = character(0),
        #' @field target_name A character string, the target
        target_name = character(0),
        #' @field handlers A list of handler functions
        handlers = list(),
        #' @field data A list of data
        data = list(),
        
        #' @description Initialize a 'Comm' object
        #' @param target_name A string, the name of the target
        #' @param id A string, the comm id
        #' @param kernel The relevant kernel
        #' @param handlers A list of handler functions
        initialize = function(target_name,
                              id = uuid(),
                              kernel = get_current_kernel(),
                              handlers = list()){
            self$target_name <- target_name
            self$id <- id
            private$kernel <- kernel
            self$handlers <- handlers
        },

        #' @description Open a comm
        #' @param data A named list
        #' @param metadata A named list
        #' @param buffers A list of raw vectors or NULL
        open = function(data,metadata=emptyNamedList,buffers=NULL){
            # log_out("comm$open")
            # log_out(data,use.print=TRUE)
            # log_out(buffers,use.print=TRUE)
            id <- self$id
            target_name <- self$target_name
            private$kernel$send_comm_open(id,target_name,data,metadata,buffers=buffers)  
        },
        #' @description Send data through a comm
        #' @param data A named list
        #' @param metadata A named list
        #' @param buffers A list of raw vectors or NULL
        send = function(data,metadata=emptyNamedList,buffers=NULL){
            # log_out("comm$send")
            # log_out(data,use.print=TRUE)
            # log_out(buffers,use.print=TRUE)
            id <- self$id
            private$kernel$send_comm_msg(id,data,metadata,buffers=buffers)  
        },
        #' @description Close a comm
        #' @param data A named list
        #' @param metadata A named list
        #' @param buffers A list of raw vectors or NULL
        close = function(data=emptyNamedList,metadata=emptyNamedList,buffers=NULL){
            # log_out("comm$close")
            # log_out(data,use.print=TRUE)
            id <- self$id
            private$kernel$send_comm_close(id,data,metadata,buffers=buffers)  
        }
    ),
    
    private = list(
        kernel = list()
    )
)

#' @describeIn Comm A constructor function for objects of class "CommClass"
#' @param ... Arguments passed to the inializer
#' @export
Comm <- function(...) CommClass$new(...)

# Get the comm manager of the current kernel
get_comm_manager <- function() {
    kernel <- get_current_kernel()
    kernel$comm_manager
}
