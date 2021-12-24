#' @export
CommManagerClass <- R6Class("CommManager",
    
    public=list(

        comms   = list(),

        initialize = function(kernel,evaluator){
            private$kernel <- kernel
            private$evaluator <- evaluator
        },

        add_handlers = function(target_name,handlers){
            for(n in names(handlers)){
                environment(handlers[[n]]) <- new.env(parent=environment(handlers[[n]]))
                assign("print",base::print,envir=environment(handlers[[n]]))
                assign("cat",base::cat,envir=environment(handlers[[n]]))
            }
            private$handlers[[target_name]] <- handlers
        },
        remove_handlers = function(target_name) {
            private$handlers[[target_name]] <- NULL
        },
        has_handlers = function(target_name) target_name %in% names(private$handlers),
        get_handlers = function(target_name) private$handlers[[target_name]],
        get_comms = function(target_name=NULL){
            comms <- list()
            for(c in self$comms){
                if(!length(target_name) || target_name == c$target_name)
                    comms[[c$id]] <- list(target_name=c$target_name)
            }
            return(comms)
        },
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
        handle_open = function(target_name,id,data){
            private$kernel$log_out("handle_open")
            private$kernel$log_out(data,use.print=TRUE)
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
                private$kernel$log_out("send_close")
                self$send_close(id,target_name)
            }
        },
        handle_close = function(id,data){
            private$kernel$log_out("handle_close")
            private$kernel$log_out(data,use.print=TRUE)
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
        send = function(id,data,metadata=emptyNamedList,buffers=NULL){
            # log_out("comm_manager$send")
            # log_out(data,use.print=TRUE)
            # log_out(buffers,use.print=TRUE)
            private$kernel$send_comm_msg(id,data,metadata,buffers=buffers)  
        },
        send_open = function(id,target_name,data,metadata=emptyNamedList,buffers=NULL){
            # log_out("comm_manager$send_open")
            # log_out(data,use.print=TRUE)
            # log_out(buffers,use.print=TRUE)
            private$kernel$send_comm_open(id,target_name,data,metadata,buffers=buffers)  
        },
        send_close = function(id,target_name,data=emptyNamedList,metadata=emptyNamedList,buffers=NULL){
            private$kernel$send_comm_close(id,data,metadata,buffers=buffers)  
        },
        list_targets = function() return(private$handlers)
    ),
    
    private = list(
        kernel  = list(),
        evaluator = list(),
        handlers = list()
    )
)
#' @export
CommManager <- function(...) CommManagerClass$new(...)

#' @export
CommClass <- R6Class("Comm",

    public = list(

        id = character(0),
        target_name = character(0),
        handlers = list(),
        data = list(),
        
        initialize = function(target_name,
                              id = uuid(),
                              kernel = get_current_kernel(),
                              handlers){
            self$target_name <- target_name
            self$id <- id
            private$kernel <- kernel
        },

        open = function(data,metadata=emptyNamedList,buffers=NULL){
            # log_out("comm$open")
            # log_out(data,use.print=TRUE)
            # log_out(buffers,use.print=TRUE)
            id <- self$id
            target_name <- self$target_name
            private$kernel$send_comm_open(id,target_name,data,metadata,buffers=buffers)  
        },
        send = function(data,metadata=emptyNamedList,buffers=NULL){
            # log_out("comm$send")
            # log_out(data,use.print=TRUE)
            # log_out(buffers,use.print=TRUE)
            id <- self$id
            private$kernel$send_comm_msg(id,data,metadata,buffers=buffers)  
        },
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
#' @export
Comm <- function(...) CommClass$new(...)

get_comm_manager <- function() {
    kernel <- get_current_kernel()
    kernel$comm_manager
}
