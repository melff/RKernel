CommManager <- R6Class("CommManager",
    
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
        get_comms = function(target_name=NULL){
            comms <- list()
            for(c in self$comms){
                if(!length(target_name) || target_name %in% names(private$targets))
                    comms[[c$id]] <- list(target_name=c$target_name)
            }
            return(comms)
        },
        new_comm = function(target_name){
            if(target_name %in% names(private$handlers)){
                id <- UUIDgenerate()
                handlers <- private$handlers[[target_name]]
                comm <- Comm$new(target_name,id,self,handlers)
                self$comms[[id]] <- comm
                return(comm)
            }
            else return(NULL)
        },
        handle_open = function(target_name,id,data){
            if(target_name %in% names(private$handlers)){
                handlers <- private$handlers[[target_name]]
                comm <- Comm$new(target_name,id,self,handlers)
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
                self$send_close(id,target_name)
            }
        },
        handle_close = function(id,data){
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
        send = function(id,data){
            private$kernel$send_comm_msg(id,data)  
        },
        send_open = function(id,target_name,data){
            private$kernel$send_comm_open(id,target_name,data)  
        },
        send_close = function(id,target_name){
            private$kernel$send_comm_close(id,data)  
        }
    ),
    
    private = list(
        kernel  = list(),
        evaluator = list(),
        targets = list(),
        handlers = list()
    )
)

Comm <- R6Class("Comm",

    public = list(

        id = character(0),
        target_name = character(0),
        handlers = list(),
        data = list(),
        
        initialize = function(target_name,id,manager,handlers){
            self$target_name <- target_name
            self$id <- id
            private$manager <- manager
            self$handlers <- handlers
        },

        open = function(data){
            id <- self$id
            target_name <- self$target_name
            private$manager$send_open(id,target_name,data)
        },
        send = function(data){
            id <- self$id
            private$manager$send(id,data)
        },
        close = function(data){
            id <- self$id
            private$manager$send_close(id,data)
        }
    ),
    
    private = list(
        manager = list()
    )
)
