#' @export
R6Class_ <- function(...,lock_objects=FALSE)
             R6Class(...,
                     lock_objects=lock_objects)

#' @export
HasTraits <- R6Class_("HasTraits",
  public=list(
    initialize=function(...){
        members <- ls(self)
        trait_names <- character()
        for(member in members){
            if(inherits(get(member,envir=self),"Trait"))
                trait_names <- union(trait_names,member)
        }
        for(tn in trait_names){
            trait <- get(tn,self)
            rm(list=tn,envir=self)
            self$validator[[tn]] <- trait$validator
            self$observe[[tn]] <- trait$observe
            self$notifier[[tn]] <- trait$notifier
            if(isTRUE(trait$sync))
                self$sync <- union(self$sync,tn)
            trait_fun <- eval(substitute(function(value){
                if(missing(value)) return(self$state[[tn]])
                validator <- self$validator[[tn]]
                self$state[[tn]] <- validator(value)
                if(self$observe[[tn]] && self$do_notify) {
                    if(length(self$notifier[[tn]])){
                        notifier <- self$notifier[[tn]]
                        notifier(tn,value)
                    } else
                        self$notify(tn)
                }
            },list(tn=tn)))
            if(length(trait$initial)){
                validator <- self$validator[[tn]]
                initial <- trait$initial
                self$state[[tn]] <- validator(initial)
            }
            makeActiveBinding(tn, trait_fun, self)
        }
    },
    do_notify = TRUE,
    notify = function(name){
        print(sprintf("%s changed",name))
    },
    get_state = function(keys=NULL){
        if(is.null(keys)){
            keys <- names(self$state)
        }
        state <- self$state[keys]
        len <- sapply(state,length)
        return(state[len>0])
    }
    )
)

