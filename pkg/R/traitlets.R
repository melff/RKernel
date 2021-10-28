#' @export
R6Class_ <- function(...,lock_objects=FALSE)
             R6Class(...,
                     lock_objects=lock_objects)

#' @export
TraitClass <- R6Class_("Trait",
    public=list(
        set = function(value){
            if("validator" %in% ls(self))
                value <- self$validator(value)
            self$value <- value
        },
        get = function() self$value,
        is_default = function() !isFALSE(self$value != self$initial)
    )                       
)


#' @export
HasTraits <- R6Class_("HasTraits",
    public=list(
        traits=list(),
        callbacks=list(),
        initialize=function(...){
            members <- ls(self)
        #print(members)
            trait_names <- character()
            for(member in members){
                if(inherits(get(member,envir=self),"Trait"))
                    trait_names <- union(trait_names,member)
            }
            print(ls.str(self))
            print(self$traits)
            for(tn in trait_names){
                trait <- get(tn,self)
                rm(list=tn,envir=self)
                # cat("===================")
                # cat(tn,"\n")
                if(is.function(trait$setup)){
                    # cat("-------------------")
                    # str(trait)
                    # cat("-------------------")
                    trait$setup()
                    # str(trait)
                    # self$traits[[tn]] <- NULL
                }
                self$traits[[tn]] <- trait
                trait_fun <- eval(substitute(function(value){
                    trait <- self$traits[[tn]]
                    if(missing(value)) return(trait$get())
                    trait$set(value)
                    if(!trait$is_default())
                        self$notify(tn,value)
                },list(tn=tn)))
                makeActiveBinding(tn, trait_fun, self)
            }
        },
        notify=function(tn,value){
            if(tn %in% names(self$callbacks)){
                trait <- self$traits[[tn]]
                callbacks <- self$callbacks[[tn]]
                for(cb in callbacks)
                    cb(tn,self,value)
            }
        },
        observe=function(tn,handler){
            if(!(tn %in% names(self$callbacks))){
                self$callbacks[[tn]] <- list()
            }
            self$callbacks[[tn]] <- append(self$callbacks[[tn]],handler)
        },
        unobserve=function(tn,handler){
            if(tn %in% names(self$callbacks)){
                callbacks_in <- self$callbacks[[tn]]
                callbacks_out <- list()
                for(h in callbacks){
                    if(!identical(h,handler))
                        callbacks_out <- append(callbacks_out,h)
                }
                self$callbacks[[tn]] <- callbacks_out
            }
        }
    )
)

