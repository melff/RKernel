#' @export
R6Class_ <- function(...,lock_objects=FALSE)
             R6Class(...,
                     lock_objects=lock_objects)

#' @export
TraitClass <- R6Class_("Trait",
   public=list(
        name = character(0),
        set = function(value){
            if("validator" %in% ls(self))
                self$validator(value)
            self$value <- value
            if(length(self$callbacks)){
                for(cb in self$callbacks){
                    if(is.function(cb))
                        cb(self$name,self,value)
                }
            }
        },
        get = function() self$value,
        is_default = function() self$value == self$initial,
        callbacks = list()
   )
)

#' @export
TraitInstance <- function(Class,...){
    structure(
        list(
            args=list(...),
            Class=Class),
        class="TraitInstance")
}


as_property <- function(self) {
    #self <- force(self)
    e <- new.env()
    res <- function(value){
            if(missing(value)) return(self$get())
            else return(self$set(value))
            }
    e$self <- self
    environment(res) <- e
    return(res)
}

#' @export
HasTraits <- R6Class_("HasTraits",
  public=list(
      traits=list(),
      properties=list(),
      envs = list(),
      initialize=function(...){
          initials <- list(...)
          members <- ls(self)
          trait_names <- character()
          for(member in members){
              if(inherits(get(member,envir=self),"TraitInstance"))
                  trait_names <- union(trait_names,member)
          }
          for(tn in trait_names){
              traitgen <- get(tn,self)
              rm(list=tn,envir=self)
              args <- traitgen$args
              Class <- traitgen$Class
              attribs <- attributes(traitgen)
              attribs$class <- NULL
              attribs$names <- NULL
              trait <- do.call(Class$new,args)
              attribs$class <- class(trait)
              attributes(trait) <- attribs
              trait$name <- tn
              if(tn %in% names(initials))
                  trait$set(initials[[tn]])
              self$traits[[tn]] <- trait
              property <- as_property(trait)
              self$properties[[tn]] <- property
              makeActiveBinding(tn, property, self)
          }
      },
      observe=function(tn,handler){
          trait <- self$traits[[tn]]
          trait$callbacks <- append(trait$callbacks,handler)
      },
      unobserve=function(tn,handler){
          trait <- self$traits[[tn]]
          callbacks_in <- trait$callbacks
          callbacks_out <- list()
          for(h in callbacks_in){
              if(!identical(h,handler))
                  callbacks_out <- append(callbacks_out,h)
          }
          trait$callbacks <- callbacks_out
        }
  )
)
