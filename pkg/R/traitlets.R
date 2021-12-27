#' @include json.R

#' @export
R6Class_ <- function(...,lock_objects=FALSE)
             R6Class(...,
                     lock_objects=lock_objects)

#' @export
TraitClass <- R6Class_("Trait",
   public=list(
       name = character(0),
       value = NULL,
       set = function(value,notify=FALSE){
            if(length(self$validators)){
                for(validator in self$validators){
                    if(is.function(validator))
                        value <- validator(value)
                }
            }
            self$value <- value
            if(length(self$observers) && notify){
                for(observer in self$observers){
                    # log_out(sprintf("Calling observer for name '%s' value %s",self$name,value))
                    if(is.function(observer))
                        observer(self$name,self,value)
                }
            }
       },
       get = function() self$value,
       is_default = function() self$value == self$initial,
       observers = list(),
       validators = list(),
       initialize = function(initial,coerce=TRUE){
            validator <- self$validator
            if(is.function(validator) && !missing(initial)){
                self$value <- validator(initial)
                self$validators <- list(validator)
            }
       }
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

#' @export
Trait <- function(...)TraitInstance(...,Class=TraitClass)


as_property <- function(self) {
    #self <- force(self)
    e <- new.env()
    res <- function(value){
            if(missing(value)) return(self$get())
            else return(self$set(value,notify=TRUE))
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
          # print(class(self))
          # print(initials)
          members <- ls(self)
          trait_names <- character()
          for(member in members){
              if(inherits(get(member,envir=self),"TraitInstance"))
                  trait_names <- union(trait_names,member)
          }
          for(tn in trait_names){
              # print(tn)
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
              observer <- function(name,trait,value){
                  self$notify(name,value)
              }
              trait$observers <- list(observer)
              self$traits[[tn]] <- trait
              property <- as_property(trait)
              self$properties[[tn]] <- property
              makeActiveBinding(tn, property, self)
          }
      },
      notify = function(tn,value){
          # log_out(sprintf("notify %s = %s",tn,value))
          if(length(self$observers) && tn %in% names(self$observers)){
              observers <- self$observers[[tn]]
              for(cb in observers){
                  if(is.function(cb))
                      cb(tn,self,value)
              }
          }
      },
      observers = list(),
      observe=function(tn,handler,remove=FALSE){
          callbacks_in <- self$observers[[tn]]
          callbacks_out <- list()
          for(h in callbacks_in){
              if(!identical(h,handler))
                  callbacks_out <- append(callbacks_out,h)
          }
          if(!remove)
              callbacks_out <- append(callbacks_out,handler)
          self$observers[[tn]] <- callbacks_out
      },
      validate=function(tn,handler,remove=FALSE){
          trait <- self$traits[[tn]]
          callbacks_in <- trait$validators
          callbacks_out <- list()
          for(h in callbacks_in){
              if(!identical(h,handler))
                  callbacks_out <- append(callbacks_out,h)
          }
          if(!remove)
              callbacks_out <- append(callbacks_out,handler)
          trait$validators <- callbacks_out
          self$traits[[tn]] <- trait
      }
  )
)


#' @export
to_json.Trait <- function(x){
    value <- x$get()
    if(!length(value))
        character(0)
    else to_json(value)
}
