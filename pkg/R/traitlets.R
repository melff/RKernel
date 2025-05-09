#' R6 Objects That Are Not Locked
#'
#' @description This function calls the R6 class constructor in such a way that
#'   new members can be added to the created objects.
#'
#' @param ... Arguments passed to the superclass constructor
#' @param lock_objects A logical value, indicates whether objects should be
#'     locked. See \code{\link[R6]{R6Class}}.
#' 
#' @include json.R
#' @export
R6Class_ <- function(...,lock_objects=FALSE)
             R6Class(...,
                     lock_objects=lock_objects)


#' Traitlets
#'
#' @description The class \code{TraitClass} brings (some of) the functionality of the
#'     \href{https://traitlets.readthedocs.io/}{traitlets framework} on which
#'     the \href{https://ipywidgets.readthedocs.io}{ipywidgets framework} is
#'     based to \emph{R}.
#'
#' @name Traitlets
NULL

#' @rdname Traitlets
#' @export
TraitClass <- R6Class_("Trait",
   public=list(
       #' @field value The value of the trait
       value = NULL,
       #' @description
       #' Set the value of the trait
       #' @param value The value to be set
       #' @param notify Logical; whether to call notification callbacks
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
       #' @description Get the trait value
       get = function() self$value,
       #' @field observers A list of functions to be called as notification callbacks
       observers = list(),
       #' @field validators A list of functions to check the validity of a
       #     trait value. Usually called by the 'set()' and 'initialize()' methods
       validators = list(),
       #' @description Initialize the trait, i.e. set an initial value
       #' @param initial The initial value
       #' @param coerce Logical; whether to coerce the initial value
       #'    to the approriate mode.
       initialize = function(initial){
            validator <- self$validator
            if(is.function(validator) && !missing(initial)){
                self$value <- validator(initial)
                self$validators <- list(validator)
            }
            else if(!missing(initial))
                self$value <- initial
       }
   )
)


#' @describeIn Traitlets A Baseline Trait Constructor
#' @param ... Arguments passed to the inializer
#' @export
Trait <- function(...)TraitInstance(...,Class=TraitClass)


#' @describeIn Traitlets A "Delayed Constructor" for Traits, to be used by constructors of derived classes.
#'
#' @description The function \code{TraitInstance}  returns information needed by a
#'     \code{\link{HasTraits}} object to construct a \code{\link{TraitClass}}
#'     object.
#' @param Class An R6 Class that inherits from "TraitClass"
#' @param ... Arguments passed to the inializer
#' @export
TraitInstance <- function(Class,...){
    structure(
        list(
            args=list(...),
            Class=Class),
        class="TraitInstance")
}

Force <- function(delayed){
    Class <- delayed$Class
    args <- delayed$args
    do.call(Class$new,args)
}

#' @export
print.TraitInstance <- function(x,...){
    x <- Force(x)
    cat(sprintf("Delayed trait instance of class '%s'\n",class(x)[1]))
    print(x$value)
}

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

#' The Base Class of Objects with Traits
#'
#' @description Objects in class HasTraits have traits as components
#'    that are correctly initialized using delayed construction with
#'    the \code{\link{TraitInstance}} function.
#' 
#' @export
HasTraits <- R6Class_("HasTraits",
  public=list(
      #' @field call The generating call
      call = character(0),
      #' @field traits A list of traits 
      traits=list(),
      #' @description Initialize an object
      #' @param ... Initializsing values
      initialize=function(...){
          mycall <- match.call()
          mycall[[1]] <- as.symbol(class(self)[1])
          self$call <- paste(deparse(mycall), collapse="\n")
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
      #' @field suspended Logical value; whether notifying observers is suspended.
      suspended = FALSE,
      #' @description
      #' Notify observers about a trait being set to a value.
      #' @param tn A string, the name of the trait.
      #' @param value The value to which the trait is set.
      notify = function(tn,value){
          # log_out(sprintf("notify %s = %s",tn,value))
          if(!self$suspended && length(self$observers) && tn %in% names(self$observers)){
              observers <- self$observers[[tn]]
              for(cb in observers){
                  # log_out(cb,use.print=TRUE)
                  if(is.function(cb))
                      cb(tn,self,value)
              }
          }
      },
      #' @field observers A list of observers, i.e. callback functions called by
      #' the \code{notify} method.
      observers = list(),
      #' @description Install or remove an observer function.
      #' @param tn A string, the name of a trait.
      #' @param observer A callback function, which should take three arguments,
      #'     (1) the trait name, (2) the object that has the trait, (3) 
      #' @param remove A logical value, indicates whether the observer is to be removed
      #'    or added
      observe=function(tn,observer,remove=FALSE){
          callbacks_in <- self$observers[[tn]]
          callbacks_out <- list()
          for(cb in callbacks_in){
              if(!identical(cb,observer))
                  callbacks_out <- append(callbacks_out,cb)
          }
          if(!remove)
              callbacks_out <- append(callbacks_out,observer)
          self$observers[[tn]] <- callbacks_out
      },
      #' @description Install or remove the validator function of a trait.
      #' @param tn A string, the name of a trait.
      #' @param validator A callback function
      #' @param remove A logical value, indicates whether the validator is to be removed
      #'    or added
      validate=function(tn,validator,remove=FALSE){
          trait <- self$traits[[tn]]
          callbacks_in <- trait$validators
          callbacks_out <- list()
          for(cb in callbacks_in){
              if(!identical(cb,validator))
                  callbacks_out <- append(callbacks_out,cb)
          }
          if(!remove)
              callbacks_out <- append(callbacks_out,validator)
          trait$validators <- callbacks_out
          self$traits[[tn]] <- trait
      }
  )
)

#' @describeIn to_json S3 method for 'TraitClass' objects, i.e. traitlets.
#' @param x An object to be converted as JSON
#' @param auto_unbox A logical value, whether one-element a JSON list should be
#'     changed into JSON scalar.
#' @param ... Other arguments, passed on to  \code{\link[jsonlite]{toJSON}}.
#' @export
to_json.Trait <- function(x,auto_unbox=TRUE,...){
    value <- x$get()
    if(!length(value) && auto_unbox)
        character(0)
    else to_json(value,auto_unbox=auto_unbox,...)
}

