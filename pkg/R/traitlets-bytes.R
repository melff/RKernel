#' Raw Bytes Traitlets
#'
#' @description A class and a constructor function to create (raw) bytes trait(let)s.
#'
#' @include traitlets.R
#' @name Bytes

#' @rdname Bytes
#' @export
BytesClass <- R6Class_("Bytes",
    inherit=TraitClass,
    public=list(
        #' @field value A raw bytes vector
        value = raw(0),
        #' @field optional Logical, whether an initializing logical value must be provided
        optional = TRUE,
        #' @field coerce Logical, whether 'as.raw()' is implicitely used when a value is
        #'   assigned to the trait
        coerce = TRUE,
        #' @description
        #' A validator method
        #' @param value A value to be checked for validity
        validator=function(value){
            if(self$coerce){
                value <- as.raw(value)
            }
            else {    
                if(!is.raw(value))
                    stop("Bytes: raw bytes required")
            }
            if(length(value) == 0 && !self$optional) {
                e_msg <- "Bytes: This object should not be empty"
                stop(e_msg)
            }
            value
        },
        #' @description
        #' The initializing method
        #' @param initial A value with which the traitlet is initialized
        #' @param coerce Logical, used to initialize the 'coerce' field
        #' @param optional Logical, used to initialize the 'optional' field
        initialize=function(initial=raw(0),coerce=TRUE,
                            optional=TRUE
                            ){
            self$optional <- optional
            self$coerce <- coerce
            super$initialize(initial)
}))

#' @rdname Bytes
#' @param ... Arguments that are passed to the initialize method of 'BytesClass'
#' @export
Bytes <- function(...)TraitInstance(...,Class=BytesClass)

#' @describeIn to_json S3 method for 'BytesClass' objects
#' @export
to_json.Bytes <- function(x,...){
    #value <- x$get()
    #value <- jsonlite::toJSON(value,auto_unbox=TRUE)
    #value <- as.character(value)
    #value <- gsub("\"","",value)
    # value <- gsub("\\\\","\\",value)
    #paste0("data:image/png;base64,",value)
    #value
    NULL
}
