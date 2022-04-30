#' @include traitlets.R
#'
#' @export
BytesClass <- R6Class_("Bytes",
    inherit=TraitClass,
    public=list(
        value = raw(0),
        optional = TRUE,
        coerce = TRUE,
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
        initialize=function(initial=raw(0),coerce=TRUE,
                            optional=TRUE
                            ){
            self$optional <- optional
            self$coerce <- coerce
            super$initialize(initial)
}))

#' @export
Bytes <- function(...)TraitInstance(...,Class=BytesClass)

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
