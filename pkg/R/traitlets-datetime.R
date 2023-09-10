#' Date Traitlets
#'
#' @description A class and constructor of date traitlets.
#'   These are Dict traitlets with year, month, and day components.
#'
#' @include traitlets-vector.R
#' @name DateClass
NULL

#' @rdname DateClass
#' @export
DateClass <- R6Class_("DateClass",
    inherit = TraitClass,
    public = list(
        #' @field value A date.
        value = as.Date(integer(0)),
        #' @field coerce Logical value, whether assignments to the value field should
        #'    be coerced to the appropriate type.
        coerce = TRUE,
        #' @description Check the value assigned to the traitlet.
        #' @param value The value assigned to the traitlet.
        validator=function(value){
            if(self$coerce){
                value <- as.Date(value)
            }
            else {    
                if(!inherits(value,"Date") || length(value) != 1)
                    stop("DateClass: single date value required")
            }
            value
        },
        #' @description Initialize the traitlet.
        #' @param initial An optional Date object or date tring
        #' @param year An optional integer
        #' @param month An optional integer
        #' @param day An optional integer
        #' @param coerce An optional logical value
        initialize=function(initial=as.Date(integer(0)),
                            year=integer(0),
                            month=integer(0),
                            day=integer(0),
                            coerce=TRUE){
            self$coerce <- coerce
            if(missing(initial) && length(year)) {
                year <- as.integer(year)
                month <- as.integer(month)
                day <- as.integer(day)
                if(!length(month)) month <- 1L
                if(!length(day)) day <- 1L
                datestr <- paste(year,month,day,sep="-")
                initial <- as.Date(datestr,"%Y-%m-%d")
            }
            initial <- as.Date(initial)
            super$initialize(initial)
        }
    )
)

leapyear <- function(x){
    res <- FALSE
    if(x %% 4 == 0) res <- TRUE
    if(x %% 100 == 0) res <- FALSE
    if(x %% 400 == 0) res <- TRUE
}


#' @rdname DateClass
#' @param ... Arguments that are passed to the initialize method of 'DictClass'
#' @export
Date <- function(...)TraitInstance(...,Class=DateClass)

#' @rdname DateClass
#' @param x A date traitlet.
#' @param ... Other arguments.
#' @export
as.Date.DateClass <- function(x,...) {
    x$value
}

