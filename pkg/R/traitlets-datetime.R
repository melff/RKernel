#' Date Traitlets
#'
#' @description A class and constructor of date traitlets.
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
        #' @param initial An optional Date object or date string
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


#' Datetime Traitlets
#'
#' @description A class and constructor of datetime traitlets.
#'
#' @include traitlets-vector.R
#' @name DatetimeClass
NULL

#' @rdname DatetimeClass
#' @export
DatetimeClass <- R6Class_("DatetimeClass",
                      inherit = TraitClass,
                      public = list(
                          #' @field value A date.
                          value = as.POSIXct(numeric(0)),
                          #' @field coerce Logical value, whether assignments to the value field should
                          #'    be coerced to the appropriate type.
                          coerce = TRUE,
                          #' @description Check the value assigned to the traitlet.
                          #' @param value The value assigned to the traitlet.
                          validator=function(value){
                              if(self$coerce){
                                  value <- as.POSIXct(value)
                              }
                              else {    
                                  if(!inherits(value,"POSIXct") || length(value) != 1)
                                      stop("DatetimeClass: single datetime value required")
                              }
                              value
                          },
                          #' @description Initialize the traitlet.
                          #' @param initial An optional POSIXct object or an object coercive into such an object
                          #' @param coerce An optional logical value
                          initialize=function(initial=as.POSIXct(integer(0)),
                                              coerce=TRUE){
                              self$coerce <- coerce
                              initial <- as.POSIXct(initial)
                              super$initialize(initial)
                          }
                      )
                      )

#' @rdname DatetimeClass
#' @param ... Arguments that are passed to the initialize method of 'DatetimeClass'
#' @export
Datetime <- function(...)TraitInstance(...,Class=DatetimeClass)

#' Time Traitlets
#'
#' @description A class and constructor of time traitlets.
#'
#' @include traitlets-vector.R
#' @name TimeClass
NULL

#' @rdname TimeClass
#' @export
TimeClass <- R6Class_("TimeClass",
                      inherit = TraitClass,
                      public = list(
                          #' @field value A date.
                          value = as.POSIXct(numeric(0)),
                          #' @field coerce Logical value, whether assignments to the value field should
                          #'    be coerced to the appropriate type.
                          coerce = TRUE,
                          #' @description Check the value assigned to the traitlet.
                          #' @param value The value assigned to the traitlet.
                          validator=function(value){
                              if(self$coerce){
                                  value <- as.POSIXct(value)
                              }
                              else {    
                                  if(!inherits(value,"POSIXct") || length(value) != 1)
                                      stop("TimeClass: single time value required")
                              }
                              value
                          },
                          #' @description Initialize the traitlet.
                          #' @param initial An optional POSIXct object or an object coercive into such an object
                          #' @param coerce An optional logical value
                          initialize=function(initial=as.POSIXct(integer(0)),
                                              coerce=TRUE){
                              self$coerce <- coerce
                              initial <- as.POSIXct(initial)
                              super$initialize(initial)
                          }
                      )
                      )

#' @rdname DatetimeClass
#' @param ... Arguments that are passed to the initialize method of 'TimeClass'
#' @export
Time <- function(...)TraitInstance(...,Class=TimeClass)

