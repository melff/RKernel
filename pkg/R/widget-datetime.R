#' Datetime Picker Widgets
#' @description An R6 class and constructor function for datetime picker widgets
#' @include widget-value.R
#' @name DatetimePicker
NULL


Datetime_from_json <- function(value) {
    if(!length(value)) return(NULL)
    value <- ISOdatetime(
        year =  value$year,
        month = value$month + 1,
        day =   value$date,
        hour =  value$hours,
        min =   value$minutes,
        sec =   value$seconds,
        tz = Sys.timezone()
    )
    value
}

Datetime_to_json <- function(dateObj) {
    value <- dateObj$value
    if(!length(value)) return(NULL)
    year <-  as.integer(format(value,"%Y"))
    month <- as.integer(format(value,"%m"))
    day <-   as.integer(format(value,"%d"))
    hour <-  as.integer(format(value,"%H"))
    min <-   as.integer(format(value,"%M"))
    sec <-   as.integer(format(value,"%S"))
    msec <-  if(length(sec)) 0 else numeric(0)
    value <- list(
        year        = year,
        month       = month - 1L,
        date        = day,
        hours       = hour,
        minutes     = min,
        seconds     = sec,
        milliseconds = msec
    )
    value
}


#' @rdname DatetimePicker
#' @export
DatetimePickerClass <- R6Class_("DatetimePicker",
   inherit = ValueWidgetClass,
   public = list(
       #' @field _model_name Name of the Javascript model in the frontend
       `_model_name` = structure(Unicode("DatetimeModel"),sync=TRUE),
       #' @field _view_name Name of the Javascript view in the frontend
       `_view_name` = structure(Unicode("DatetimeView"),sync=TRUE),
       #' @field value The date and time. If non-zero length, must have valid timezone info.
       value = structure(Datetime(),sync=TRUE,from_json=Datetime_from_json,to_json=Datetime_to_json),
       #' @field disabled Boolean, whether the user can make changes 
       disabled = structure(Boolean(FALSE),sync=TRUE),
       #' @field min Minimum selectable date and time. If non-zero length, must have valid timezone info.
       min = structure(Datetime(),sync=TRUE,from_json=Datetime_from_json,to_json=Datetime_to_json),
       #' @field max Maximum selectable date and time. If non-zero length, must have valid timezone info.
       max = structure(Datetime(),sync=TRUE,from_json=Datetime_from_json,to_json=Datetime_to_json),
       #' @description Check whether time zone is valid.
       #' @param value A value
       validate_tz = function(value) {
           timezone <- attr(value,"tz")
           if(!nzchar(timezone)) stop("Explicit timezone definition needed")
           value
       },
       #' @description Check wether "value" is within range.
       #' @param value A date and time to be checked for validity
       validate_value = function(value){
           if(!length(value)) return(value)
           value <- self$validate_tz(value)
           min <- self$min
           max <- self$max
           if(length(min) && value < min)
               value <- min
           if(length(max) && value > max)
               value <- max
           value
       },
       #' @description Validate the "min" field after assignment.
       #' @param min A minimum date and time to be checked for validity
       validate_min = function(min){
           if(!length(min)) return(min)
           min <- self$validate_tz(min)
           max <- self$max
           value <- self$value
           if(length(max) && max < min)
               stop("max >= min required")
           if(length(value) && value < min){
               self$value <- min
               self$send_state()
           }
           min
       },
       #' @description Validate the "max" field after assignment.
       #' @param max A maximum date and time to be checked for validity
       validate_max = function(max){
           if(!length(max)) return(max)
           max <- self$validate_tz(max)
           min <- self$min
           value <- self$value
           if(length(max) && max < min)
               stop("max >= min required")
           if(length(value) && value > max){
               self$value <- max
               self$send_state()
           }
           max
       },
       #' @param ... Arguments passed to the superclass initializer
       initialize = function(...){
           super$initialize(...)
           self$validate("value",self$validate_value)
           self$validate("min",self$validate_min)
           self$validate("max",self$validate_max)
       })
)

#' @describeIn DatetimePicker A constructor for dat picker widgets
#' @param ... Arguments passed to the inializer
#' @export
DatetimePicker <- function(...) DatetimePickerClass$new(...)



NaiveDatetime_from_json <- function(value) {
    if(!length(value)) return(NULL)
    value <- ISOdatetime(
        year =  value$year,
        month = value$month + 1,
        day =   value$date,
        hour =  value$hours,
        min =   value$minutes,
        sec =   value$seconds,
        tz = Sys.timezone()
    )
    value
}

NaiveDatetime_to_json <- function(dateObj) {
    value <- dateObj$value
    if(!length(value)) return(NULL)
    year <-  as.integer(format(value,"%Y"))
    month <- as.integer(format(value,"%m"))
    day <-   as.integer(format(value,"%d"))
    hour <-  as.integer(format(value,"%H"))
    min <-   as.integer(format(value,"%M"))
    sec <-   as.integer(format(value,"%S"))
    msec <-  if(length(sec)) 0 else numeric(0)
    value <- list(
        year        = year,
        month       = month - 1L,
        date        = day,
        hours       = hour,
        minutes     = min,
        seconds     = sec,
        milliseconds = msec
    )
    value
}



#' @rdname DatetimePicker
#' @export
NaiveDatetimePickerClass <- R6Class_("NaiveDatetimePicker",
   inherit = DatetimePickerClass,
   public = list(
       #' @field _model_name Name of the Javascript model in the frontend
       `_model_name` = structure(Unicode("NaiveDatetimeModel"),sync=TRUE),
       #' @field value The date and time. If non-zero length, must have valid timezone info.
       value = structure(Datetime(),sync=TRUE,from_json=NaiveDatetime_from_json,to_json=NaiveDatetime_to_json),
       #' @field min Minimum selectable date and time. If non-zero length, must have valid timezone info.
       min = structure(Datetime(),sync=TRUE,from_json=NaiveDatetime_from_json,to_json=NaiveDatetime_to_json),
       #' @field max Maximum selectable date and time. If non-zero length, must have valid timezone info.
       max = structure(Datetime(),sync=TRUE,from_json=NaiveDatetime_from_json,to_json=NaiveDatetime_to_json),
       #' @description Check whether time zone is valid.
       #' @param value A value
       validate_tz = function(value) {
           timezone <- attr(value,"tz")
           if(nzchar(timezone)) stop("No explicit timezone allowed")
           value
       },
       #' @description Check wether "value" is within range.
       #' @param value A date and time to be checked for validity
       validate_value = function(value){
           if(!length(value)) return(value)
           value <- self$validate_tz(value)
           min <- self$min
           max <- self$max
           if(length(min) && value < min)
               value <- min
           if(length(max) && value > max)
               value <- max
           value
       },
       #' @param ... Arguments passed to the superclass initializer
       initialize = function(...){
           super$initialize(...)
           self$validate("value",self$validate_value)
           self$validate("min",self$validate_min)
           self$validate("max",self$validate_max)
       })
)

#' @describeIn DatetimePicker A constructor for dat picker widgets
#' @param ... Arguments passed to the inializer
#' @export
NaiveDatetimePicker <- function(...) NaiveDatetimePickerClass$new(...)


