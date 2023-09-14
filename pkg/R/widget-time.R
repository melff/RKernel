#' Datetime Picker Widgets
#' @description An R6 class and constructor function for date picker widgets
#' @include widget-value.R
#' @name DatetimePicker
NULL


Time_from_json <- function(value) {
    if(!length(value)) return(NULL)
    value <- ISOdatetime(
        year =  1970,
        month = 1,
        day =   1,
        hour =  value$hours,
        min =   value$minutes,
        sec =   value$seconds,
        tz = Sys.timezone()
    )
    value
}

Time_to_json <- function(timeObj) {
    value <- timeObj$value
    if(!length(value)) return(NULL)
    hour <-  as.integer(format(value,"%H"))
    min <-   as.integer(format(value,"%M"))
    sec <-   as.integer(format(value,"%S"))
    msec <-  if(length(sec)) 0 else numeric(0)
    value <- list(
        hours       = hour,
        minutes     = min,
        seconds     = sec,
        milliseconds = msec
    )
    value
}


#' @rdname TimePicker
#' @export
TimePickerClass <- R6Class_("TimePicker",
   inherit = ValueWidgetClass,
   public = list(
       #' @field _model_name Name of the Javascript model in the frontend
       `_model_name` = structure(Unicode("TimeModel"),sync=TRUE),
       #' @field _view_name Name of the Javascript view in the frontend
       `_view_name` = structure(Unicode("TimeView"),sync=TRUE),
       #' @field value The date and time. If non-zero length, must have valid timezone info.
       value = structure(Time(),sync=TRUE,from_json=Time_from_json,to_json=Time_to_json),
       #' @field disabled Boolean, whether the user can make changes 
       disabled = structure(Boolean(FALSE),sync=TRUE),
       #' @field min Minimum selectable date and time. If non-zero length, must have valid timezone info.
       min = structure(Time(),sync=TRUE,from_json=Time_from_json,to_json=Time_to_json),
       #' @field max Maximum selectable date and time. If non-zero length, must have valid timezone info.
       max = structure(Time(),sync=TRUE,from_json=Time_from_json,to_json=Time_to_json),
       #' @description Check wether "value" is within range.
       #' @param value A date and time to be checked for validity
       validate_value = function(value){
           if(!length(value)) return(value)
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

#' @describeIn TimePicker A constructor for dat picker widgets
#' @param ... Arguments passed to the inializer
#' @export
TimePicker <- function(...) TimePickerClass$new(...)
