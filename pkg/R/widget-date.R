#' Date Picker Widgets
#' @description An R6 class and constructor function for date picker widgets
#' @include widget-value.R
#' @name DatePicker
NULL


Date_from_json <- function(value) {
    datestr <- paste(value$year,
                     value$month+1,
                     value$date,
                     sep="-")
    value <- as.Date(datestr,"%Y-%m-%d")
    # log_out(value,use.print=TRUE)
    
    value
}

Date_to_json <- function(dateObj) {
    value <- dateObj$value
    year <- as.integer(format(value,"%Y"))
    month <- as.integer(format(value,"%m"))
    day <- as.integer(format(value,"%d"))
    value <- list(year=year,month=month-1L,date=day)
    value
}


#' @rdname DatePicker
#' @export
DatePickerClass <- R6Class_("DatePicker",
   inherit = ValueWidgetClass,
   public = list(
       #' @field _model_name Name of the Javascript model in the frontend
       `_model_name` = structure(Unicode("DatePickerModel"),sync=TRUE),
       #' @field _view_name Name of the Javascript view in the frontend
       `_view_name` = structure(Unicode("DatePickerView"),sync=TRUE),
       #' @field value The date
       value = structure(Date(),sync=TRUE,from_json=Date_from_json,to_json=Date_to_json),
       #' @field disabled Boolean, whether the user can make changes 
       disabled = structure(Boolean(FALSE),sync=TRUE),
       #' @field min Minimum selectable date
       min = structure(Date(),sync=TRUE,from_json=Date_from_json,to_json=Date_to_json),
       #' @field max Maximum selectable date
       max = structure(Date(),sync=TRUE,from_json=Date_from_json,to_json=Date_to_json),
       #' @field step Date step used for the picker in days
       step = structure(Integer(1),sync=TRUE),
       #' @description Check wether "value" is within range.
       #' @param value A value
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
       #' @param min A minimum value, should be an integer number.
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
       #' @param max A maximum value, should be an integer number.
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

#' @describeIn DatePicker A constructor for dat picker widgets
#' @param ... Arguments passed to the inializer
#' @export
DatePicker <- function(...) DatePickerClass$new(...)


