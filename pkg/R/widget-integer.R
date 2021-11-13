#' @include widget.R widget-description.R

#' @export
IntegerWidgetClass <- R6Class_("IntegerWidget",
  inherit = DescriptionWidgetClass,
  public = list(
      value = structure(Integer(0L),sync=TRUE),
      initialize = function(value,...){
          super$initialize(...)
          self$value <- value
      }
  )
)

#' @export
IntegerWidget <- function(value,...) IntegerWidgetClass$new(value=value,...)

#' @export
BoundedIntegerWidgetClass <- R6Class_("BoundedIntegerWidget",
  inherit = DescriptionWidgetClass,
  public = list(
      value = structure(Integer(0L),sync=TRUE),
      min = structure(Integer(0L),sync=TRUE),
      max = structure(Integer(100L),sync=TRUE),
      validate_value = function(value){
          if(value > self$max)
              value <- self$max
          if(value < self$min)
              value <- self$min
          value
      },
      validate_min = function(min){
          if(min > self$max) stop("min <= max required")
          if(min > self$value)
              self$value <- min
          min
      },
      validate_max = function(max){
          if(max < self$min) stop("min <= max required")
          if(max < self$value)
              self$value <- max
          max
      },
      initialize = function(...){
          super$initialize(...)
          self$validate("value",self$validate_value)
          self$validate("min",self$validate_min)
          self$validate("max",self$validate_max)
      })
  )

#' @export
BoundedIntegerWidget <- function(value,min,max,...) 
    BoundedIntegerWidgetClass$new(value=value,
                                  min=min,
                                  max=max,
                                  ...)


#' @export
BoundedIntRangeWidgetClass <- R6Class_("BoundedIntRangeWidget",
  inherit = DescriptionWidgetClass,
  public = list(
      value = structure(Integer(c(0L,1L)),sync=TRUE),
      min = structure(Integer(0L),sync=TRUE),
      max = structure(Integer(100L),sync=TRUE),
      validate_value = function(value){
          if(length(value) > 2)
              value <- value[1:2]
          else if(length(value) < 2)
              value <- c(value,value)
          value <- sort(value)
          if(value[2] > self$max)
              value[2] <- self$max
          if(value[1] < self$min)
              value[1] <- self$min
          value
      },
      validate_min = function(min){
          if(min > self$max) stop("min <= max required")
          if(min > self$value[1])
              self$value[1] <- min
          if(min > self$value[2])
              self$value[2] <- min
          min
      },
      validate_max = function(max){
          if(max < self$min) stop("min <= max required")
          if(max < self$value[2])
              self$value[2] <- max
          if(max < self$value[1])
              self$value[1] <- max
          max
      },
      initialize = function(...){
          super$initialize(...)
          self$validate("value",self$validate_value)
          self$validate("min",self$validate_min)
          self$validate("max",self$validate_max)
      })
  )

#' @export
BoundedIntRangeWidget <- function(value,min,max,...) 
    BoundedIntRangeWidgetClass$new(value=value,
                                  min=min,
                                  max=max,
                                  ...)

#' @export
IntTextClass <- R6Class_("IntText",
   inherit = IntegerWidgetClass,
   public = list(
       `_model_name` = structure(Unicode("IntTextModel"),sync=TRUE),
       `_view_name` = structure(Unicode("IntTextView"),sync=TRUE),
       disabled = structure(Boolean(FALSE),sync=TRUE),
       continuous_update = structure(Boolean(FALSE),sync=TRUE),
       step = structure(Integer(1L),sync=TRUE)
   ))

#' @export
IntText <- function(value=0,step=1,...) IntTextClass$new(value=value,step=step,...)

#' @export
BoundedIntTextClass <- R6Class_("BoundedIntText",
   inherit = BoundedIntegerWidgetClass,
   public = list(
       `_model_name` = structure(Unicode("BoundedIntTextModel"),sync=TRUE),
       `_view_name` = structure(Unicode("IntTextView"),sync=TRUE),
       disabled = structure(Boolean(FALSE),sync=TRUE),
       continuous_update = structure(Boolean(FALSE),sync=TRUE),
       step = structure(Integer(1L),sync=TRUE)
   ))

#' @export
BoundedIntText <- function(value=0,min=0,max=100,step=1,...) 
    IntTextClass$new(value=value,min=min,max=max,step=step,...)
