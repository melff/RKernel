#' @include widget.R widget-description.R

#' @export
FloatWidgetClass <- R6Class_("FloatWidget",
  inherit = DescriptionWidgetClass,
  public = list(
      value = structure(Float(0L),sync=TRUE))
)

#' @export
FloatWidget <- function(value,...) FloatWidgetClass$new(value=value,...)

#' @export
BoundedFloatWidgetClass <- R6Class_("BoundedFloatWidget",
  inherit = DescriptionWidgetClass,
  public = list(
    value = structure(Float(0.0),sync=TRUE),
    min = structure(Float(0.0),sync=TRUE),
    max = structure(Float(100.0),sync=TRUE),
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
    initialize = function(value,min,max,...){
        super$initialize(value=value,min=min,max=max,...)
        self$validate("value",self$validate_value)
        self$validate("min",self$validate_min)
        self$validate("max",self$validate_max)
    })
)

#' @export
BoundedFloatWidget <- function(value,min,max,...) 
    BoundedFloatWidgetClass$new(value=value,
                                min=min,
                                max=max,
                                ...)

#' @export
BoundedLogFloatWidgetClass <- R6Class_("BoundedLogFloatWidget",
  inherit = DescriptionWidgetClass,
  public = list(
    value = structure(Float(1.0),sync=TRUE),
    min = structure(Float(0.0),sync=TRUE),
    max = structure(Float(4.0),sync=TRUE),
    base = structure(Float(10.0),sync=TRUE),
    validate_value = function(value){
        if(value > self$base^self$max)
            value <- self$max
        if(value < self$base^self$min)
            value <- self$min
        value
    },
    validate_min = function(min){
        if(min > self$max) stop("min <= max required")
        if(self$base^min > self$value)
            self$value <- self$base^min
        min
    },
    validate_max = function(max){
        if(max < self$min) stop("min <= max required")
        if(self$base^max < self$value)
            self$value <- self$base^max
        max
    },
    initialize = function(value,min,max,...){
        super$initialize(value=value,min=min,max=max,...)
        self$validate("value",self$validate_range)
        self$validate("min",self$validate_min)
        self$validate("max",self$validate_max)
    })
)

#' @export
BoundedLogFloatWidget <- function(value,min,max,base,...) 
    BoundedLogFloatWidgetClass$new(value=value,
                                   min=min,
                                   max=max,
                                   base=base,
                                   ...)

#' @export
BoundedFloatRangeWidgetClass <- R6Class_("BoundedFloatRangeWidget",
  inherit = DescriptionWidgetClass,
  public = list(
      value = structure(Float(c(0,1)),sync=TRUE),
      min = structure(Float(0),sync=TRUE),
      max = structure(Float(100),sync=TRUE),
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
BoundedFloatRangeWidget <- function(value,min,max,...) 
    BoundedFloatRangeWidgetClass$new(value=value,
                                  min=min,
                                  max=max,
                                  ...)


#' @export
FloatTextClass <- R6Class_("FloatText",
   inherit = FloatWidgetClass,
   public = list(
       `_model_name` = structure(Unicode("FloatTextModel"),sync=TRUE),
       `_view_name` = structure(Unicode("FloatTextView"),sync=TRUE),
       disabled = structure(Boolean(FALSE),sync=TRUE),
       continuous_update = structure(Boolean(FALSE),sync=TRUE),
       step = structure(Float(1.0),sync=TRUE)
   ))

#' @export
FloatText <- function(value=0,step=.1,...) FloatTextClass$new(value=value,step=step,...)

#' @export
BoundedFloatTextClass <- R6Class_("BoundedFloatText",
   inherit = BoundedFloatWidgetClass,
   public = list(
       `_model_name` = structure(Unicode("BoundedFloatTextModel"),sync=TRUE),
       `_view_name` = structure(Unicode("FloatTextView"),sync=TRUE),
       disabled = structure(Boolean(FALSE),sync=TRUE),
       continuous_update = structure(Boolean(FALSE),sync=TRUE),
       step = structure(Float(1.0),sync=TRUE)
   ))

#' @export
BoundedFloatText <- function(value=0,min=0,max=100,step=.1,...) 
    BoundedFloatTextClass$new(value=value,min=min,max=max,step=step,...)

