#' Widgets for Floating Point Numbers
#' @description An R6 class and a constructor function for the creation of
#'     widgets that can be used to manipulate floating point numbers
#' @details The function \code{FloatWidget} creates objects of the R6 Class
#'     "FloatWidgetClass", which in turn have the S3 class attribute "FloatWidget"
#' @include widget-value.R
#' @name FloatWidget
NULL

#' @rdname FloatWidget
#' @export
FloatWidgetClass <- R6Class_("FloatWidget",
  inherit = ValueWidgetClass,
  public = list(
    #' @field value A \link{Float} traitlet
    value = structure(Float(0L),sync=TRUE))
)

#' @rdname FloatWidget
#' @param value The floating point value
#' @param ... Other arguments, passed to the superclass initializer
#' @export
FloatWidget <- function(value,...) FloatWidgetClass$new(value=value,...)

#' Widgets for Bounded Floating Point Numbers
#' @description An R6 class and a constructor function for the creation of
#'     widgets that can be used to manipulate floating point numbers that are bounded within an interval
#' @details The function \code{BoundedFloatWidget} creates objects of the R6 Class
#'     "BoundedFloatWidgetClass", which in turn have the S3 class attribute "BoundedFloatWidget"
#' @name BounedFloatWidget
NULL

#' @rdname BoundedFloatWidget
#' @export
BoundedFloatWidgetClass <- R6Class_("BoundedFloatWidget",
  inherit = ValueWidgetClass,
  public = list(
    #' @field value A \link{Float} traitlet.
    value = structure(Float(0.0),sync=TRUE),
    #' @field min A \link{Float} traitlet, the minimum allowed value.
    min = structure(Float(0.0),sync=TRUE),
    #' @field max A \link{Float} traitlet, the maximum allowed value.
    max = structure(Float(100.0),sync=TRUE),
    #' @description Validate the "value" after assignment.
    #' @param value A value, should be numeric.
    validate_value = function(value){
        if(value > self$max)
            value <- self$max
        if(value < self$min)
            value <- self$min
        value
    },
    #' @description Validate the "min" field after assignment.
    #' @param min A minimum value, should be numeric.
    validate_min = function(min){
        if(min > self$max) stop("min <= max required")
        if(min > self$value)
            self$value <- min
        min
    },
    #' @description Validate the "max" field after assignment.
    #' @param max A maximum value, should be numeric.
    validate_max = function(max){
        if(max < self$min) stop("min <= max required")
        if(max < self$value)
            self$value <- max
        max
    },
    #' @description Initialize an object.
    #' @param value The floating point value.
    #' @param min The lower bound of the interval.
    #' @param max The upper bound of the interval.
    #' @param ... Other arguments, passed to the superclass initializer.
    initialize = function(value,min,max,...){
        super$initialize(value=value,min=min,max=max,...)
        self$validate("value",self$validate_value)
        self$validate("min",self$validate_min)
        self$validate("max",self$validate_max)
    })
)

#' @rdname BoundedFloatWidget
#' @param value The floating point value
#' @param min The lower bound of the interval
#' @param max The upper bound of the interval
#' @param ... Other arguments, passed to the superclass initializer
#' @export
BoundedFloatWidget <- function(value,min,max,...) 
    BoundedFloatWidgetClass$new(value=value,
                                min=min,
                                max=max,
                                ...)

#' Widgets for Bounded Floating Point Numbers on a logarithmic scale
#' @description An R6 class and a constructor function for the creation of
#'     widgets that can be used to manipulate floating point numbers that are
#'     bounded within an interval on an logarithmic scale
#' @details The function \code{BoundedLogFloatWidget} creates objects of the R6
#'     Class "BoundedLogFloatWidgetClass", which in turn have the S3 class
#'     attribute "BoundedLogFloatWidget"
#' @name BounedLogFloatWidget
NULL

#' @rdname BoundedLogFloatWidget
#' @export
BoundedLogFloatWidgetClass <- R6Class_("BoundedLogFloatWidget",
  inherit = ValueWidgetClass,
  public = list(
    #' @field value A \link{Float} traitlet.
    value = structure(Float(1.0),sync=TRUE),
    #' @field min A \link{Float} traitlet, the minimum allowed value.
    min = structure(Float(0.0),sync=TRUE),
    #' @field max A \link{Float} traitlet, the maximum allowed value.
    max = structure(Float(4.0),sync=TRUE),
    #' @field base A \link{Float} traitlet, the logarithmic base.
    base = structure(Float(10.0),sync=TRUE),
    #' @description Validate the "value" after assignment.
    #' @param value A value, should be numeric.
    validate_value = function(value){
        if(value > self$base^self$max)
            value <- self$max
        if(value < self$base^self$min)
            value <- self$min
        value
    },
    #' @description Validate the "min" field after assignment.
    #' @param min A minimum value, should be numeric.
    validate_min = function(min){
        if(min > self$max) stop("min <= max required")
        if(self$base^min > self$value)
            self$value <- self$base^min
        min
    },
    #' @description Validate the "max" field after assignment.
    #' @param max A maximum value, should be numeric.
    validate_max = function(max){
        if(max < self$min) stop("min <= max required")
        if(self$base^max < self$value)
            self$value <- self$base^max
        max
    },
    #' @description Initialize an object.
    #' @param value The floating point value.
    #' @param min The lower bound of the interval.
    #' @param max The upper bound of the interval.
    #' @param ... Other arguments, passed to the superclass initializer.
    initialize = function(value,min,max,...){
        super$initialize(value=value,min=min,max=max,...)
        self$validate("value",self$validate_range)
        self$validate("min",self$validate_min)
        self$validate("max",self$validate_max)
    })
)

#' @rdname BoundedLogFloatWidget
#' @param value The floating point value.
#' @param min The lower bound of the interval.
#' @param max The upper bound of the interval.
#' @param base The base of the logarithm.
#' @param ... Other arguments, passed to the superclass initializer.
#' @export
BoundedLogFloatWidget <- function(value,min,max,base,...) 
    BoundedLogFloatWidgetClass$new(value=value,
                                   min=min,
                                   max=max,
                                   base=base,
                                   ...)

#' Widgets for Floating Point Number Ranges
#' @description An R6 class and a constructor function for the creation of
#'     widgets that can be used to manipulate floating pairs of point numbers
#'     that are bounded within an interval, where a pair defines a number range.
#' @details The function \code{BoundedFloatRangeWidget} creates objects of the R6
#'     Class "BoundedFloatRangeWidgetClass", which in turn have the S3 class
#'     attribute "BoundedFloatRangeWidget"
#' @name BoundedFloatRangeWidget
NULL

#' @rdname BoundedFloatRangeWidget
#' @export
BoundedFloatRangeWidgetClass <- R6Class_("BoundedFloatRangeWidget",
  inherit = ValueWidgetClass,
  public = list(
    #' @field value A \link{Float} traitlet.
    value = structure(Float(c(0,1)),sync=TRUE),
    #' @field min A \link{Float} traitlet, the minimum allowed value.
    min = structure(Float(0),sync=TRUE),
    #' @field max A \link{Float} traitlet, the maximum allowed value.
    max = structure(Float(100),sync=TRUE),
    #' @description Validate the "value" after assignment.
    #' @param value A value, should be numeric.
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
    #' @description Validate the "min" field after assignment.
    #' @param min A minimum value, should be numeric.
    validate_min = function(min){
        if(min > self$max) stop("min <= max required")
        if(min > self$value[1])
            self$value[1] <- min
        if(min > self$value[2])
            self$value[2] <- min
        min
    },
    #' @description Validate the "max" field after assignment.
    #' @param max A maximum value, should be numeric.
    validate_max = function(max){
        if(max < self$min) stop("min <= max required")
        if(max < self$value[2])
            self$value[2] <- max
        if(max < self$value[1])
            self$value[1] <- max
        max
    },
    #' @param ... Arguments passed to the superclass initializer.
    initialize = function(...){
        super$initialize(...)
        self$validate("value",self$validate_value)
        self$validate("min",self$validate_min)
        self$validate("max",self$validate_max)
    })
  )

#' @rdname BoundedFloatRangeWidget
#' @param value A pair of floating point values.
#' @param min The lower bound of the enclosing interval.
#' @param max The upper bound of the enclosing interval.
#' @param ... Other arguments, passed to the superclass initializer.
#' @export
BoundedFloatRangeWidget <- function(value,min,max,...) 
    BoundedFloatRangeWidgetClass$new(value=value,
                                  min=min,
                                  max=max,
                                  ...)

#' Widgets for Text Elements with Floating Point Numbers
#' @description An R6 class and a constructor function for the creation of
#'     widgets that can be used to manipulate floating point numbers.
#' @details The function \code{FloatText} creates objects of the R6 Class
#'     "FloatTextClass", which in turn have the S3 class attribute "FloatText".
#' @name FloatText

#' @rdname FloatText
#' @export
FloatTextClass <- R6Class_("FloatText",
   inherit = FloatWidgetClass,
   public = list(
       #' @field _model_name Name of the Javascript model in the frontend.
       `_model_name` = structure(Unicode("FloatTextModel"),sync=TRUE),
       #' @field _view_name Name of the Javascript view in the frontend.
       `_view_name` = structure(Unicode("FloatTextView"),sync=TRUE),
       #' @field A \link{Boolean} traitlet, whether the text widget is disabled.
       disabled = structure(Boolean(FALSE),sync=TRUE),
       #' @field A \link{Boolean} traitlet, whether the text widget is
       #'    continuously updated upon change in the frontend.
       continuous_update = structure(Boolean(FALSE),sync=TRUE),
       #' @field A \link{Float} traitlet, a step size by which the 
       #'   value is incremented or decremented if the arrows are clicked.
       step = structure(Float(1.0),sync=TRUE)
   ))

#' @rdname FloatText
#' @param value Initial value of the floating point number.
#' @param step Increment by which the number is increased or decreased by the
#'     text field controls.
#' @param ... Other arguments.
#' @export
FloatText <- function(value=0,step=.1,...) FloatTextClass$new(value=value,step=step,...)

#' Widgets for Text Elements with Floating Point Numbers Bounded within an Interval
#' @description An R6 class and a constructor function for the creation of
#'     widgets that can be used to manipulate floating point numbers.
#' @details The function \code{BoundedFloatText} creates objects of the R6 Class
#'     "BoundedFloatTextClass", which in turn have the S3 class attribute "BoundedFloatText".
#' @name BoundedFloatText

#' @rdname BoundedFloatText
#' @export
BoundedFloatTextClass <- R6Class_("BoundedFloatText",
   inherit = BoundedFloatWidgetClass,
   public = list(
       #' @field _model_name Name of the Javascript model in the frontend.
       `_model_name` = structure(Unicode("BoundedFloatTextModel"),sync=TRUE),
       #' @field _view_name Name of the Javascript view in the frontend.
       `_view_name` = structure(Unicode("FloatTextView"),sync=TRUE),
       #' @field A \link{Boolean} traitlet, whether the text widget is disabled.
       disabled = structure(Boolean(FALSE),sync=TRUE),
       #' @field A \link{Boolean} traitlet, whether the text widget is
       #'    continuously updated upon change in the frontend.
       continuous_update = structure(Boolean(FALSE),sync=TRUE),
       #' @field A \link{Float} traitlet, a step size by which the 
       #'   value is incremented or decremented if the arrows are clicked.
       step = structure(Float(1.0),sync=TRUE)
   ))

#' @rdname BoundedFloatText
#' @param value Initial value of the floating point number.
#' @param min Lower limit of the enclosing interval.
#' @param max Upper limit of the enclosing interval.
#' @param step Increment by which the number is increased or decreased by the
#'     text field controls.
#' @export
BoundedFloatText <- function(value=0,min=0,max=100,step=.1,...) 
    BoundedFloatTextClass$new(value=value,min=min,max=max,step=step,...)

