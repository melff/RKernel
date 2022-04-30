#' Widgets for Integer Numbers
#' @description An R6 class and a constructor function for the creation of
#'     widgets that can be used to manipulate integer numbers
#' @details The function \code{IntegerWidget} creates objects of the R6 Class
#'     "IntegerWidgetClass", which in turn have the S3 class attribute "IntegerWidget"
#' @include widget-value.R
#' @name IntegerWidget

#' @rdname IntegerWidget
#' @export
IntegerWidgetClass <- R6Class_("IntegerWidget",
  inherit = ValueWidgetClass,
  public = list(
      value = structure(Integer(0L),sync=TRUE)
  )
)

#' @rdname IntegerWidget
#' @param value The integer value
#' @param ... Other arguments, passed to the superclass initializer
#' @export
IntegerWidget <- function(value,...) IntegerWidgetClass$new(value=value,...)

#' Widgets for Bounded Integer Numbers
#' @description An R6 class and a constructor function for the creation of
#'     widgets that can be used to manipulate integer numbers that are bounded within an interval
#' @details The function \code{BoundedIntegerWidget} creates objects of the R6 Class
#'     "BoundedIntegerWidgetClass", which in turn have the S3 class attribute "BoundedIntegerWidget"
#' @name BounedIntegerWidget
NULL

#' @rdname BoundedIntegerWidget
#' @export
BoundedIntegerWidgetClass <- R6Class_("BoundedIntegerWidget",
  inherit = ValueWidgetClass,
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

#' @rdname BoundedIntegerWidget
#' @param value The integer value
#' @param min The lower bound of the interval
#' @param max The upper bound of the interval
#' @param ... Other arguments, passed to the superclass initializer
#' @export
BoundedIntegerWidget <- function(value,min,max,...) 
    BoundedIntegerWidgetClass$new(value=value,
                                  min=min,
                                  max=max,
                                  ...)

#' @rdname BoundedIntegerRangeWidget
#' @param value A pair of integer values
#' @param min The lower bound of the enclosing interval
#' @param max The upper bound of the enclosing interval
#' @param ... Other arguments, passed to the superclass initializer
#' @export
BoundedIntegerRangeWidgetClass <- R6Class_("BoundedIntegerRangeWidget",
  inherit = ValueWidgetClass,
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

#' @rdname BoundedIntegerRangeWidget
#' @param value A pair of integer values
#' @param min The lower bound of the enclosing interval
#' @param max The upper bound of the enclosing interval
#' @param ... Other arguments, passed to the superclass initializer
#' @export
BoundedIntegerRangeWidget <- function(value,min,max,...) 
    BoundedIntegerRangeWidgetClass$new(value=value,
                                  min=min,
                                  max=max,
                                  ...)

#' Widgets for Text Elements with Integer Numbers
#' @description An R6 class and a constructor function for the creation of
#'     widgets that can be used to manipulate integer numbers
#' @details The function \code{IntegerText} creates objects of the R6 Class
#'     "IntegerTextClass", which in turn have the S3 class attribute "IntegerText"
#' @name IntegerText

#' @rdname IntegerText
#' @export
IntegerTextClass <- R6Class_("IntegerText",
   inherit = IntegerWidgetClass,
   public = list(
       `_model_name` = structure(Unicode("IntTextModel"),sync=TRUE),
       `_view_name` = structure(Unicode("IntTextView"),sync=TRUE),
       disabled = structure(Boolean(FALSE),sync=TRUE),
       continuous_update = structure(Boolean(FALSE),sync=TRUE),
       step = structure(Integer(1L),sync=TRUE)
   ))

#' @rdname IntegerText
#' @param value Initial value of the integer number
#' @param step Increment by which the number is increased or decreased by the
#'     text field controls
#' @export
IntegerText <- function(value=0,step=1,...) IntegerTextClass$new(value=value,step=step,...)

#' Widgets for Text Elements with Integer Numbers Bounded within an Interval
#' @description An R6 class and a constructor function for the creation of
#'     widgets that can be used to manipulate integer numbers
#' @details The function \code{BoundedIntegerText} creates objects of the R6 Class
#'     "BoundedIntegerTextClass", which in turn have the S3 class attribute "BoundedIntegerText"
#' @name BoundedIntegerText

#' @rdname BoundedIntegerText
#' @export
BoundedIntegerTextClass <- R6Class_("BoundedIntegerText",
   inherit = BoundedIntegerWidgetClass,
   public = list(
       `_model_name` = structure(Unicode("BoundedIntTextModel"),sync=TRUE),
       `_view_name` = structure(Unicode("IntTextView"),sync=TRUE),
       disabled = structure(Boolean(FALSE),sync=TRUE),
       continuous_update = structure(Boolean(FALSE),sync=TRUE),
       step = structure(Integer(1L),sync=TRUE)
   ))

#' @rdname BoundedIntegerText
#' @param value Initial value of the integer number
#' @param min Lower limit of the enclosing interval
#' @param max Upper limit of the enclosing interval
#' @param step Increment by which the number is increased or decreased by the
#'     text field controls
#' @export
BoundedIntegerText <- function(value=0,min=0,max=100,step=1,...) 
    IntegerTextClass$new(value=value,min=min,max=max,step=step,...)
