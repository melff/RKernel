#' Widgets for Integer Numbers
#' @description An R6 class and a constructor function for the creation of
#'     widgets that can be used to manipulate integer numbers
#' @details The function \code{IntWidget} creates objects of the R6 Class
#'     "IntWidgetClass", which in turn have the S3 class attribute "IntWidget"
#' @include widget-value.R
#' @name IntWidget

#' @rdname IntWidget
#' @export
IntWidgetClass <- R6Class_("IntWidget",
  inherit = ValueWidgetClass,
  public = list(
      value = structure(Integer(0L),sync=TRUE)
  )
)

#' @rdname IntWidget
#' @param value The integer value
#' @param ... Other arguments, passed to the superclass initializer
#' @export
IntWidget <- function(value,...) IntWidgetClass$new(value=value,...)

#' Widgets for Bounded Integer Numbers
#' @description An R6 class and a constructor function for the creation of
#'     widgets that can be used to manipulate integer numbers that are bounded within an interval
#' @details The function \code{BoundedIntWidget} creates objects of the R6 Class
#'     "BoundedIntWidgetClass", which in turn have the S3 class attribute "BoundedIntWidget"
#' @name BounedIntWidget
NULL

#' @rdname BoundedIntWidget
#' @export
BoundedIntWidgetClass <- R6Class_("BoundedIntWidget",
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

#' @rdname BoundedIntWidget
#' @param value The integer value
#' @param min The lower bound of the interval
#' @param max The upper bound of the interval
#' @param ... Other arguments, passed to the superclass initializer
#' @export
BoundedIntWidget <- function(value,min,max,...) 
    BoundedIntWidgetClass$new(value=value,
                                  min=min,
                                  max=max,
                                  ...)

#' @rdname BoundedIntRangeWidget
#' @param value A pair of integer values
#' @param min The lower bound of the enclosing interval
#' @param max The upper bound of the enclosing interval
#' @param ... Other arguments, passed to the superclass initializer
#' @export
BoundedIntRangeWidgetClass <- R6Class_("BoundedIntRangeWidget",
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

#' @rdname BoundedIntRangeWidget
#' @param value A pair of integer values
#' @param min The lower bound of the enclosing interval
#' @param max The upper bound of the enclosing interval
#' @param ... Other arguments, passed to the superclass initializer
#' @export
BoundedIntRangeWidget <- function(value,min,max,...) 
    BoundedIntRangeWidgetClass$new(value=value,
                                  min=min,
                                  max=max,
                                  ...)

#' Widgets for Text Elements with Integer Numbers
#' @description An R6 class and a constructor function for the creation of
#'     widgets that can be used to manipulate integer numbers
#' @details The function \code{IntText} creates objects of the R6 Class
#'     "IntTextClass", which in turn have the S3 class attribute "IntText"
#' @name IntText

#' @rdname IntText
#' @export
IntTextClass <- R6Class_("IntText",
   inherit = IntWidgetClass,
   public = list(
       `_model_name` = structure(Unicode("IntTextModel"),sync=TRUE),
       `_view_name` = structure(Unicode("IntTextView"),sync=TRUE),
       disabled = structure(Boolean(FALSE),sync=TRUE),
       continuous_update = structure(Boolean(FALSE),sync=TRUE),
       step = structure(Integer(1L),sync=TRUE)
   ))

#' @rdname IntText
#' @param value Initial value of the integer number
#' @param step Increment by which the number is increased or decreased by the
#'     text field controls
#' @export
IntText <- function(value=0,step=1,...) IntTextClass$new(value=value,step=step,...)

#' Widgets for Text Elements with Integer Numbers Bounded within an Interval
#' @description An R6 class and a constructor function for the creation of
#'     widgets that can be used to manipulate integer numbers
#' @details The function \code{BoundedIntText} creates objects of the R6 Class
#'     "BoundedIntTextClass", which in turn have the S3 class attribute "BoundedIntText"
#' @name BoundedIntText

#' @rdname BoundedIntText
#' @export
BoundedIntTextClass <- R6Class_("BoundedIntText",
   inherit = BoundedIntWidgetClass,
   public = list(
       `_model_name` = structure(Unicode("BoundedIntTextModel"),sync=TRUE),
       `_view_name` = structure(Unicode("IntTextView"),sync=TRUE),
       disabled = structure(Boolean(FALSE),sync=TRUE),
       continuous_update = structure(Boolean(FALSE),sync=TRUE),
       step = structure(Integer(1L),sync=TRUE)
   ))

#' @rdname BoundedIntText
#' @param value Initial value of the integer number
#' @param min Lower limit of the enclosing interval
#' @param max Upper limit of the enclosing interval
#' @param step Increment by which the number is increased or decreased by the
#'     text field controls
#' @export
BoundedIntText <- function(value=0,min=0,max=100,step=1,...) 
    IntTextClass$new(value=value,min=min,max=max,step=step,...)
