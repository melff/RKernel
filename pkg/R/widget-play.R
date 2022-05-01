#' A Player Widget
#'
#' @description An R6 class and a constructor function for the creation of
#'    a player widget, which automatically increases its value-
#' 
#' @include widget-integer.R
#' @name Play

#' @rdname Play
#' @export
PlayClass <- R6Class_("Play",
   inherit = BoundedIntegerWidgetClass,
   public = list(
       `_model_name` = structure(Unicode("PlayModel"),sync=TRUE),
       `_view_name` = structure(Unicode("PlayView"),sync=TRUE),
       interval = structure(Integer(100L),sync=TRUE),
       step = structure(Integer(1L),sync=TRUE),
       `_playing` = structure(Boolean(FALSE),sync=TRUE),
       `_repeat` = structure(Boolean(FALSE),sync=TRUE),
       show_repeat = structure(Boolean(TRUE),sync=TRUE)
   ))

#' @describeIn Play The player widget constructor function
#' @param value Integer, an initial value
#' @param min Integer, the minimum value
#' @param max Integer, the maximum value
#' @param interval Integer, the maximum value of the intrval 
#' @param step The maximum value for the play control
#' @param show_repeat Logical, whether to show a repeat toggle button
#' @export
Play <- function(value=0L,
                 min=0L,
                 max=100L,
                 interval=100L,
                 step=1L,
                 show_repeat=TRUE,
                 ...) 
    PlayClass$new(value=value,
                  min=min,
                  max=max,
                  interval=interval,
                  step=step,
                  show_repeat=show_repeat,
                  ...)
