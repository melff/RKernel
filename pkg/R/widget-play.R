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
   inherit = BoundedIntWidgetClass,
   public = list(
        #' @field _model_name Name of the Javascript model in the frontend
       `_model_name` = structure(Unicode("PlayModel"),sync=TRUE),
        #' @field _view_name Name of the Javascript model view in the frontend
       `_view_name` = structure(Unicode("PlayView"),sync=TRUE),
       #' @field interval An Integer traitlet, the time interval between
       #'    between two steps.
       interval = structure(Integer(100L),sync=TRUE),
       #' @field step An Integer traitlet, the step size.
       step = structure(Integer(1L),sync=TRUE),
       #' @field _playing A Boolean traitlet, indicates wether the player widget
       #'    is running.
       `_playing` = structure(Boolean(FALSE),sync=TRUE),
       #' @field _repeat A Boolean traitlet, indicates wether the the repeat toggle
       #'    is on.
       `_repeat` = structure(Boolean(FALSE),sync=TRUE),
       #' @field show_repeat A Boolean traitlet, determines whether
       #'    to show a repeat toggle button.
       show_repeat = structure(Boolean(TRUE),sync=TRUE)
   ))

#' @describeIn Play The player widget constructor function.
#' @param value Integer, an initial value.
#' @param min Integer, the minimum value.
#' @param max Integer, the maximum value.
#' @param interval Integer, the maximum value of the intrval .
#' @param step The maximum value for the play control.
#' @param show_repeat Logical, whether to show a repeat toggle button.
#' @param ... Further arguments, passed to the superclass constructor.
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
