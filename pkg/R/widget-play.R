#' @include widget-integer.R

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

#' @export
Play <- function(value=0L,min=0L,max=100L,interval=100L,step=1L,...) 
    PlayClass$new(value=value,
                  min=min,
                  max=max,
                  interval=interval,
                  step=step,
                  ...)
