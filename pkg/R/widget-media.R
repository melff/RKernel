#' @include widget-dom.R

#' @export
MediaWidgetClass <- R6Class_("MediaWidget",
    inherit = DOMWidgetClass,
    public = list(
      value = structure(Bytes(),sync=TRUE),
      on_change = function(handler,remove=FALSE){
          self$observe("value",handler,remove)
      }
    )
)

#' @export
ImageWidgetClass <- R6Class_("ImageWidget",
    inherit = MediaWidgetClass,
    public =list(
        `_view_name` = structure(Unicode("ImageView"),sync=TRUE),
        `_model_name` = structure(Unicode("ImageModel"),sync=TRUE),
        format = structure(Unicode("png"),sync=TRUE),
        width = structure(Unicode(""),sync=TRUE),
        height = structure(Unicode(""),sync=TRUE)
    )
)

#' @export
ImageWidget <- function(...) ImageWidgetClass$new(...)
