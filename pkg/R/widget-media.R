#' Media widgets
#' @description Classes and constructors to wrap media into widgets
#' @include widget-dom.R
#' @name MediaWidget

#' @rdname MediaWidget
#' @export
MediaWidgetClass <- R6Class_("MediaWidget",
    inherit = DOMWidgetClass,
    public = list(
      `_model_module` = structure(Unicode("@jupyter-widgets/controls"),sync=TRUE),
      `_model_module_version` = structure(Unicode(jupyter_widgets_controls_version),sync=TRUE),
      `_view_module` = structure(Unicode("@jupyter-widgets/controls"),sync=TRUE),
      `_view_module_version` = structure(Unicode(jupyter_widgets_controls_version),sync=TRUE),
      value = structure(Bytes(),sync=TRUE),
      on_change = function(handler,remove=FALSE){
          self$observe("value",handler,remove)
      }
    )
)

#' @rdname MediaWidget 
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

#' @describeIn MediaWidget The ImageWidget constructor function
#' @param format A character string, the format of the image
#' @param width The width of the image in pixels
#' @param height The height of the image in pixels
#' @export
ImageWidget <- function(format="png",width,height)
    ImageWidgetClass$new(format=format,width=width,height=height)
