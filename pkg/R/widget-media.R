#' Media widgets
#' @description Classes and constructors to wrap media into widgets
#' @include widget-dom.R
#' @name MediaWidget

#' @rdname MediaWidget
#' @export
MediaWidgetClass <- R6Class_("MediaWidget",
    inherit = DOMWidgetClass,
    public = list(
      #' @field _model_name Name of the Javascript model in the frontend.
      `_model_module` = structure(Unicode("@jupyter-widgets/controls"),sync=TRUE),
      #' @field _model_module_version Version of the Javascript model module in the frontend.
      `_model_module_version` = structure(Unicode(jupyter_widgets_controls_version),sync=TRUE),
      #' @field _view_module_version Version of the Javascript view module in the frontend.
      `_view_module` = structure(Unicode("@jupyter-widgets/controls"),sync=TRUE),
      #' @field _view_module_version Version of the Javascript view module in the frontend.
      `_view_module_version` = structure(Unicode(jupyter_widgets_controls_version),sync=TRUE),
      #' @field value A \link{Bytes} traitlet.
      value = structure(Bytes(),sync=TRUE),
      #' @description Add or remove a handler to be called if value 
      #'    is changed.
      #' @param handler A function that is called when the button is clicked.
      #' @param remove Logical value, whether the handler is to be removed.
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
        #' @field _view_name Name of the Javascript view in the frontend.
        `_view_name` = structure(Unicode("ImageView"),sync=TRUE),
        #' @field _model_name Name of the Javascript model in the frontend.
        `_model_name` = structure(Unicode("ImageModel"),sync=TRUE),
        #' @field format A string, giving the graphics fromat.
        format = structure(Unicode("png"),sync=TRUE),
        #' @field width A string, describing the width in CSS language, e.g. "480px".
        width = structure(Unicode(""),sync=TRUE),
        #' @field width A string, describing the height in CSS language, e.g. "480px".
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
