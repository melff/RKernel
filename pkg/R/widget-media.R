#' Media widgets
#' @description Classes and constructors to wrap media into widgets
#' @include widget-dom.R
#' @name MediaWidget

#' @rdname MediaWidget
#' @export
MediaWidgetClass <- R6Class_("MediaWidget",
    inherit = DOMWidgetClass,
    public = list(
      #' @field _model_module Name of the Javascript model in the frontend.
      `_model_module` = structure(Unicode("@jupyter-widgets/controls"),sync=TRUE),
      #' @field _model_module_version Version of the Javascript model module in the frontend.
      `_model_module_version` = structure(Unicode(jupyter_widgets_controls_version()),sync=TRUE),
      #' @field _view_module Name of the Javascript view module in the frontend.
      `_view_module` = structure(Unicode("@jupyter-widgets/controls"),sync=TRUE),
      #' @field _view_module_version Version of the Javascript view module in the frontend.
      `_view_module_version` = structure(Unicode(jupyter_widgets_controls_version()),sync=TRUE),
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
        #' @field height A string, describing the height in CSS language, e.g. "480px".
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


#' @rdname MediaWidget 
#' @export
VideoWidgetClass <- R6Class_("VideoWidget",
    inherit = MediaWidgetClass,
    public =list(
        #' @field _view_name Name of the Javascript view in the frontend.
        `_view_name` = structure(Unicode("VideoView"),sync=TRUE),
        #' @field _model_name Name of the Javascript model in the frontend.
        `_model_name` = structure(Unicode("VideoModel"),sync=TRUE),
        #' @field format A string, giving the video fromat.
        format = structure(Unicode("mp4"),sync=TRUE),
        #' @field width A string, describing the width in CSS language, e.g. "480px".
        width = structure(Unicode(""),sync=TRUE),
        #' @field height A string, describing the height in CSS language, e.g. "480px".
        height = structure(Unicode(""),sync=TRUE),
        #' @field autoplay Boolean, when TRUE the video starts when it is displayed.
        autoplay = structure(Boolean(TRUE),sync=TRUE),
        #' @field loop Boolean, when TRUE the video restarts after finishing.
        loop = structure(Boolean(TRUE),sync=TRUE),
        #' @field controls Boolean, when TRUE then video controls are shown.
        controls = structure(Boolean(TRUE),sync=TRUE)
    )
)

#' @describeIn MediaWidget The VideoWidget constructor function
#' @param format A character string, the format of the image
#' @param width The width of the image in pixels
#' @param height The height of the image in pixels
#' @param autoplay Logical, when TRUE the video starts when it is displayed.
#' @param loop Logical, when TRUE the video restarts after finishing.
#' @param controls Logical, when TRUE then video controls are shown.
#' @export
VideoWidget <- function(format="mp4",width,height,autoplay,loop,controls)
    VideoWidgetClass$new(format=format,width=width,height=height,autoplay=autoplay,loop=loop,controls=controls)


#' @rdname MediaWidget 
#' @export
AudioWidgetClass <- R6Class_("AudioWidget",
    inherit = MediaWidgetClass,
    public =list(
        #' @field _view_name Name of the Javascript view in the frontend.
        `_view_name` = structure(Unicode("AudioView"),sync=TRUE),
        #' @field _model_name Name of the Javascript model in the frontend.
        `_model_name` = structure(Unicode("AudioModel"),sync=TRUE),
        #' @field format A string, giving the audio fromat.
        format = structure(Unicode("mp3"),sync=TRUE),
        #' @field autoplay Boolean, when TRUE the video starts when it is displayed.
        autoplay = structure(Boolean(TRUE),sync=TRUE),
        #' @field loop Boolean, when TRUE the video restarts after finishing.
        loop = structure(Boolean(TRUE),sync=TRUE),
        #' @field controls Boolean, when TRUE then video controls are shown.
        controls = structure(Boolean(TRUE),sync=TRUE)
    )
)

#' @describeIn MediaWidget The AudioWidget constructor function
#' @param format A character string, the format of the image
#' @param autoplay Logical, when TRUE the video starts when it is displayed.
#' @param loop Logical, when TRUE the video restarts after finishing.
#' @param controls Logical, when TRUE then video controls are shown.
#' @export
AudioWidget <- function(format="mp3",autoplay,loop,controls)
    AudioWidgetClass$new(format=format,autoplay=autoplay,loop=loop,controls=controls)
