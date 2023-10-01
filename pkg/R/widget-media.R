#' Media widgets
#' @description Classes and constructors to wrap media into widgets
#' @include widget-value.R
#' @name MediaWidget
NULL

#' @rdname MediaWidget
#' @export
MediaWidgetClass <- R6Class_("MediaWidget",
    inherit = ValueWidgetClass,
    public = list(
      #' @field format A string, giving the graphics fromat.
      format = structure(Unicode("url"),sync=TRUE),
      #' @field value A \link{Bytes} traitlet.
      value = structure(Bytes(),sync=TRUE),
      #' @description Create media widget from url
      #' @param url A character string
      #' @param width A character string with CSS width specification
      #' @param height A character string with CSS height specification
      from_url = function(url,width=NULL,height=NULL){
          self$value <- charToRaw(url)
          self$format <- "url"
          if(!missing(width)) self$width <- width
          if(!missing(height)) self$height <- height
      },
      #' @description Create media widget from file
      #' @param filename A character string
      from_file = function(filename){
          data <- read_file(filename)
          self$value <- data
      },
      #' @description Add or remove a handler to be called if value 
      #'    is changed.
      #' @param handler A function that is called when the button is clicked.
      #' @param remove Logical value, whether the handler is to be removed.
      on_change = function(handler,remove=FALSE){
          self$observe("value",handler,remove)
      },
      #' @description Initialize an object
      #' @param from_file An optional character string, name of the file from which to initialize the widget.
      #' @param from_url An optional character string, URL from which to initialize the widget.
      #' @param ... Other arguments, passed to the superclass initializer.
      initialize = function(from_file=NULL,from_url=NULL,...){
          super$initialize(...)
          if(!missing(from_file)) self$from_file(filename=from_file)
          else if(!missing(from_url)) self$from_url(url=from_url)
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
        height = structure(Unicode(""),sync=TRUE),
        #' @description Create image widget from file
        #' @param filename A character string
        #' @param width A character string with CSS width specification
        #' @param height A character string with CSS height specification
        from_file = function(filename,width=NULL,height=NULL){
            ext <- get_file_ext(filename)
            fmt <- img_fmts[ext]
            self$format <- fmt
            data <- read_file(filename)
            self$value <- data
            if(!missing(width)) self$width <- width
            if(!missing(height)) self$height <- height
        }
    )
)

#' @describeIn MediaWidget The ImageWidget constructor function
#' @param ... Any arguments used to initialize the fields of the object
#' @export
ImageWidget <- function(...) ImageWidgetClass$new(...)


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
        autoplay = structure(Boolean(FALSE),sync=TRUE),
        #' @field loop Boolean, when TRUE the video restarts after finishing.
        loop = structure(Boolean(FALSE),sync=TRUE),
        #' @field controls Boolean, when TRUE then video controls are shown.
        controls = structure(Boolean(TRUE),sync=TRUE),
        #' @description Create image widget from file
        #' @param filename A character string
        #' @param width A character string with CSS width specification
        #' @param height A character string with CSS height specification
        from_file = function(filename,width=NULL,height=NULL){
            data <- read_file(filename)
            self$value <- data
            if(!missing(width)) self$width <- width
            if(!missing(height)) self$height <- height
        }
    )
)

#' @describeIn MediaWidget The VideoWidget constructor function
#' @param ... Any arguments used to initialize the fields of the object
#' @export
VideoWidget <- function(...) VideoWidgetClass$new(...)


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
        autoplay = structure(Boolean(FALSE),sync=TRUE),
        #' @field loop Boolean, when TRUE the video restarts after finishing.
        loop = structure(Boolean(FALSE),sync=TRUE),
        #' @field controls Boolean, when TRUE then video controls are shown.
        controls = structure(Boolean(TRUE),sync=TRUE)
    )
)

#' @describeIn MediaWidget The AudioWidget constructor function
#' @param ... Any arguments used to initialize the fields of the object
#' @export
AudioWidget <- function(...) AudioWidgetClass$new(...)



get_file_ext <- function(fn){
    bn <- basename(fn)
    ext <- strsplit(bn,".",fixed=TRUE)[[1:2]]
    ext
}

img_fmts <- c(
    png = "png",
    jpg = "jpeg",
    jpeg = "jpeg"
)

read_file <- function(fn){
    con <- file(fn,"rb")
    data <- readBin(con,raw(),n=file.size(fn))
    close(con)
    data
}
