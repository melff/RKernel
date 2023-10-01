#' File Upload Widgets
#' @description Class and constructor for file upload widgets
#' @include widget-value.R
#' @name FileUpload
NULL


#' @rdname FileUpload
#' @export
FileUploadClass <- R6Class_("FileUpload",
   inherit = ValueWidgetClass,
   public = list(
       #' @field _model_name Name of the Javascript model in the frontend.
       `_model_name` = structure(Unicode("FileUploadModel"),sync=TRUE),
       #' @field _view_name Name of the Javascript view in the frontend.
       `_view_name` = structure(Unicode("FileUploadView"),sync=TRUE),
       #' @field accept A character string that defineds the accepted file type.
       #'    If empty, all files are accepted.
       accept = structure(Unicode(""),sync=TRUE),
       #' @field multiple A Boolean traitlet, whether multiple files are accepted.
       multiple = structure(Boolean(FALSE),sync=TRUE),
       #' @field disabled A \link{Boolean} traitlet, whether the widget is disabled.
       disabled = structure(Boolean(FALSE),sync=TRUE),
       #' @field icon A character string, the font-awesome without the 'fa-' prefix.
       icon = structure(Unicode("upload"),sync=TRUE),
       #' @field box_style The string that describes the button style
       box_style = structure(StrEnum(
           c("primary","success","info","warning","danger",""),
           default="")),
       #' @field style The button style, an object of class "ButtonStyleClass".
       style = structure(R6Instance(ButtonStyleClass),sync=TRUE),
       #' @field error A string with an error message, if applicable.
       error = structure(Unicode(""),sync=TRUE),
       #' @field value The uploaded data.
       value = structure(Unicode(length=NA),sync=TRUE,auto_unbox=FALSE),
       #' @description A generic initializer function
       #' @param ... Any arguments used to initialize the fields of the object
       initialize = function(...){
           super$initialize(...)
           warning("Unfortunately file upload does not work. [Help wanted]")
       }       
   ))


#' @describeIn FileUpload The FileUpload constructor function
#' @param ... Any arguments used to initialize the fields of the object
#' @export
FileUpload <- function(...) FileUploadClass$new(...)
