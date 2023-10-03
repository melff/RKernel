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
       #' @field button_style The string that describes the button style
       button_style = structure(StrEnum(
           c("primary","success","info","warning","danger",""),
           default=""),sync=TRUE),
       #' @field style The button style, an object of class "ButtonStyleClass".
       style = structure(R6Instance(ButtonStyleClass),sync=TRUE),
       #' @field error A string with an error message, if applicable.
       error = structure(Unicode(""),sync=TRUE),
       #' @field value The uploaded data.
       value = structure(List(),sync=TRUE,auto_unbox=FALSE),
       # value = structure(Unicode(length=NA),sync=TRUE,auto_unbox=FALSE),
       #' @description A generic initializer function
       #' @param description The button description
       #' @param ... Any arguments used to initialize the fields of the object
       initialize = function(description="Upload",...){
           super$initialize(description=description,...)
       }
   ),
   active = list(
       #' @field names of the uploaded files
       filename = function(value){
           if(missing(value)) {
               if(length(self$value)) return(sapply(self$value,"[[","name"))
               else return(character(0))
           }
           else stop("file names cannot be changed")
       },
       contents = function(value){
           if(missing(value)) {
               if(length(self$value)) {
                   contents <- lapply(self$value,"[[","content")
                   if(length(contents) == 1) return(contents[[1]])
                   else return(contents)
               }
               else return(NULL)
           }
           else stop("file contents cannot be changed")
       }
   ))


#' @describeIn FileUpload The FileUpload constructor function
#' @param ... Any arguments used to initialize the fields of the object
#' @export
FileUpload <- function(...) FileUploadClass$new(...)
