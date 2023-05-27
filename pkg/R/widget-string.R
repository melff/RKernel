#' String widgets
#'
#' @description Classes and constructor functions for string-related widgets (text areas etc.)
#' 
#' @include widget.R widget-description.R
#' @name StringWidget
NULL

#' @rdname StringWidget
#' @export
StringWidgetClass <- R6Class_("StringWidget",
   inherit = ValueWidgetClass,
   public = list(
       #' @field _model_name Name of the Javascript model in the frontend
       `_model_name` = structure(Unicode("StringModel"),sync=TRUE),
       #' @field value A unicode vector
       value = structure(Unicode(character(0)),sync=TRUE),
       #' @field placeholder A placeholder character
       placeholder = structure(Unicode("\u200b"),sync=TRUE)
   )
)

#' @describeIn StringWidget A constructor for string widgets
#' @param value A character vector
#' @param ... Arguments passed to the inializer
#' @export
StringWidget <- function(value=character(0),...) StringWidgetClass$new(value=value,...)


#' @rdname StringWidget
#' @export
HTMLClass <- R6Class_("HTML",
   inherit = StringWidgetClass,
   public = list(
       #' @field _model_name Name of the Javascript model in the frontend
       `_model_name` = structure(Unicode("HTMLModel"),sync=TRUE),
       #' @field _view_name Name of the Javascript view in the frontend
       `_view_name` = structure(Unicode("HTMLView"),sync=TRUE)
   )
)

#' @describeIn StringWidget A constructor for HTML widgets
#' @param value A character vector
#' @param ... Arguments passed to the inializer
#' @export
HTML <- function(value=character(0),...) HTMLClass$new(value=value,...)

#' @rdname StringWidget
#' @export
HTMLMathClass <- R6Class_("HTMLMath",
   inherit = StringWidgetClass,
   public = list(
       #' @field _model_name Name of the Javascript model in the frontend
       `_model_name` = structure(Unicode("HTMLMathModel"),sync=TRUE),
       #' @field _view_name Name of the Javascript view in the frontend
       `_view_name` = structure(Unicode("HTMLMathView"),sync=TRUE)
   )
)

#' @describeIn StringWidget A constructor for HTML widgets with math
#' @param value A character vector
#' @param ... Arguments passed to the inializer
#' @export
HTMLMath <- function(value=character(0),...) HTMLMathClass$new(value=value,...)


#' @rdname StringWidget
#' @export
LabelClass <- R6Class_("Label",
   inherit = StringWidgetClass,
   public = list(
       #' @field _model_name Name of the Javascript model in the frontend
       `_model_name` = structure(Unicode("LabelModel"),sync=TRUE),
       #' @field _view_name Name of the Javascript view in the frontend
       `_view_name` = structure(Unicode("LabelView"),sync=TRUE)
   )
)

#' @describeIn StringWidget A constructor for label widgets
#' @param value A character vector
#' @param ... Arguments passed to the inializer
#' @export
Label <- function(value=character(0),...) LabelClass$new(value=value,...)


#' @rdname StringWidget
#' @export
TextareaClass <- R6Class_("Textarea",
   inherit = StringWidgetClass,
   public = list(
       #' @field _model_name Name of the Javascript model in the frontend
       `_model_name` = structure(Unicode("TextareaModel"),sync=TRUE),
       #' @field _view_name Name of the Javascript view in the frontend
       `_view_name` = structure(Unicode("TextareaView"),sync=TRUE),
       #' @field disabled A logical value, whether the slider is disabled
       disabled = structure(Boolean(FALSE),sync=TRUE),
       #' @field continuous_update A logical value, whether values should be updated as the slider is moved by the user
       continuous_update = structure(Boolean(FALSE),sync=TRUE),
       #' @field rows An integer, the number of rows
       rows = structure(Integer(),sync=TRUE)
   )
)

#' @describeIn StringWidget A constructor for text area widgets
#' @param value A character vector
#' @param ... Arguments passed to the inializer
#' @export
Textarea <- function(value=character(0),...) TextareaClass$new(value=value,...)


#' @rdname StringWidget
#' @export
TextWidgetClass <- R6Class_("TextWidget",
   inherit = StringWidgetClass,
   public = list(
       #' @field _model_name Name of the Javascript model in the frontend
       `_model_name` = structure(Unicode("TextModel"),sync=TRUE),
       #' @field _view_name Name of the Javascript view in the frontend
       `_view_name` = structure(Unicode("TextView"),sync=TRUE),
       #' @field disabled A logical value, whether the slider is disabled
       disabled = structure(Boolean(FALSE),sync=TRUE),
       #' @field continuous_update A logical value, whether values should be updated as the slider is moved by the user
       continuous_update = structure(Boolean(FALSE),sync=TRUE),
       #' @description Clear the text area
       clear = function(){
           self$suspended <- TRUE
           self$value <- character(0)
           self$suspended <- FALSE
           self$send_state()
    }
   )
)

#' @describeIn StringWidget A constructor for text field widgets
#' @param value A character vector
#' @param ... Arguments passed to the inializer
#' @export
TextWidget <- function(value=character(0),...) TextWidgetClass$new(value=value,...)


#' @rdname StringWidget
#' @export
PasswordWidgetClass <- R6Class_("PasswordWidget",
   inherit = TextWidgetClass,
   public = list(
       #' @field _model_name Name of the Javascript model in the frontend
       `_model_name` = structure(Unicode("PasswordModel"),sync=TRUE),
       #' @field _view_name Name of the Javascript view in the frontend
       `_view_name` = structure(Unicode("PasswordView"),sync=TRUE)
   )
)

#' @describeIn StringWidget A constructor for password entry widgets
#' @param value A character vector
#' @param ... Arguments passed to the inializer
#' @export
PasswordWidget <- function(value=character(0),...) PasswordWidgetClass$new(value=value,...)



#' @rdname StringWidget
#' @export
ComboboxClass <- R6Class_("Combobox",
   inherit = TextWidgetClass,
   public = list(
       #' @field _model_name Name of the Javascript model in the frontend
       `_model_name` = structure(Unicode("ComboboxModel"),sync=TRUE),
       #' @field _view_name Name of the Javascript view in the frontend
       `_view_name` = structure(Unicode("ComboboxView"),sync=TRUE),
       #' @field options A unicode vector, the available options
       options = structure(Unicode("",length=NA,optional=FALSE),sync=TRUE),
       #' @field ensure_option A boolean (logical) value, whether at least one option has to be activated
       ensure_option = structure(Boolean(FALSE),sync=TRUE)
   )
)

#' @describeIn StringWidget A constructor for combo boxes
#' @param value A character vector
#' @param ... Arguments passed to the inializer
#' @export
Combobox <- function(value=character(0),...) ComboboxClass$new(value=value,...)
