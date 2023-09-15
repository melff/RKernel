#' String widgets
#'
#' @description Classes and constructor functions for string-related widgets (text areas etc.)
#' 
#' @include widget.R widget-description.R
#' @name StringWidget
NULL

#' @rdname StringWidget
#' @export
StringStyleClass <- R6Class_("StringStyle",
   inherit = DescriptionStyleClass,
   public = list(
    #' @field _model_name Name of the Javascript model in the frontend
    `_model_name` = structure(Unicode("StringStyleModel"),sync=TRUE),
    #' @field background The background color
    background = structure(Unicode(""),sync=TRUE),
    #' @field font_size The font size
    font_size = structure(Unicode(""),sync=TRUE),
    #' @field text_color The text color
    text_color = structure(Unicode(""),sync=TRUE),
    #' @field required_version Minimum required ipywidgets version in which the
    #'        current widget class is supported.
    required_version = list(from=c(8,0,0))
   )
)

#' @rdname StringWidget
#' @export
LabelStyleClass <- R6Class_("LabelStyle",
   inherit = StringStyleClass,
   public = list(
    #' @field _model_name Name of the Javascript model in the frontend
    `_model_name` = structure(Unicode("LabelStyleModel"),sync=TRUE),
    #' @field font_family The font family
    font_family = structure(Unicode(""),sync=TRUE),
    #' @field font_style The font style
    font_style = structure(Unicode(""),sync=TRUE),
    #' @field font_variant The font variant
    font_variant = structure(Unicode(""),sync=TRUE),
    #' @field font_weight The font weight
    font_weight = structure(Unicode(""),sync=TRUE),
    #' @field text_decoration The text decoration
    text_decoration = structure(Unicode(""),sync=TRUE),
    #' @field required_version Minimum required ipywidgets version in which the
    #'        current widget class is supported.
    required_version = list(from=c(8,0,0))
   )
)

#' @describeIn StringWidget The constructor for Label styles
#' @param ... Arguments passed to the inializer
#' @export
LabelStyle <- function(...) LabelStyleClass$new(...)



#' @rdname StringWidget
#' @export
TextStyleClass <- R6Class_("TextStyle",
   inherit = StringStyleClass,
   public = list(
    #' @field _model_name Name of the Javascript model in the frontend
    `_model_name` = structure(Unicode("TextStyleModel"),sync=TRUE),
    #' @field required_version Minimum required ipywidgets version in which the
    #'        current widget class is supported.
     required_version = list(from=c(8,0,0))
   )
)

#' @describeIn StringWidget The constructor for Text styles
#' @param ... Arguments passed to the inializer
#' @export
TextStyle <- function(...) TextStyleClass$new(...)


#' @rdname StringWidget
#' @export
HTMLStyleClass <- R6Class_("HTMLStyle",
   inherit = StringStyleClass,
   public = list(
    #' @field _model_name Name of the Javascript model in the frontend
    `_model_name` = structure(Unicode("HTMLStyleModel"),sync=TRUE),
    #' @field required_version Minimum required ipywidgets version in which the
    #'        current widget class is supported.
     required_version = list(from=c(8,0,0))
   )
)

#' @describeIn StringWidget The constructor for HTML styles
#' @param ... Arguments passed to the inializer
#' @export
HTMLStyle <- function(...) HTMLStyleClass$new(...)


#' @rdname StringWidget
#' @export
HTMLMathStyleClass <- R6Class_("HTMLMathStyle",
   inherit = StringStyleClass,
   public = list(
    #' @field _model_name Name of the Javascript model in the frontend
    `_model_name` = structure(Unicode("HTMLMathStyleModel"),sync=TRUE),
    #' @field required_version Minimum required ipywidgets version in which the
    #'        current widget class is supported.
    required_version = list(from=c(8,0,0))
   )
)

#' @describeIn StringWidget The constructor for HTMLMath styles
#' @param ... Arguments passed to the inializer
#' @export
HTMLMathStyle <- function(...) HTMLMathStyleClass$new(...)




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
       `_view_name` = structure(Unicode("HTMLView"),sync=TRUE),
       #' @field style The HTML style, an object of class "HTMLStyleClass"
       style = structure(R6Instance(HTMLStyleClass),sync=TRUE)
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
       `_view_name` = structure(Unicode("HTMLMathView"),sync=TRUE),
       #' @field style The HTMLMath style, an object of class "HTMLMathStyleClass"
       style = structure(R6Instance(HTMLMathStyleClass),sync=TRUE)
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
       `_view_name` = structure(Unicode("LabelView"),sync=TRUE),
       #' @field style The Label style, an object of class "LabelStyleClass"
       style = structure(R6Instance(LabelStyleClass),sync=TRUE)
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
       rows = structure(Integer(),sync=TRUE),
       #' @field style The Text style, an object of class "TextStyleClass"
       style = structure(R6Instance(TextStyleClass),sync=TRUE)
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
       },
       #' @field style The Text style, an object of class "TextStyleClass"
       style = structure(R6Instance(TextStyleClass),sync=TRUE)
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
