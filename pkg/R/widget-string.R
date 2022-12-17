#' @include widget.R widget-description.R


#' @export
StringWidgetClass <- R6Class_("StringWidget",
   inherit = ValueWidgetClass,
   public = list(
    `_model_name` = structure(Unicode("StringModel"),sync=TRUE),
    value = structure(Unicode(character(0)),sync=TRUE),
    placeholder = structure(Unicode("\u200b"),sync=TRUE)
   )
)

#' @export
StringWidget <- function(value=character(0),...) StringWidgetClass$new(value=value,...)


#' @export
HTMLClass <- R6Class_("HTML",
   inherit = StringWidgetClass,
   public = list(
    `_model_name` = structure(Unicode("HTMLModel"),sync=TRUE),
    `_view_name` = structure(Unicode("HTMLView"),sync=TRUE)
   )
)

#' @export
HTML <- function(value=character(0),...) HTMLClass$new(value=value,...)

#' @export
HTMLMathClass <- R6Class_("HTMLMath",
   inherit = StringWidgetClass,
   public = list(
    `_model_name` = structure(Unicode("HTMLMathModel"),sync=TRUE),
    `_view_name` = structure(Unicode("HTMLMathView"),sync=TRUE)
   )
)

#' @export
HTMLMath <- function(value=character(0),...) HTMLMathClass$new(value=value,...)


#' @export
LabelClass <- R6Class_("Label",
   inherit = StringWidgetClass,
   public = list(
    `_model_name` = structure(Unicode("LabelModel"),sync=TRUE),
    `_view_name` = structure(Unicode("LabelView"),sync=TRUE)
   )
)

#' @export
Label <- function(value=character(0),...) LabelClass$new(value=value,...)


#' @export
TextareaClass <- R6Class_("Textarea",
   inherit = StringWidgetClass,
   public = list(
    `_model_name` = structure(Unicode("TextareaModel"),sync=TRUE),
    `_view_name` = structure(Unicode("TextareaView"),sync=TRUE),
    disabled = structure(Boolean(FALSE),sync=TRUE),
    continuous_update = structure(Boolean(FALSE),sync=TRUE),
    rows = structure(Integer(),sync=TRUE)
   )
)

#' @export
Textarea <- function(value=character(0),...) TextareaClass$new(value=value,...)


#' @export
TextWidgetClass <- R6Class_("TextWidget",
   inherit = StringWidgetClass,
   public = list(
    `_model_name` = structure(Unicode("TextModel"),sync=TRUE),
    `_view_name` = structure(Unicode("TextView"),sync=TRUE),
    disabled = structure(Boolean(FALSE),sync=TRUE),
    continuous_update = structure(Boolean(FALSE),sync=TRUE),
    clear = function(){
        self$suspended <- TRUE
        self$value <- character(0)
        self$suspended <- FALSE
        self$send_state()
    }
   )
)

#' @export
TextWidget <- function(value=character(0),...) TextWidgetClass$new(value=value,...)


#' @export
PasswordWidgetClass <- R6Class_("PasswordWidget",
   inherit = TextWidgetClass,
   public = list(
    `_model_name` = structure(Unicode("PasswordModel"),sync=TRUE),
    `_view_name` = structure(Unicode("PasswordView"),sync=TRUE)
   )
)

#' @export
PasswordWidget <- function(value=character(0),...) PasswordWidgetClass$new(value=value,...)



#' @export
ComboboxClass <- R6Class_("Combobox",
   inherit = TextWidgetClass,
   public = list(
    `_model_name` = structure(Unicode("ComboboxModel"),sync=TRUE),
    `_view_name` = structure(Unicode("ComboboxView"),sync=TRUE),
    options = structure(Unicode("",length=NA,optional=FALSE),sync=TRUE),
    ensure_option = structure(Boolean(FALSE),sync=TRUE)
   )
)

#' @export
Combobox <- function(value=character(0),...) ComboboxClass$new(value=value,...)
