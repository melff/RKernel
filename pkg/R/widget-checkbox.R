#' @include widget.R widget-description.R

# Needed only for later ipywidgets version
#' @export
CheckboxStyleClass <- R6Class_("CheckboxStyle",
  inherit = DescriptionStyleClass,
  public = list(
    `_model_name` = structure(Unicode("CheckboxStyleModel"),sync=TRUE),
    backbround = structure(Unicode(character(0)),sync=TRUE)
  )
)

#' @export
CheckboxClass <- R6Class_("Checkbox",
   inherit = DescriptionWidgetClass,
   public = list(
    `_model_name` = structure(Unicode("CheckboxModel"),sync=TRUE),
    `_view_name` = structure(Unicode("CheckboxView"),sync=TRUE),
    disabled = structure(Boolean(FALSE),sync=TRUE),
    indent = structure(Boolean(TRUE),sync=TRUE),
    style = structure(R6Instance(DescriptionStyleClass),sync=TRUE),
    value = structure(Boolean(FALSE),sync=TRUE)
   )
)

#' @export
Checkbox <- function(...) CheckboxClass$new(...)
