#' @include widget.R widget-core.R

#' @export
DescriptionStyleClass <- R6Class_("DescriptionStyle",
  inherit = CoreWidgetClass,
  public = list(
    `_model_name` = structure(Unicode("DescriptionStyleModel"),sync=TRUE),
    `_view_name` = structure(Unicode("StyleView"),sync=TRUE),
    description_width = structure(Unicode(character(0)),sync=TRUE)
  )
)
#' @export
DescriptionStyle <- function(...) DescriptionStyleClass$new(...)

#' @export
DescriptionWidgetClass <- R6Class_("DescriptionWidget",
  inherit = DOMWidgetClass,
  public = list(
    `_model_name` = structure(Unicode("DescriptionModel"),sync=TRUE),
    description = structure(Unicode(""),sync=TRUE),
    description_tooltip = structure(Unicode(""),sync=TRUE),
    style = structure(R6Instance(DescriptionStyleClass),sync=TRUE)
  )
)

#' @export
DescriptionWidget <- function(...) DescriptionWidgetClass$new(...)
