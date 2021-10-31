#' @include widget.R widget-description.R

#' @export
LabelStyleClass <- R6Class_("LabelStyle",
  inherit = DescriptionStyleClass,
  public = list(
    #`_model_name` = structure(Unicode("LabelStyleModel"),sync=TRUE),
    font_family = structure(Unicode(character(0)),sync=TRUE),
    font_style = structure(Unicode(character(0)),sync=TRUE),
    font_variant = structure(Unicode(character(0)),sync=TRUE),
    font_weight = structure(Unicode(character(0)),sync=TRUE),
    font_decoration = structure(Unicode(character(0)),sync=TRUE)
  )
)

#' @export
LabelClass <- R6Class_("Label",
   inherit = DescriptionWidgetClass,
   public = list(
    `_model_name` = structure(Unicode("LabelModel"),sync=TRUE),
    `_view_name` = structure(Unicode("LabelView"),sync=TRUE),
    value = structure(Unicode("***"),sync=TRUE),
    placeholder = structure(Unicode("\u200b"),sync=TRUE),
    style = structure(R6Instance(LabelStyleClass),sync=TRUE)
   )
)

#' @export
Label <- function(value,...) LabelClass$new(value=value,...)
