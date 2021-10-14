#' @export
DescriptionStyleClass <- R6Class_("DescriptionStyle",
  inherit = CoreWidgetClass,
  public = list(
    `_model_name` = Unicode("DescriptionStyleModel",sync=TRUE),
    `_view_name` = Unicode("StyleView",sync=TRUE),
    description_width = Unicode(character(0),sync=TRUE)
  )
)
#' @export
DescriptionStyle <- function(...) DescriptionStyleClass$new(...)

#' @export
DescriptionWidgetClass <- R6Class_("DescriptionWidget",
  inherit = DOMWidgetClass,
  public = list(
    `_model_name` = Unicode("DescriptionModel",sync=TRUE),
    description = Unicode("",sync=TRUE),
    description_allow_html = Boolean(FALSE,sync=TRUE),
    style = Instance(DescriptionStyleClass,sync=TRUE)
  )
)
#' @export
DescriptionWidget <- function(...) DescriptionWidgetClass$new(...)
