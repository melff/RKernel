#' @export
DescriptionStyleClass <- R6Class("DescriptionStyle",
  inherit = CoreWidgetClass,
  public = list(
    `_model_name` = trait("DescriptionStyleModel",sync=TRUE),
    `_view_name` = trait("StyleView",sync=TRUE),
    description_width = trait(character(0),sync=TRUE)
  )
)
#' @export
DescriptionStyle <- function(...) DescriptionStyleClass$new(...)

#' @export
DescriptionWidgetClass <- R6Class("DescriptionWidget",
  inherit = DOMWidgetClass,
  public = list(
    `_model_name` = trait("DescriptionModel",sync=TRUE),
    description = trait("",sync=TRUE),
    description_allow_html = trait(FALSE,sync=TRUE),
    style = trait(quote(DescriptionStyle()),sync=TRUE)
  )
)
#' @export
DescriptionWidget <- function(...) DescriptionWidgetClass$new(...)
