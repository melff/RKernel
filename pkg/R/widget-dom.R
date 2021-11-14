#' @include widget.R widget-core.R widget-layout.R

#' @export
DOMWidgetClass <- R6Class_("DOMWidget",
  inherit = CoreWidgetClass,
  public = list(
    `_dom_classes` = structure(Vector(),sync=TRUE),
    layout = structure(R6Instance(LayoutClass),sync=TRUE),
    add_class = function(className){
      self$`_dom_classes` <- as.list(unlist(union(self$`_dom_classes`),className))
    },
    remove_class = function(className){
      self$`_dom_classes` <- as.list(setdiff(unlist(self$`_dom_classes`),className))
    }
  )
)

#' @export
DOMWidget <- function(...) DOMWidgetClass$new(...)


# Local Variables:
# ess-indent-offset: 2
# End:

