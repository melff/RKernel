#' @include widget.R widget-core.R widget-layout.R

#' @export
DOMWidgetClass <- R6Class_("DOMWidget",
  inherit = CoreWidgetClass,
  public = list(
    `_dom_classes` = structure(Unicode(character(0)),sync=TRUE),
    tabbable = structure(Boolean(logical(0)),sync=TRUE),
    tooltip = structure(Unicode(character(0)),sync=TRUE),
    #layout = structure(R6Instance(LayoutClass),sync=TRUE),
    add_class = function(className){
      self$`_dom_classes` <- union(self$`_dom_classes`,className)
    },
    remove_class = function(className){
      self$`_dom_classes` <- setdiff(self$`_dom_classes`,className)
    },
    focus = function() self$send(list(do="focus")),
    blur = function() self$send(list(do="blur"))
  )
)

#' @export
DOMWidget <- function(...) DOMWidgetClass$new(...)


# Local Variables:
# ess-indent-offset: 2
# End:

