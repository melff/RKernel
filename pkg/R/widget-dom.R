#' @export
DOMWidgetClass <- R6Class_("DOMWidget",
  inherit = CoreWidgetClass,
  public = list(
    `_dom_classes` = Unicode(character(0),sync=TRUE),
    tabbable = Boolean(logical(0),sync=TRUE),
    tooltip = Unicode(character(0),sync=TRUE),
    layout = Instance(LayoutClass,sync=TRUE),
    add_class = function(className){
      self$state$`_dom_classes` <- union(self$state$`_dom_classes`,className)
    },
    remove_class = function(className){
      self$state$`_dom_classes` <- setdiff(self$state$`_dom_classes`,className)
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

