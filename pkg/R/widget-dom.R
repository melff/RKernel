#' @export
DOMWidgetClass <- R6Class("DOMWidget",
  inherit = CoreWidgetClass,
  public = list(
    `_dom_classes` = trait(character(0),sync=TRUE),
    tabbable = trait(FALSE,optional=TRUE,sync=TRUE),
    tooltip = trait(character(0),optional=TRUE,sync=TRUE),
    layout = trait(quote(Layout()),sync=TRUE),
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

