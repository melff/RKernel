#' @include widget.R widget-core.R widget-layout.R

#' @export
DOMWidgetClass <- R6Class_("DOMWidget",
  inherit = WidgetClass,
  public = list(
    `_dom_classes` = structure(Unicode(length=NA),sync=TRUE,auto_unbox=FALSE),
    `_model_module` = structure(Unicode("@jupyter-widgets/base"),sync=TRUE),
    `_model_module_version` = structure(Unicode(jupyter_widgets_base_version),sync=TRUE),
    `_model_name` = structure(Unicode("DOMWidgetModel"),sync=TRUE),
    layout = structure(R6Instance(LayoutClass),sync=TRUE),
    add_class = function(className){
      if(!length(self$`_dom_classes`))
          dom_classes <- className
      else {
          dom_classes <- self$`_dom_classes`
          dom_classes <- union(dom_classes,className)
      }
      self$`_dom_classes` <- dom_classes
    },
    remove_class = function(className){
      if(length(self$`_dom_classes`)) {
          dom_classes <- self$`_dom_classes`
          dom_classes <- setdiff(dom_classes,className)
          self$`_dom_classes` <- dom_classes
      }
    }
  )
)

#' @export
DOMWidget <- function(...) DOMWidgetClass$new(...)


# Local Variables:
# ess-indent-offset: 2
# End:

