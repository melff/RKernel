#' A Base Class for DOM Widgets
#'
#' @description This is a base class for all widgets that are supposed to be
#'   part of the document object model
#' @include widget.R widget-core.R widget-layout.R
#' @export
DOMWidgetClass <- R6Class_("DOMWidget",
  inherit = WidgetClass,
  public = list(
    #' @field _model_module Name of the Javascript module with the model
    `_model_module` = structure(Unicode("@jupyter-widgets/base"),sync=TRUE),
    #' @field _model_module_version Version of the module where the model is defined
    `_model_module_version` = structure(Unicode(jupyter_widgets_base_version()),sync=TRUE),
    #' @field _model_name Name of the Javascript model in the frontend
    `_model_name` = structure(Unicode("DOMWidgetModel"),sync=TRUE),
    #' @field _dom_classes A set of character strings that indicate the DOM
    #'        classes the widget is assigned to
    `_dom_classes` = structure(Unicode(length=NA),sync=TRUE,auto_unbox=FALSE),
    #' @field layout The layout, a "LayoutClass" Widget
    layout = structure(R6Instance(LayoutClass),sync=TRUE),
    #' @description Add a class attribute to the DOM element
    #' @param className Name of the class attribute
    add_class = function(className){
      if(!length(self$`_dom_classes`))
          dom_classes <- className
      else {
          dom_classes <- self$`_dom_classes`
          dom_classes <- union(dom_classes,className)
      }
      self$`_dom_classes` <- dom_classes
    },
    #' @description Remove a class attribute to the DOM element
    #' @param className Name of the class attribute
    remove_class = function(className){
      if(length(self$`_dom_classes`)) {
          dom_classes <- self$`_dom_classes`
          dom_classes <- setdiff(dom_classes,className)
          self$`_dom_classes` <- dom_classes
      }
    },
    #' @description Check whether the DOM element has a class attribute
    #' @param className Name of the class attribute
    has_class = function(className){
      className %in% self[["_dom_classes"]]
    }
  )
)

#' @describeIn DOMWidgetClass The DOM widget constructor function
#' @param ... Arguments passed to the inializer
#' @export
DOMWidget <- function(...) DOMWidgetClass$new(...)


# Local Variables:
# ess-indent-offset: 2
# End:

