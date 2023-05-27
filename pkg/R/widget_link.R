#' Synchronize two Widgets Using a Link
#' @description An R6 class and a constructor function for the creation of
#'     a link widget, which links two widgets so that their values
#'     are synchronized
#' @details The function \code{WidgetLink} creates objects of the R6 Class
#'     "WidgetLinkClass", which in turn have the S3 class attribute "WidgetLink"
#' @include widget.R
#' @name WidgetLink
NULL

#' @rdname WidgetLink
#' @export
WidgetLinkClass <- R6Class_("WidgetLink",
  inherit = CoreWidgetClass,
  public = list(
      #' @field _model_name Name of the Javascript model in the frontend.
      `_model_name` = structure(Unicode("LinkModel"),sync=TRUE),
      #' @field source A pair of Unicode strings, the first is the JSON representation 
      #'    of a widget, the second is the name of a trait(let) of the widget.
      source = structure(Unicode(length=2),sync=TRUE),
      #' @field target A pair of Unicode strings, the first is the JSON representation 
      #'    of a widget, the second is the name of a trait(let) of the widget.
      target = structure(Unicode(length=2),sync=TRUE),
      #' @description An initializer method
      #' @param source A list with two elements, a widget and the name of a trait(let).
      #' @param target A list with two elements, a widget and the name of a trait(let).
      #' @param ... Futher arguments, passed to the superclass initializer.
      initialize = function(source,target,...){
          super$initialize(...)
          source_widget <- source[[1]]
          source_name <- source[[2]]
          target_widget <- target[[1]]
          target_name <- target[[2]]
          self$source <- c(to_json(source_widget),source_name)
          self$target <- c(to_json(target_widget),target_name)
      }
  )
)

#' @describeIn WidgetLink The WidgetLink constructor function
#' @param source A link with two elements, the first is a widget, the second is one of its traits.
#' @param target A link with two elements, the first is a widget, the second is one of its traits.
#' @param ... Other arguments passed to the inializer
#' @export
WidgetLink <- function(source,target,...) WidgetLinkClass$new(source=source,target=target,...)
