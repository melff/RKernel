#' @export
WidgetLinkClass <- R6Class_("WidgetLink",
  inherit = CoreWidgetClass,
  public = list(
      `_model_name` = structure(Unicode("LinkModel"),sync=TRUE),
      source = structure(Unicode(length=2),sync=TRUE),
      target = structure(Unicode(length=2),sync=TRUE),
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

#' @export
WidgetLink <- function(source,target,...) WidgetLinkClass$new(source=source,target=target,...)
