sidecar_module_name <- "@jupyter-widgets/jupyterlab-sidecar"
sidecar_module_version <- "1.1.0"

#' Sidecar widgets
#'
#' @description Sidecar widgets - work only with Jupyter Lab
#'
#' @include widget-output.R
#' @name Sidecar
NULL

#' @rdname Sidecar
#' @export
SidecarClass <- R6Class_("Sidecar",
    inherit = OutputWidgetClass,
    public = list(
        #' @field _model_name Name of the Javascript model in the frontend
        `_model_name` = structure(Unicode("SidecarModel"),sync=TRUE),
        #' @field _model_module Name of the Javascript frontend module
        `_model_module` = structure(Unicode(sidecar_module_name),sync=TRUE),
        #' @field _model_module_version Version of the Javascript frontend module
        `_model_module_version` = structure(Unicode(sidecar_module_version),sync=TRUE),
        #' @field _view_name Name of the Javascript view in the frontend
        `_view_name` = structure(Unicode("SidecarView"),sync=TRUE),
        #' @field _view_module Name of the Javascript frontend view module
        `_view_module` = structure(Unicode(sidecar_module_name),sync=TRUE),
        #' @field _view_module_version Version of the the Javascript view module
        `_view_module_version` = structure(Unicode(sidecar_module_version),sync=TRUE),
        #' @field title A unicode string, the title of the widget.
        title = structure(Unicode("Sidecar"),sync=TRUE),
        #' @field anchor A string that specifies where the widget s to appear: 
        #'     one of "split-right", "split-left", "split-top", "split-bottom",
        #'     "tab-before", "tab-after", or "right".
        anchor = structure(StrEnum(c("split-right","split-left","split-top","split-bottom",
                                     "tab-before","tab-after","right"),
                                   default="right"),sync=TRUE)
    )
)

#' @describeIn Sidecar A constructor for sidebar widgets
#' @param ... Arguments passed to the inializer
#' @export
Sidecar <- function(...) SidecarClass$new(...)
