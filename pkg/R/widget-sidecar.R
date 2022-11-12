sidecar_module_name <- "@jupyter-widgets/jupyterlab-sidecar"
sidecar_module_version <- "1.1.0"

#' @include widget-output.R
#' @name Sidecar Widget
#'
#' @rdname Sidecar
#' @export
SidecarClass <- R6Class_("Sidecar",
    inherit = OutputWidgetClass,
    public = list(
        `_model_name` = structure(Unicode("SidecarModel"),sync=TRUE),
        `_model_module` = structure(Unicode(sidecar_module_name),sync=TRUE),
        `_model_module_version` = structure(Unicode(sidecar_module_version),sync=TRUE),
        `_view_name` = structure(Unicode("SidecarView"),sync=TRUE),
        `_view_module` = structure(Unicode(sidecar_module_name),sync=TRUE),
        `_view_module_version` = structure(Unicode(sidecar_module_version),sync=TRUE),

        title = structure(Unicode("Sidecar"),sync=TRUE),
        anchor = structure(StrEnum(c("split-right","split-left","split-top","split-bottom",
                                     "tab-before","tab-after","right"),
                                   default="right"),sync=TRUE)
    )
)

#' @export
Sidecar <- function(...) SidecarClass$new(...)
