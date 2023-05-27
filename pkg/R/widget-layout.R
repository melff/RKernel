#' Widget Layout Manipulation
#' @description An R6 class and a constructor function for the creation of
#'     a layout widget, which itself is used to manipulate the layout of
#'     a \code{\link{DOMWidget}}.
#' @details The function \code{Layout} creates objects of the R6 Class
#'     "LayoutClass", which in turn have the S3 class attribute "Layout"
#' @include widget.R
#' @name Layout
NULL

#' @rdname Layout
#' @export
LayoutClass <- R6Class_("Layout",
   inherit = WidgetClass,
   public = list(
    #' @field _view_name Name of the Javascript view in the frontend.
    `_view_name` = structure(Unicode("LayoutView"),sync=TRUE),
    #' @field _view_module Name of the Javascript view module in the frontend.
    `_view_module` = structure(Unicode("@jupyter-widgets/base"),sync=TRUE),
    #' @field _view_module_version Version of the Javascript view module in the frontend.
    `_view_module_version` = structure(Unicode(jupyter_widgets_base_version),sync=TRUE),
    #' @field _model_name Name of the Javascript model in the frontend.
    `_model_name` = structure(Unicode("LayoutModel"),sync=TRUE),
    #' @field align_content An optional string, if non-empty, one of "flex-start", "flex-end",
    #'                      "center", "space-between", "space-around", "space-evenly", "stretch"
    align_content = structure(StrEnum(c("flex-start","flex-end","center","space-between",
                                        "space-around","space-evenly","stretch"),
                                      optional=TRUE),
                            sync=TRUE),
    #' @field align_items An optional string, if non-empty, one of "flex-start", "flex-end",
    #'                      "center", "baseline", "stretch"
    align_items = structure(StrEnum(c("flex-start","flex-end","center","baseline","stretch"),
                                    optional=TRUE),
                            sync=TRUE),
    #' @field align_self An optional string, if non-empty, one of "flex-start", "flex-end",
    #'                      "center", "baseline", "stretch"
    align_self = structure(StrEnum(c("auto","flex-start","flex-end","center","baseline","stretch"),
                                   optional=TRUE),
                           sync=TRUE),
    # border_top = structure(Unicode(character(0)),sync=TRUE),
    # border_right = structure(Unicode(character(0)),sync=TRUE),
    # border_bottom = structure(Unicode(character(0)),sync=TRUE),
    # border_left = structure(Unicode(character(0)),sync=TRUE),
    #' @field bottom Position from bottom, an optional string that should, if non-empty, contain a valid CSS dimension
    bottom = structure(Unicode(character(0)),sync=TRUE),
    #' @field border An optional string with a valid CSS border specification
    border = structure(Unicode(character(0)),sync=TRUE),
    #' @field display An optional string with a valid CSS display property
    display = structure(Unicode(character(0)),sync=TRUE),
    #' @field flex An optional string with a valid CSS flex property
    flex = structure(Unicode(character(0)),sync=TRUE),
    #' @field flex_flow An optional string with a valid CSS flex_flow property
    flex_flow = structure(Unicode(character(0)),sync=TRUE),
    #' @field height An optional string with a valid CSS height 
    height = structure(Unicode(character(0)),sync=TRUE),
    #' @field justify_content An optional string, if non-empty, one of "flex-start", "flex-end",
    #'      "center", "space-between", "space-around".
    justify_content = structure(StrEnum(c("flex-start","flex-end","center","space-between",
                                          "space-around"),
                                        optional=TRUE),
                                sync=TRUE),
    #' @field justify_items An optional string, if non-empty, one of "flex-start", "flex-end",
    #'                        or "center"
    justify_items = structure(StrEnum(c("flex-start","flex-end","center"),optional=TRUE),
                              sync=TRUE),
    #' @field left Position from left, an optional string that should, if non-empty, contain a valid CSS dimension
    left = structure(Unicode(character(0)),sync=TRUE),
    #' @field margin An optional string, if non-empty, should be a valid CSS margin specification
    margin = structure(Unicode(character(0)),sync=TRUE),
    #' @field max_height An optional string, if non-emtpy, should be a valid CSS dimension
    max_height = structure(Unicode(character(0)),sync=TRUE),
    #' @field max_width An optional string, if non-emtpy, should be a valid CSS dimension
    max_width = structure(Unicode(character(0)),sync=TRUE),
    #' @field min_height An optional string, if non-emtpy, should be a valid CSS dimension
    min_height = structure(Unicode(character(0)),sync=TRUE),
    #' @field min_width An optional string, if non-emtpy, should be a valid CSS dimension
    min_width = structure(Unicode(character(0)),sync=TRUE),
    #' @field overflow An optonal string, if non-empty, should be a valid CSS overflow specification
    overflow = structure(Unicode(character(0)),sync=TRUE),
    #' @field order An optional string, if non-empty should contain a number
    order = structure(Unicode(character(0)),sync=TRUE),
    #' @field padding An optional string, if non-emtpy should be a valid CSS dimension
    padding = structure(Unicode(character(0)),sync=TRUE),
    #' @field right Position from right, an optional string, if non-empty, should be a valid CSS dimension
    right = structure(Unicode(character(0)),sync=TRUE),
    #' @field top Position from top, an optional string, if non-empty, should be a valid CSS dimension
    top = structure(Unicode(character(0)),sync=TRUE),
    #' @field visibility An optional string, if non-empty, should be either "visible" or "hidden"
    visibility = structure(StrEnum(c("visible","hidden"),
                                   optional=TRUE),sync=TRUE),
    #' @field width An optional string, if non-empty, should be a valid CSS dimension
    width = structure(Unicode(character(0),optional=TRUE),sync=TRUE),
    #' @field object_fit An optional string, if non-empty, should be one of "contain", "cover",
    #'               "fill", "scale-down", "none"
    object_fit = structure(StrEnum(c("contain","cover","fill","scale-down","none"),
                                   optional=TRUE),sync=TRUE),
    #' @field object_position An optional string, if non-empty, should be a valid CSS object-position specification
    object_position = structure(Unicode(character(0)),sync=TRUE),
    #' @field grid_auto_columns An optional string, if non-empty should be valid CSS code for the grid-auto-columns 
    #'       property
    grid_auto_columns = structure(Unicode(character(0)),sync=TRUE),
    #' @field  grid_auto_flow An optional string, if non-empty should be valid CSS code for the grid-auto-flow
    #'       property
    grid_auto_flow = structure(StrEnum(c("column","row","row dense","column dense"),
                                       optional=TRUE),sync=TRUE),
    #' @field grid_auto_rows An optional string, if non-empty should be valid CSS code for the grid-auto-rows 
    #'       property
    grid_auto_rows = structure(Unicode(character(0)),sync=TRUE),
    #' @field grid_gap An optional string, if non-empty should be valid CSS code for the grid-gap 
    #'       property
    grid_gap = structure(Unicode(character(0)),sync=TRUE),
    #' @field grid_template_rows An optional string, if non-empty should be valid CSS code for the
    #'       grid-template-rows property
    grid_template_rows = structure(Unicode(character(0)),sync=TRUE),
    #' @field grid_template_columns An optional string, if non-empty should be valid CSS code for the
    #'       grid-template-columns property
    grid_template_columns = structure(Unicode(character(0)),sync=TRUE),
    #' @field grid_template_areas An optional string, if non-empty should be valid CSS code for the
    #'       grid-template-areas property
    grid_template_areas = structure(Unicode(character(0)),sync=TRUE),
    #' @field grid_row An optional string, if non-empty should be valid CSS code for the
    #'       grid-row property
    grid_row = structure(Unicode(character(0)),sync=TRUE),
    #' @field grid_column An optional string, if non-empty should be valid CSS code for the
    #'       grid-column property
    grid_column = structure(Unicode(character(0)),sync=TRUE),
    #' @field grid_area An optional string, if non-empty should be valid CSS code for the
    #'       grid-area property
    grid_area = structure(Unicode(character(0)),sync=TRUE)
   )#, For later ipywidgets version
   # active=list(
   #   border = function(value){
   #     if(missing(value)){
   #       found <- NULL
   #       for(side in c("top","right","bottom","left")){
   #         old <- found
   #         border_side <- self[[paste0("border_",side)]]
   #         found <- as.character(border_side)
   #         if(!length(found)) return(NULL)
   #         if(length(old) && found != old) return(NULL)
   #       }
   #       return(found)
   #     }
   #     for(side in c("top","right","bottom","left")){
   #       self[[paste0("border_",side)]] <- value
   #     }
   #   }
   # )
)

#' @describeIn Layout The Layout constructor function
#' @param ... Arguments passed to the inializer
#' @export
Layout <- function(...) LayoutClass$new(...)

# Local Variables:
# ess-indent-offset: 2
# End:

