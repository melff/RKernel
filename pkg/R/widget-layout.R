#' @include widget.R

#' @export
LayoutClass <- R6Class_("Layout",
   inherit = WidgetClass,
   public = list(
     `_view_name` = structure(Unicode("LayoutView"),sync=TRUE),
    `_view_module` = structure(Unicode("@jupyter-widgets/base"),sync=TRUE),
    `_view_module_version` = structure(Unicode(jupyter_widgets_base_version),sync=TRUE),
     `_model_name` = structure(Unicode("LayoutModel"),sync=TRUE),
    align_content = structure(StrEnum(c("flex-start","flex-end","center","space-between",
                                        "space-around","space-evenly","stretch"),
                                      optional=TRUE),
                            sync=TRUE),
    align_items = structure(StrEnum(c("flex-start","flex-end","center","baseline","stretch"),
                                    optional=TRUE),
                            sync=TRUE),
    align_self = structure(StrEnum(c("auto","flex-start","flex-end","center","baseline","stretch"),
                                   optional=TRUE),
                           sync=TRUE),
    # border_top = structure(Unicode(character(0)),sync=TRUE),
    # border_right = structure(Unicode(character(0)),sync=TRUE),
    # border_bottom = structure(Unicode(character(0)),sync=TRUE),
    # border_left = structure(Unicode(character(0)),sync=TRUE),
    bottom = structure(Unicode(character(0)),sync=TRUE),
    border = structure(Unicode(character(0)),sync=TRUE),
    display = structure(Unicode(character(0)),sync=TRUE),
    flex = structure(Unicode(character(0)),sync=TRUE),
    flex_flow = structure(Unicode(character(0)),sync=TRUE),
    height = structure(Unicode(character(0)),sync=TRUE),
    justify_content = structure(StrEnum(c("flex-start","flex-end","center","space-between",
                                          "space-around"),
                                        optional=TRUE),
                                sync=TRUE),
    justify_items = structure(StrEnum(c("flex-start","flex-end","center"),optional=TRUE),
                              sync=TRUE),
    left = structure(Unicode(character(0)),sync=TRUE),
    margin = structure(Unicode(character(0)),sync=TRUE),
    max_height = structure(Unicode(character(0)),sync=TRUE),
    max_width = structure(Unicode(character(0)),sync=TRUE),
    min_height = structure(Unicode(character(0)),sync=TRUE),
    min_width = structure(Unicode(character(0)),sync=TRUE),
    overflow = structure(Unicode(character(0)),sync=TRUE),
    order = structure(Unicode(character(0)),sync=TRUE),
    padding = structure(Unicode(character(0)),sync=TRUE),
    right = structure(Unicode(character(0)),sync=TRUE),
    top = structure(Unicode(character(0)),sync=TRUE),
    visibility = structure(StrEnum(c("visible","hidden"),
                                   optional=TRUE),sync=TRUE),
    width = structure(Unicode(character(0),optional=TRUE),sync=TRUE),
    object_fit = structure(StrEnum(c("contain","cover","fill","scale-down","none"),
                                   optional=TRUE),sync=TRUE),
    object_position = structure(Unicode(character(0)),sync=TRUE),
    grid_auto_columns = structure(Unicode(character(0)),sync=TRUE),
    grid_auto_flow = structure(StrEnum(c("column","row","row dense","column dense"),
                                       optional=TRUE),sync=TRUE),
    grid_auto_rows = structure(Unicode(character(0)),sync=TRUE),
    grid_gap = structure(Unicode(character(0)),sync=TRUE),
    grid_template_rows = structure(Unicode(character(0)),sync=TRUE),
    grid_template_columns = structure(Unicode(character(0)),sync=TRUE),
    grid_template_areas = structure(Unicode(character(0)),sync=TRUE),
    grid_row = structure(Unicode(character(0)),sync=TRUE),
    grid_column = structure(Unicode(character(0)),sync=TRUE),
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

#' @export
Layout <- function(...) LayoutClass$new(...)

# Local Variables:
# ess-indent-offset: 2
# End:

