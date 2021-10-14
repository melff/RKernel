#' @export
LayoutClass <- R6Class_("Layout",
   inherit = WidgetClass,
   public = list(
     `_view_name` = Unicode("LayoutView",sync=TRUE),
    `_view_module` = Unicode("@jupyter-widgets/controls",sync=TRUE),
    `_view_module_version` = trait(jupyter_widgets_controls_version,sync=TRUE),
     `_model_name` = Unicode("LayoutModel",sync=TRUE),
    align_content = StrEnum(c("flex-start","flex-end","center","space-between",
                            "space-around","space-evenly","stretch"),
                          optional=TRUE,sync=TRUE),
    align_items = StrEnum(c("flex-start","flex-end","center","baseline","stretch"),
                        optional=TRUE,sync=TRUE),
    align_self = StrEnum(c("auto","flex-start","flex-end","center","baseline","stretch"),
                        optional=TRUE,sync=TRUE),
    border_top = Unicode(character(0),sync=TRUE),
    border_right = Unicode(character(0),sync=TRUE),
    border_bottom = Unicode(character(0),sync=TRUE),
    border_left = Unicode(character(0),sync=TRUE),
    bottom = Unicode(character(0),sync=TRUE),
    display = Unicode(character(0),sync=TRUE),
    flex = Unicode(character(0),sync=TRUE),
    flex_flow = Unicode(character(0),sync=TRUE),
    height = Unicode(character(0),sync=TRUE),
    justify_content = StrEnum(c("flex-start","flex-end","center","space-between",
                            "space-around"),optional=TRUE,sync=TRUE),
    justify_items = traitStrEnum(c("flex-start","flex-end","center"),optional=TRUE,sync=TRUE),
    left = Unicode(character(0),sync=TRUE),
    margin = Unicode(character(0),sync=TRUE),
    max_height = Unicode(character(0),sync=TRUE),
    max_width = Unicode(character(0),sync=TRUE),
    min_height = Unicode(character(0),sync=TRUE),
    min_width = Unicode(character(0),sync=TRUE),
    overflow = Unicode(character(0),sync=TRUE),
    order = Unicode(character(0),sync=TRUE),
    padding = Unicode(character(0),sync=TRUE),
    right = Unicode(character(0),sync=TRUE),
    top = Unicode(character(0),sync=TRUE),
    visibility = StrEnum(c("visible","hidden"),
                        optional=TRUE,sync=TRUE),
    width = Unicode(character(0),optional=TRUE,sync=TRUE),
    object_fit = StrEnum(c("contain","cover","fill","scale-down","none"),
                        optional=TRUE,sync=TRUE),
    object_position = Unicode(character(0),sync=TRUE),
    grid_auto_columns = Unicode(character(0),sync=TRUE),
    grid_auto_flow = StrEnum(c("column","row","row dense","column dense"),optional=TRUE,sync=TRUE),
    grid_auto_rows = Unicode(character(0),sync=TRUE),
    grid_gap = Unicode(character(0),sync=TRUE),
    grid_template_rows = Unicode(character(0),sync=TRUE),
    grid_template_columns = Unicode(character(0),sync=TRUE),
    grid_template_areas = Unicode(character(0),sync=TRUE),
    grid_row = Unicode(character(0),sync=TRUE),
    grid_column = Unicode(character(0),sync=TRUE),
    grid_area = Unicode(character(0),sync=TRUE)
   ),
   active=list(
     border = function(value){
       if(missing(value)){
         found <- NULL
         for(side in c("top","right","bottom","left")){
           old <- found
           found <- self$state[[paste0("border_",side)]]
           if(!length(found)) return(NULL)
           if(found != old) return(NULL)
         }
         return(found)
       }
       for(side in c("top","right","bottom","left")){
         self$state[[paste0("border_",side)]] <- value
       }
     }
   )
)

#' @export
Layout <- function(...) LayoutClass$new(...)

# Local Variables:
# ess-indent-offset: 2
# End:

