#' @export
LayoutClass <- R6Class("Layout",
   inherit = WidgetClass,
   public = list(
     `_view_name` = trait("LayoutView",sync=TRUE),
    `_view_module` = trait("@jupyter-widgets/controls",sync=TRUE),
    `_view_module_version` = trait(jupyter_widgets_controls_version,sync=TRUE),
     `_model_name` = trait("LayoutModel",sync=TRUE),
    align_content = trait(c("flex-start","flex-end","center","space-between",
                            "space-around","space-evenly","stretch"),
                          type="str_enum",optional=TRUE,sync=TRUE),
    align_items = trait(c("flex-start","flex-end","center","baseline","stretch"),
                        type="str_enum",optional=TRUE,sync=TRUE),
    align_self = trait(c("auto","flex-start","flex-end","center","baseline","stretch"),
                        type="str_enum",optional=TRUE,sync=TRUE),
    border_top = trait(character(0),optional=TRUE,sync=TRUE),
    border_right = trait(character(0),optional=TRUE,sync=TRUE),
    border_bottom = trait(character(0),optional=TRUE,sync=TRUE),
    border_left = trait(character(0),optional=TRUE,sync=TRUE),
    bottom = trait(character(0),optional=TRUE,sync=TRUE),
    display = trait(character(0),optional=TRUE,sync=TRUE),
    flex = trait(character(0),optional=TRUE,sync=TRUE),
    flex_flow = trait(character(0),optional=TRUE,sync=TRUE),
    height = trait(character(0),optional=TRUE,sync=TRUE),
    justify_content = trait(c("flex-start","flex-end","center","space-between",
                            "space-around"),optional=TRUE,sync=TRUE),
    justify_items = trait(c("flex-start","flex-end","center"),optional=TRUE,sync=TRUE),
    left = trait(character(0),optional=TRUE,sync=TRUE),
    margin = trait(character(0),optional=TRUE,sync=TRUE),
    max_height = trait(character(0),optional=TRUE,sync=TRUE),
    max_width = trait(character(0),optional=TRUE,sync=TRUE),
    min_height = trait(character(0),optional=TRUE,sync=TRUE),
    min_width = trait(character(0),optional=TRUE,sync=TRUE),
    overflow = trait(character(0),optional=TRUE,sync=TRUE),
    order = trait(character(0),optional=TRUE,sync=TRUE),
    padding = trait(character(0),optional=TRUE,sync=TRUE),
    right = trait(character(0),optional=TRUE,sync=TRUE),
    top = trait(character(0),optional=TRUE,sync=TRUE),
    visibility = trait(c("visible","hidden"),
                        type="str_enum",optional=TRUE,sync=TRUE),
    width = trait(character(0),optional=TRUE,sync=TRUE),
    object_fit = trait(c("contain","cover","fill","scale-down","none"),
                        type="str_enum",optional=TRUE,sync=TRUE),
    object_position = trait(character(0),optional=TRUE,sync=TRUE),
    grid_auto_columns = trait(character(0),optional=TRUE,sync=TRUE),
    grid_auto_flow = trait(c("column","row","row dense","column dense"),optional=TRUE,sync=TRUE),
    grid_auto_rows = trait(character(0),optional=TRUE,sync=TRUE),
    grid_gap = trait(character(0),optional=TRUE,sync=TRUE),
    grid_template_rows = trait(character(0),optional=TRUE,sync=TRUE),
    grid_template_columns = trait(character(0),optional=TRUE,sync=TRUE),
    grid_template_areas = trait(character(0),optional=TRUE,sync=TRUE),
    grid_row = trait(character(0),optional=TRUE,sync=TRUE),
    grid_column = trait(character(0),optional=TRUE,sync=TRUE),
    grid_area = trait(character(0),optional=TRUE,sync=TRUE)
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

