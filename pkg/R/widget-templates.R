#' Widget Layout Templates
#' @description R6 classes and constructor functions for widget layout templates
#' @include widget-box.R
#' @name LayoutTemplates
NULL

#' @rdname LayoutTemplates
#' @export
TemplateBaseClass <- R6Class_("TemplateBase",
    inherit = GridBoxClass,
    public = list(
        #' @field grid_gap The grid-gap CSS attribute
        grid_gap = Unicode(optional=TRUE),
        #' @field justify_content The justify-content CSS attribute
        justify_content = StrEnum(c("flex-start","flex-end","center",
                                    "space-between","space-around"),optional=TRUE),
        #' @field align_items The align-items CSS attribute
        align_items = StrEnum(c("top","bottom",
                                "flex-start","flex-end","center",
                                "baseline","scretch"),optional=TRUE),
        #' @field width The width CSS attribute
        width = Unicode(optional=TRUE),
        #' @field height The height CSS attribute
        height = Unicode(optional=TRUE),
        #' @description Initializer
        #' @param ... Arguments, used to initialize the fields
        initialize = function(...){
            super$initialize(...)
            private$copy_layout_props()
            private$set_observers()
        }
    ),
    private = list(
        properties = c("grid_gap","justify_content","align_items","width","height"),
        property_mapping = NULL,
        property_rewrite = function(prop,value){
            if(!prop %in% names(private$property_mapping)) return(value)
            else {
                map <- property_mapping[prop]
                n <- intersect(names(map),value)
                i <- match(value,n)
                value[i] <- map[n]
                return(value)
            }
        },
        copy_layout_props = function(){
            for(prop in private$properties)
                private$sync_layout_prop(prop)
        },
        sync_layout_prop = function(prop){
            value <- self[[prop]]
            value <- private$property_rewrite(prop,value)
            if(length(value))
                self$layout[[prop]] <- value
        },
        set_observers = function(){
            for(prop in private$properties){
                self$observe(prop,function(prop,self,value){
                    private$sync_layout_prop(prop)
                })
            }
        }
    )
)

#' @rdname LayoutTemplates
#' @export
AppLayoutClass <- R6Class_("AppLayout",
    inherit = TemplateBaseClass,
    public = list(
        #' @field header Widget that appears in the header section
        header = Vector(),
        #' @field footer Widget that appears in the footer section
        footer = Vector(),
        #' @field left_sidebar Widget that appears as left sidebar
        left_sidebar = Vector(),
        #' @field right_sidebar Widget that appears as right sidebar
        right_sidebar = Vector(),
        #' @field center Widget that appears in the center section
        center = Vector(),
        #' @field pane_widths Unicode string with CSS widths for the app panes
        pane_widths = Unicode(c("1fr","2fr","1fr")),
        #' @field pane_heights Unicode string with CSS heights for the app panes
        pane_heights = Unicode(c("1fr","3fr","1fr")),
        #' @field merge Boolean, whether space of missing widgets chould be merged
        #     with adjacent sections
        merge = Boolean(TRUE),
        #' @description Initializer method
        #' @param ... Arguments, passed on to the superclass initializer
        initialize = function(...){
            super$initialize(...)
            private$update_layout()
            properties <- c("footer","header","center",
                            "left_sidebar","right_sidebar",
                            "merge","pane_widths","pane_heights")
            for(prop in properties)
                self$observe(prop,private$update_layout)
        }
    ),
    private = list(
        update_layout = function(...){
            grid_template_areas <- matrix(c(
                "header","header","header",
                "left-sidebar","center","right-sidebar",
                "footer","footer","footer"),
                ncol=3,byrow=TRUE
            )
            grid_template_columns <- private$convert_sizes(self$pane_widths)
            grid_template_rows <- private$convert_sizes(self$pane_heights)
            all_children <- list(
                header = self$header,
                footer = self$footer,
                'left-sidebar' = self$left_sidebar,
                'right-sidebar' = self$right_sidebar,
                center = self$center
            )
            l <- sapply(all_children,length)
            children <- all_children[l>0]
            if(!length(children)) return()
            for(position in names(children)){
                child <- children[[position]]
                child$layout$grid_area <- position
            }
            if(self$merge){
                if(length(children) == 1){
                    position <- names(children)[1]
                    grid_template_areas[] <- position
                } else {
                    if(!length(self$center)){
                        grid_template_areas <- grid_template_areas[,-2]
                        grid_template_columns <- grid_template_columns[-2]
                    }
                    if(!length(self$left_sidebar)){
                        grid_template_areas[2,1] <- grid_template_areas[2,2]
                    }
                    if(!length(self$right_sidebar)){
                        n <- ncol(grid_template_areas)
                        grid_template_areas[2,n] <- grid_template_areas[2,n-1]
                    }
                    if(!length(self$left_sidebar) &&
                       !length(self$right_sidebar) &&
                       !length(self$center)){
                        grid_template_areas <- as.matrix(c("header","footer"))
                        grid_template_columns <- "1fr"
                        grid_template_rows <- c("1fr","1fr")
                    }
                    if(!length(self$header)){
                        grid_template_areas <- grid_template_areas[-1,,drop=FALSE]
                        grid_template_rows <- grid_template_rows[-1]
                    }
                    if(!length(self$footer)){
                        n <- nrow(grid_template_areas)
                        grid_template_areas <- grid_template_areas[-n,,drop=FALSE]
                        grid_template_rows <- grid_template_rows[-n]
                   }
                }
            }
            grid_template_areas_css <- paste(
                paste0("\"",apply(grid_template_areas,1,paste,collapse=" "),"\""),
                collapse="\n"
            )
            self$layout$grid_template_columns <- paste(grid_template_columns,collapse=" ")
            self$layout$grid_template_rows <- paste(grid_template_rows,collapse=" ")
            self$layout$grid_template_areas <- grid_template_areas_css
            self$children <- unname(children)
        },
        size_to_css = function(size){
            if(grepl("\\d+\\.?\\d*(px|fr|%)",size))return(size)
            if(grepl("\\d+\\.?\\d*$",size))return(paste0(size,"fr"))
            stop(paste("the pane sizes must be in one of the following formats:",
                       "'10px', '10fr', 10 (will be converted to '10fr').",
                       sprintf("Got '%s'",size),sep="\n"))
        },
        convert_sizes = function(sizes){
            sapply(sizes,private$size_to_css)
        }
    )
)

#' @export
AppLayout <- function(...)AppLayoutClass$new(...)

