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
        #' @param ... Arguments used to initialize the fields
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

#' @rdname LayoutTemplates
#' @param ... Arguments used to initialize the fields
#' @export
AppLayout <- function(...)AppLayoutClass$new(...)



#' @rdname LayoutTemplates
#' @export
GridspecLayoutClass <- R6Class_("GridspecLayout",
    inherit = TemplateBaseClass,
    public = list(
        #' @description Initializer function
        #' @param nrow A positive integer, the number of rows
        #' @param ncol A positive integer, the number of columns
        #' @param ... Other arguments, passed to the superclass initializer
        initialize = function(nrow,ncol,...){
            if(nrow < 1 || ncol < 1) stop("'nrow' and 'ncol' must be positive integers")
            nrow <- as.integer(nrow)
            ncol <- as.integer(ncol)
            super$initialize(...)
            private$nrow <- nrow
            private$ncol <- ncol
            private$grid_template_areas <- matrix(".",nrow=nrow,ncol=ncol)
            private$grid_template_rows <- sprintf('repeat(%d, 1fr)',nrow)
            private$grid_template_columns <- sprintf('repeat(%d, 1fr)',ncol)
        },
        #' @description Set widget in grid cells
        #' @param i The rows into which the widget is to be placed
        #' @param j The columns into which the widget is to be placed
        #' @param value A widget
        set_item = function(i,j,value){
            if(!inherits(value,"DOMWidget")) stop("'value' must be a DOM widget.")
            old_ids <- unique(private$grid_template_areas[i,j])
            old_ids <- setdiff(old_ids,".")
            if(length(old_ids)){
                to_remove <- private$grid_children[old_ids]
                for(w in to_remove)
                    w$layout$grid_area <- character(0)
                private$grid_children[old_ids] <- NULL
            }
            private$counter <- private$counter + 1L
            obj_id <- sprintf('widget%03d',private$counter)
            value$layout$grid_area <- obj_id
            private$grid_children[[obj_id]] <- value
            private$grid_template_areas[i,j] <- obj_id
            private$update_layout()
        },
        #' @description Get widget from grid cells
        #' @param i The rows where the widget is located
        #' @param j The columns where the widget is located
        get_item = function(i,j){
            obj_id <- private$grid_template_areas[i,j]
            private$grid_children[obj_id]
        }
    ),
    private = list(
        nrow = 0L,
        ncol = 0L,
        grid_template_areas = character(0),
        grid_template_rows = character(0),
        grid_template_columns = character(0),
        grid_children = list(),
        counter = 0L,
        update_layout = function(){
            grid_template_areas_css <- paste(
                paste0("\"",apply(private$grid_template_areas,1,paste,collapse=" "),"\""),
                collapse="\n"
            )
            self$layout$grid_template_columns <- private$grid_template_columns
            self$layout$grid_template_rows <- private$grid_template_rows
            self$layout$grid_template_areas <- grid_template_areas_css
            self$children <- unname(private$grid_children)
        }
    )
)

#' @rdname LayoutTemplates
#' @param ... Arguments used to initialize the fields
#' @export
GridspecLayout <- function(...)GridspecLayoutClass$new(...)

#' @rdname LayoutTemplates
#' @param ... Other arguments, ignored
#' @param x A GridspecLayout object
#' @param i Integer value(s) referring to the row(s)
#' @param j Integer value(s) referring to the column(s)
#' @param drop Logical, whether the result is a widget or a list with one element if both i an j select a single element
#' @export
"[.GridspecLayout" <- function(x,i,j,...,drop=TRUE){
    res <- x$get_item(i,j)
    if(length(res) == 1 && drop)
        return(res[[1]])
    else return(res)
}

#' @rdname LayoutTemplates
#' @param value One or more widgets put at the idicated positions in the grid
#' @export
"[<-.GridspecLayout" <- function(x,i,j,value){
    x$set_item(i,j,value)
    return(x)
}



#' @rdname LayoutTemplates
#' @export
TwoByTwoLayoutClass <- R6Class_("TwoByTwoLayout",
    inherit = TemplateBaseClass,
    public = list(
        #' @field top_left Widget that appears on the top left
        top_left = Vector(),
        #' @field top_right Widget that appears on the top right
        top_right = Vector(),
        #' @field bottom_left Widget that appears on the bottom left
        bottom_left = Vector(),
        #' @field bottom_right Widget that appears on the bottom right
        bottom_right = Vector(),
        #' @field merge Boolean, whether space of missing widgets chould be merged
        #     with adjacent sections
        merge = Boolean(TRUE),
        #' @description Initializer method
        #' @param ... Arguments, passed on to the superclass initializer
        initialize = function(...){
            super$initialize(...)
            private$update_layout()
            properties <- c("top_left","top_right",
                            "bottom_left","bottom_right",
                            "merge")
            for(prop in properties)
                self$observe(prop,private$update_layout)
        }
    ),
    private = list(
        update_layout = function(...){
            grid_template_areas <- matrix(c(
                "top-left","top-right",
                "bottom-left","bottom-right"),
                ncol=2,byrow=TRUE
            )
            all_children <- list(
                'top-left' = self$top_left,
                'top-right' = self$top_right,
                'bottom-left' = self$bottom_left,
                'bottom-right' = self$bottom_right
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
                    columns <- c("left","right")
                    for(i in 1:2){
                        top <- children[[paste0("top-",columns[i])]]
                        bottom <- children[[paste0("bottom-",columns[i])]]
                        i_neighbour <- (i %% 2) + 1
                        if(!length(top) && !length(bottom)){
                            # merge cells in column with neighbours in same row
                            grid_template_areas[1,i] <- grid_template_areas[1,i_neighbour]
                            grid_template_areas[2,i] <- grid_template_areas[2,i_neighbour]
                        } else if(!length(top)){
                            # merge with cell below
                            grid_template_areas[1,i] <- grid_template_areas[2,i]
                        } else if(!length(bottom)){
                            # merge with cell above
                            grid_template_areas[2,i] <- grid_template_areas[1,i]
                        }
                    }
                }
            }
            grid_template_areas_css <- paste(
                paste0("\"",apply(grid_template_areas,1,paste,collapse=" "),"\""),
                collapse="\n"
            )
            self$layout$grid_template_columns <- "1fr 1fr"
            self$layout$grid_template_rows <- "1fr 1fr"
            self$layout$grid_template_areas <- grid_template_areas_css
            self$children <- unname(children)
        }
    )
)

#' @rdname LayoutTemplates
#' @param ... Arguments used to initialize the fields
#' @export
TwoByTwoLayout <- function(...)TwoByTwoLayoutClass$new(...)

