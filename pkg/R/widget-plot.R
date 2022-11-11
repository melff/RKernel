#' Widgets to receive plots graphics
#' @description Classes and constructors show graphics created by code
#' @include widget-media.R
#' @name PlotWidget

#' @rdname PlotWidget
#' @export
PlotWidgetClass <- R6Class_(
    "PlotWidget",
    inherit = ImageWidgetClass,
    public = list(
        context = NULL,
        initialize = function(...,
                              envir = new.env()){
            super$initialize(...)
            context <- Context$new(envir=self$envir,
                                   attachment=list(
                                       display=self$display
                                   ))
            context$on_enter(self$enter)
            context$on_exit(self$exit)

            context$on_eval(self$handle_graphics)
            self$context <- context
            self$value <- empty20x20
            private$kernel <- get_current_kernel()
        },
        handle_graphics = function(){

            # plt <- self$context$get_graphics()
            plt <- recordPlot()
            if(!length(plt)) return(NULL)
            
            width      <- getOption("jupyter.plot.width",6)
            height     <- getOption("jupyter.plot.height",6)
            pointsize  <- getOption("jupyter.plot.pointsize",12)
            res        <- getOption("jupyter.plot.res",150)
            scale      <- getOption("jupyter.plot.scale",0.5)
            units      <- getOption("jupyter.plot.units","units")

            self$width <- width*res*scale
            self$height <- height*res*scale
            
            self$value <- mime_graphics(plt,
                                        mime="image/png",
                                        width=width,
                                        height=height,
                                        pointsize=pointsize,
                                        scale=scale,
                                        res=res,
                                        units=units)
        },
        enter = function(){
            parent <- private$kernel$get_parent("shell")
            self$msg_id <- parent$header$msg_id
            graphics$current$push(self$context$current_plot)
            em <- get_current_event_manager()
            for(event in c("before_print","print","before_cat","cat")){
                em$push_handlers(event)
            }
        },
        exit = function(){
            self$msg_id <- ""
            self$context$current_plot <- graphics$current$pop()
            em <- get_current_event_manager()
            for(event in c("before_print","print","before_cat","cat")){
                em$pop_handlers(event)
            }
        }
    ),
    private = list(
        kernel = NULL
    )
)

#' @rdname PlotWidget
#' @param ... Arguments, passed to the ImageWidget constructors.
#' @export
PlotWidget <- function(...) PlotWidgetClass$new(...)

#' @rdname PlotWidget
#' @param data An "PlotWidget" object
#' @param expr An expression to evaluate, or a sequence of expression, 
#'    encapsulated by curly braces.
#' @param enclos An enclosing environment.
#' @export
with.PlotWidget <- function(data,expr,enclos=parent.frame(),...)
    data$context$eval(substitute(expr),enclos=enclos)





#' Widgets to receive plots graphics
#' @description Classes and constructors show graphics created by code
#' @include widget-media.R
#' @name SVGWidget

#' @rdname SVGWidget
#' @importFrom svglite svgstring
#' @export
SVGWidgetClass <- R6Class_(
    "SVGWidget",
    inherit = HTMLClass,
    public = list(
        context = NULL,
        initialize = function(...,
                              use.recording = FALSE,
                              envir = new.env()){
            super$initialize(...)
            context <- Context$new(envir=self$envir,
                                   attachment=list(
                                       display=self$display
                                   ))
            context$on_enter(self$enter)
            context$on_exit(self$exit)

            context$on_eval(self$handle_graphics)
            self$context <- context
            private$kernel <- get_current_kernel()
            private$dev_num_other <- dev.cur()
            private$use.recording <- use.recording
            if(!private$use.recording){
                private$new_device()
                private$dev_num <- dev.cur()
                dev.set(private$dev_num_other)
            }
        },
        handle_graphics = function(){
            if(private$use.recording){
                plt <- recordPlot()
                if(!length(plt)) return(NULL)
                private$new_device()
                replayPlot(plt)
                dev.off()
            }
            string <- private$svg_string()
            if(length(string)>1){
                string <- string[length(string)]
            }
            self$value <- string
        },
        enter = function(){
            if(private$use.recording){
                graphics$current$push(self$context$current_plot)
            } else {
                private$dev_num_other <- dev.cur()
                dev.set(private$dev_num)
            }
        },
        exit = function(){
            if(private$use.recording){
                self$context$current_plot <- graphics$current$pop()
            } else {
                dev.set(private$dev_num_other)
            }
        }
    ),
    private = list(
        kernel = NULL,
        dev_num = 0,
        dev_num_other = 0,
        svg_string = NULL,
        new_device = function(){
            width <- getOption("jupyter.plot.width",6)
            height <- getOption("jupyter.plot.height",6)
            pointsize <- getOption("jupyter.plot.pointsize",12)
            scale <- getOption("jupyter.plot.scale",0.5)
            private$svg_string <- svgstring(width=width,height=height,pointsize=pointsize,
                                            standalone=FALSE)
        },
        use.recording = FALSE
    )
)

#' @rdname SVGWidget
#' @param ... Arguments, passed to the ImageWidget constructors.
#' @export
SVGWidget <- function(...) SVGWidgetClass$new(...)

#' @rdname SVGWidget
#' @param data An "SVGWidget" object
#' @param expr An expression to evaluate, or a sequence of expression, 
#'    encapsulated by curly braces.
#' @param enclos An enclosing environment.
#' @export
with.SVGWidget <- function(data,expr,enclos=parent.frame(),...)
    data$context$eval(substitute(expr),enclos=enclos)
