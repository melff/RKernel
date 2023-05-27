#' @importFrom grDevices png jpeg svg pdf replayPlot dev.off
mime_graphics <- function(plt,mime,width,height,pointsize,scale,res,units="in"){
    grapics_data <- NULL
    if(mime=="image/png"){
        tf <- tempfile(fileext=".png")
        png(filename=tf,
            width=width,
            height=height,
            units=units,
            res=res/scale)
        replayPlot(plt)
        dev.off()
        graphics_data <- readBin(tf, raw(), file.info(tf)$size)
    }
    else if(mime=="image/jpeg"){
        tf <- tempfile(fileext=".jpeg")
        jpeg(filename=tf,
            width=width,
            height=height,
            units=units,
            res=res/scale)
        replayPlot(plt)
        dev.off()
        graphics_data <- readBin(tf, raw(), file.info(tf)$size)
    }
    else if(mime=="image/svg+xml"){
        tf <- tempfile(fileext=".svg")
        svg(filename=tf,
            width=width,
            height=height)
        replayPlot(plt)
        dev.off()
        graphics_data <- readChar(tf, file.info(tf)$size,useBytes=TRUE)
    }
    else if(mime=="application/pdf"){
        tf <- tempfile(fileext=".pdf")
        pdf(file=tf,
            width=width,
            height=height)
        replayPlot(plt)
        dev.off()
        graphics_data <- readBin(tf, raw(), file.info(tf)$size)
    }
    return(graphics_data)
}

plot_new_hook <- function(...){
    if(inherits(eventmanagers$graphics,"EventManager"))
        eventmanagers$graphics$send("plot_new",...)
}
before_plot_new_hook <- function(...){
    if(inherits(eventmanagers$graphics,"EventManager"))
        eventmanagers$graphics$send("before_plot_new",...)
}


GraphicsDevice <- R6Class("GraphicsDevice",
    public = list(
        initialize = function(){
            os <- .Platform$OS.type
            sysname <- Sys.info()[["sysname"]]
            if(os == "unix" && sysname=="Darwin")
                os <- "osx"
            private$dev_filename <- switch(os,
                                           windows="NUL",
                                           osx=NULL,
                                           unix="/dev/null")
            private$dev_name <- switch(os,
                                       windows="png",
                                       osx="pdf",
                                       unix="png")
            if(!length(installed_hooks$graphics)){
                setHook('plot.new',plot_new_hook)
                setHook('grid.newpage',plot_new_hook)
                setHook('before.plot.new',before_plot_new_hook)
                setHook('before.grid.newpage',before_plot_new_hook)
                installed_hooks$graphics <- TRUE
            }
            em <- EventManager(type="graphics")
            em$activate()
            private$event_manager <- em
            em$on("plot_new",private$plot_new_hook)
            private$device()
            private$empty_plot <- recordPlot()
        },
        is_active = function(){
            private$dev_num == dev.cur()
        },
        create = function(){
            private$device()
        },
        activate = function(){
            if(private$dev_num != dev.cur()){
                private$other_dev = dev.cur()
                dev.set(private$dev_num)
            }
            private$event_manager$activate()
            
        },
        suspend = function(){
            private$event_manager$suspend()
            if(private$other_dev > 0)
                dev.set(private$other_dev)
        },
        get_plot = function(){
            plt <- recordPlot()
            return(plt)
        },
        new_page = function(reset=FALSE){
            result <- isTRUE(private$plot_new_called)
            if(reset)
                private$plot_new_called <- FALSE
            return(result)
        },
        complete_page = function(){
            self$is_active() && par("page")
        },
        empty = function(){
            self$empty_plot
        },
        clear = function(){
            replayPlot(self$empty_plot)
        },
        on_new_page = function(handler,replace=FALSE,before=FALSE){
            em <- private$event_manager
            if(before){
                em$on("before_plot_new",handler,replace)
            }
            else {
                em$on("plot_new",handler,replace)
            }
        }
    ),
    private = list(

        evaluator = NULL,
        event_manager = NULL,
        plot_new_called = FALSE,
        graphics_par_usr = numeric(0),

        plot_new_hook = function(...){
            # log_out("plot_new_hook")
            # log_out("private$dev_num==",private$dev_num)
            # log_out("dev.cur()==",dev.cur())
            if(self$is_active()){
                private$plot_new_called <- TRUE
                private$graphics_par_usr <- par("usr")
            } #else log_out("graphics not active ...")
        },

        dev_filename = character(0),
        dev_name = character(),
        dev_num = 0,
        other_dev = 0,
        device = function(filename = NULL,
                          width = getOption("jupyter.plot.width",6),
                          height = getOption("jupyter.plot.height",6),
                          res = getOption("jupyter.plot.res",96),
                          pointsize = getOption("jupyter.plot.pointsize",12),
                          units = getOption("jupyter.plot.units","in"),
                          ...){
            dev <- get(private$dev_name)
            if(is.null(filename))
                dev(filename=private$dev_filename,
                    width=width,
                    height=height,
                    res=res,
                    units=units,
                    ...)
            private$dev_num <- dev.cur()
            dev.control(displaylist="enable")
            
            # log_out('private$device')
            # log_out("private$dev_num==",private$dev_num)

        },
        empty_plot = NULL
    )
)


empty20x20 <- local({
    imf <- system.file("images/empty20x20.png",package="RKernel")
    readBin(imf, raw(), file.info(imf)$size)
})
