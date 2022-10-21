#' @importFrom grDevices png jpeg svg pdf
mime_graphics <- function(plt,mime,width,height,pointsize,scale,res,units="in"){
    grapics_data <- NULL
    if(mime=="image/png"){
        tf <- tempfile(fileext="png")
        png(file=tf,
            width=width,
            height=height,
            units=units,
            res=res/scale)
        replayPlot(plt)
        dev.off()
        graphics_data <- readBin(tf, raw(), file.info(tf)$size)
    }
    else if(mime=="image/jpeg"){
        tf <- tempfile(fileext="jpeg")
        jpeg(file=tf,
            width=width,
            height=height,
            units=units,
            res=res/scale)
        replayPlot(plt)
        dev.off()
        graphics_data <- readBin(tf, raw(), file.info(tf)$size)
    }
    else if(mime=="image/svg+xml"){
        tf <- tempfile(fileext="svg")
        svg(file=tf,
            width=width,
            height=height)
        replayPlot(plt)
        dev.off()
        graphics_data <- readChar(tf, file.info(tf)$size,useBytes=TRUE)
    }
    else if(mime=="application/pdf"){
        tf <- tempfile(fileext="pdf")
        pdf(file=tf,
            width=width,
            height=height)
        replayPlot(plt)
        dev.off()
        graphics_data <- readBin(tf, raw(), file.info(tf)$size)
    }
    return(graphics_data)
}
