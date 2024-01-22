
#' @import httpgd
#' @export
GraphicsClass <- R6Class("Graphics",
    public = list(
        initialize = function(width = 7,
                              height = 7){
            private$width <- width
            private$height <- height
            stopifnot(width > 0)
            stopifnot(height > 0)
            hgd(width = width*self$dpi,
                height = height*self$dpi,
                silent = TRUE)
            private$renderers <- hgd_renderers()
            private$dev_num <- dev.cur() 
            private$mime <- private$renderers$mime
            private$formats <- private$renderers$id
            names(private$mime)  <- private$formats
            private$binary_format <- private$renderers$bin
            names(private$binary_format)  <- private$renderers$id
            state <- hgd_state()
            private$port <- state$port
            private$host <- state$host
            private$port <- state$port
            private$token <- state$token
            private$hsize <- state$hsize
            private$upid <- state$upid
        },
        active = function(){
            dev.cur() == private$dev_num  
        },
        activate = function(){
            dev.set(private$dev_num)
        },
        render = function(type,
                          mime,
                          width=-1,
                          height=-1,
                          dpi=-1){
            if(missing(type)){
                if(!missing(mime))
                    type <- self$mime2format(mime)
                else type <- "svgp"
            }
            if(!type %in% private$renderers$id) stop("Unsupported format")
            if(width < 0) width <- private$width
            else private$width <- width
            if(height < 0) height <- private$height
            else private$height <- height
            if(dpi < 0) dpi <- self$dpi
            if(type == "png")
               zoom <- dpi/self$dpi
            else zoom <- 1L
            width <- width * dpi
            height <- height * dpi
            dev.set(private$dev_num)
            data <- hgd_plot(width    = width,
                             height   = height,
                             zoom     = zoom,
                             renderer = type,
                             which    = private$dev_num)
            mime <- private$mime[type]
            binary <- private$binary_format[type]
            state <- hgd_state()
            private$hsize <- state$hsize
            private$upid <- state$upid
            # return(list(
            #     data = data,
            #     mime = unname(mime),
            #     binary = unname(binary),
            #     type = type
            # ))
            return(data)
        },
        url = function(type,
                       mime,
                       width=-1,
                       height=-1,
                       dpi=-1){
            if(missing(type)){
                if(!missing(mime))
                    type <- self$mime2format(mime)
                else type <- "svgp"
            }
            if(!type %in% private$renderers$id) stop("Unsupported format")
            if(width < 0) width <- private$width
            else private$width <- width
            if(height < 0) height <- private$height
            else private$height <- height
            if(dpi < 0) dpi <- self$dpi
            if(type == "png")
               zoom <- dpi/self$dpi
            else zoom <- 1L
            width <- as.integer(width * dpi)
            height <- as.integer(height * dpi)
            host <- private$host
            port <- private$port
            state <- hgd_state()
            private$hsize <- state$hsize
            private$upid <- state$upid
            renderer <- type
            token <- private$token
            index <- private$hsize - 1L
            if(zoom != 1){
                url <- sprintf("http://%s:%d/plot?index=%d&width=%d&height=%d&renderer=%s&zoom=%f&token=%s",
                               host,port,index,width,height,renderer,zoom,token)
            }
            else {
                url <- sprintf("http://%s:%d/plot?index=%d&width=%d&height=%d&renderer=%s&token=%s",
                               host,port,index,width,height,renderer,token)
            }
            url
        },
        get_dims = function(){
            c(width  = private$width,
              height = private$height)
        },
        set_dims = function(width = -1, height = -1){
            if(width < 0) width <- private$width
            else private$width <- width
            if(height < 0) height <- private$height
            else private$height <- height
            dev.set(private$dev_num)
            hgd_plot(renderer="meta", width = width, height = height)
        },
        mime2format = function(mime){
            mime <- mime[1]
            if(mime %in% private$formats) return(mime)
            else {
                ii <- which(private$mime == mime)
                format <- private$formats[min(ii)]
                return(format)
            }
        },
        format2mime = function(format){
            format <- format[1]
            if(format %in% private$mime) return(format)
            else {
                mime <- unique(private$mime[format])
                return(mime)
            }
        },
        close = function(){
            dev.off(private$dev_num)
        },
        get_hsize = function(){
            private$hsize
        },
        get_upid = function(){
            private$upid
        },
        live_url = function(){
            dev.set(private$dev_num)
            hgd_url()
        },
        dpi = 72 # from pdf()
    ),
    private = list(
        plt = NULL,
        renderers = NULL,
        dev_num = 0,
        mime = "",
        formats = "",
        binary_format = logical(0),
        host = "",
        port = 0,
        token = NULL,
        width = -1,
        height = -1,
        hsize = 0,
        upid = 0
    )
)

#' @export
Graphics <- function(...)GraphicsClass$new(...)

#' @export
print.Graphics <- function(x,...) display(x)

#' @include display.R
#' @importFrom uuid UUIDgenerate
#' @export
display_data.Graphics <- function(x,
                                  width=getOption("jupyter.plot.width",7),
                                  height=getOption("jupyter.plot.height",7),
                                  resolution=getOption("jupyter.plot.res",144),
                                  id=UUIDgenerate(),
                                  update=FALSE,
                                  ...){
    rkernel_graphics_types <- getOption("jupyter.graphics.types",
                                        c("image/svg+xml",
                                          "image/png","application/pdf"))
    
    formats <- sapply(rkernel_graphics_types,x$mime2format)
    mime_types <- sapply(rkernel_graphics_types,x$format2mime)
    
    mime_data <- list()
    mime_metadata <- list()

    for(i in seq_along(formats)){
        mime_i <- mime_types[i]
        format_i <- formats[i]
        mime_data_i <- x$render(type=format_i)
        mime_data[[mime_i]] <- mime_data_i
        width_i <- width * x$dpi
        height_i <- height * x$dpi
        mime_metadata[[mime_i]] <- list(
            width = width_i,
            height = height_i
        )
        if(mime_i == "image/svg+xml")
            mime_metadata[[mime_i]]$isolated <- TRUE
    }
    
    d <- list(data = mime_data,
              metadata = mime_metadata)
              transient = list(display_id = id)
    if(update) cl <- "update_display_data"
    else cl <- "display_data"
    structure(d,class=cl)
}

empty20x20 <- local({
    imf <- system.file("images/empty20x20.png",package="RKernel")
    readBin(imf, raw(), file.info(imf)$size)
})
