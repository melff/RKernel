#' @importFrom base64enc dataURI
#' @import httpgd
#' @export
GraphicsClass <- R6Class("Graphics",
    public = list(
        initialize = function(...) self$open(...),
        open = function(width = 7,
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
            # log_out(sprintf("Opened graphics at port %d",private$port))
        },
        close = function(){
            hgd_close(private$dev_num)
            # log_out(sprintf("Closed graphics at port %d",private$port))
        },
        active = function(){
            dev.cur() == private$dev_num  
        },
        activate = function(){
            dev.set(private$dev_num)
        },
        checkpoint = function(){
            if(self$active()){
                state <- hgd_state()
                private$hsize <- state$hsize
                private$upid <- state$upid
            }
        },
        new_cell = TRUE,
        new_page_called = FALSE,
        new_page = function(){
            state <- hgd_state()
            state$hsize > private$hsize
        },
        incomplete_layout = FALSE,
        updated = function(){
            state <- hgd_state()
            state$upid > private$upid
        },
        size = function(){
            state <- hgd_state()
            state$hsize
        },
        render = function(type,
                          mime,
                          page=0,
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
            width <- width * self$dpi
            height <- height * self$dpi
            if(type == "png"){
               zoom <- dpi/self$dpi
               width <- width * zoom
               height <- height * zoom
            }
            else zoom <- 1L
            dev.set(private$dev_num)
            data <- hgd_plot(page     = page,
                             width    = width,
                             height   = height,
                             zoom     = zoom,
                             renderer = type,
                             which    = private$dev_num)
            mime <- private$mime[type]
            binary <- private$binary_format[type]
            # log_out(sprintf("Rendered graphics at port %d",private$port))
            return(data)
        },
        svg = function(page = 0){
            self$render("svgp", page = page)
        },
        png = function(page = 0){
            self$render("png", page = page)
        },
        last_plot = function(){
            state <- hgd_state()
            state$hsize
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
            dpi <- self$dpi
            hgd_plot(renderer="meta", 
                     width = width * dpi, 
                     height = height * dpi)
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

graphics <- new.env()

#' @export
start_graphics <- function(){
    graphics$current <- Graphics()
    add_output_hook(send_changed_graphics,"graphics")
    setHook('cell-end',send_changed_graphics)
    setHook('cell-begin',graphics_new_cell)
    setHook('before.plot.new',graphics_apply_changed_dims)
    setHook('before.plot.new',send_changed_graphics)
    setHook('before.grid.newpage',graphics_apply_changed_dims)
    setHook('before.grid.newpage',send_changed_graphics)
    setHook('plot.new',send_new_plot)
    setHook('grid.newpage',send_new_plot)
    graphics$delivery_mode <- "display"
    graphics$last_display <- ""
}

#' @importFrom uuid UUIDgenerate
#' @export
send_changed_graphics <- function(...){
    # log_out("======= send_changed_graphics ================")
    g <- graphics$current
    dm <- graphics$delivery_mode
    use_update <- getOption("jupyter.update.graphics",TRUE)
    new_display <- FALSE
    update_display <- FALSE
    if(g$active()){
        if(dm == "display"){
            if(use_update){
                if(g$new_page()){
                    if(g$incomplete_layout)
                        update_display <- TRUE
                    else
                        new_display <- TRUE
                    g$incomplete_layout <- !par("page")
                }
                else if(g$updated()){
                    update_display <- TRUE
                    g$incomplete_layout <- FALSE
                }
            }
            else {
                if(g$new_cell){
                    if(g$new_page() || g$updated())
                        new_display <- TRUE
                }
                else {
                    if(g$new_page()){
                        if(g$incomplete_layout)
                            update_display <- TRUE
                        else
                            new_display <- TRUE
                        g$incomplete_layout <- !par("page")
                    }
                    else if(g$updated()){
                        update_display <- TRUE
                        g$incomplete_layout <- FALSE
                    }
                }
            }
            
            if(new_display){
                id <- UUIDgenerate()
                graphics$last_display <- id
                display(g,id=id)
            }
            else if(update_display){
                id <- graphics$last_display
                display(g,id=id,update=TRUE)
            }
            if(new_display || update_display) {
                g$new_cell <- FALSE
                g$new_page_called <- FALSE
            }
        }
        g$checkpoint()
    }
}

send_new_plot <- function(...){
    g <- graphics$current
    g$new_page_called <- TRUE
    dm <- graphics$delivery_mode
    if(g$active() && dm == "id" && par("page")){
            cat_(send_graphics_id(g))
    }
}

graphics_new_cell <- function(){
    g <- graphics$current
    if(g$active()){
        g$new_cell <- TRUE
    }    
}

graphics_apply_changed_dims <- function(){
    g <- graphics$current
    if(g$active()){
        if(any_option_changed("jupyter.plot.width","jupyter.plot.height")){
            cur_dim <- g$get_dims()
            g$set_dims(width=getOption("jupyter.plot.width",unname(cur_dim["width"])),
                       height=getOption("jupyter.plot.height",unname(cur_dim["height"])))
            save_options("jupyter.plot.width","jupyter.plot.height")
        }
    }
}

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
        mime_data_i <- x$render(type=format_i,
                                dpi=resolution)
        mime_data[[mime_i]] <- mime_data_i
        width_i <- width * x$dpi
        height_i <- height * x$dpi
        mime_metadata[[mime_i]] <- list(
            width = width_i,
            height = height_i
        )
        #if(mime_i == "image/svg+xml") # No longer necessary(?)
        #    mime_metadata[[mime_i]]$isolated <- TRUE
    }

    d <- list(data = mime_data,
              metadata = mime_metadata,
              transient = list(display_id = id))
    if(update) cl <- "update_display_data"
    else cl <- "display_data"
    structure(d,class=cl)
}

empty20x20 <- local({
    imf <- system.file("images/empty20x20.png",package="RKernel")
    readBin(imf, raw(), file.info(imf)$size)
})

HGD_ID  <- "[!hgd_id]"

send_graphics_id <- function(g){
    id <- g$last_plot()
    if(id > 0){
        cat_(DLE)
        cat_(HGD_ID)
        cat_(id)
        cat_(DLE)
    }
}

#'@export
graphics_url <- function(){
    g <- graphics$current
    g$live_url()
}

#'@export
graphics_frame <- function(width = 600L,
                           height = 500L,
                           class = "rkernel-iframe",
                           style = "border-style:none",...){
    url <- graphics_url()
    if_tmpl <- "
                   <iframe src=\"%s\" width=\"%s\" height=\"%s\" class=\"%s\" style=\"%s\">
                   </iframe>
                   "
    iframe <- sprintf(if_tmpl,url,width,height,class,style)
    id <- UUIDgenerate()
    structure(iframe,class="iframe",id=id)
}
