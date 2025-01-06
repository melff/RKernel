#' @import httpgd
#' @importFrom jsonlite fromJSON
#' @importFrom curl curl_fetch_memory
#' @importFrom jsonlite base64_enc
#' @import unigd

GraphicsObserver <- R6Class("GraphicsObserver",
  public = list(
    host = NULL,
    port = NULL,
    token = NULL,
    internal = FALSE,
    device = NULL,
    initialize = function(details, internal = FALSE) {
      self$internal <- internal
      if(internal) {
        self$device <- dev.cur()
      } else {
        self$host <- details$host
        self$port <- details$port
        self$token <- details$token
      }
      ur <- subset(ugd_renderers(),type=="plot")
      tikz <- which(ur$id == "tikz")
      ur$mime[tikz] <- "text/latex"
      self$formats <- ur$id
      self$mime_types <- ur$mime
      self$binary_formats <- !ur$text
    },
    id = 0,
    upid = 0,
    poll = function() {
      state <- self$get_current_state()
      c(active = state$active,
        id = state$id > self$id,
        upid = state$upid > self$upid)
    },
    store = function() {
      state <- self$get_current_state()
      self$id <- state$id
      self$upid <- state$upid
    },
    get_current_state = function() {
      if(self$internal) {
        self$get_current_state_internal()
      } else {
        self$get_current_state_http()
      }
    },
    get_current_state_internal = function() {
      state <- ugd_state(self$device)
      list(
        active = state$active,
        hsize = state$hsize,
        id = state$hsize,
        upid = state$upid
      )
    },
    get_current_state_http = function() {
      gurl <- paste0(
                "http://",
                self$host,":",
                self$port,"/",
                "plots","?",
                "token=",self$token)
      con <- url(gurl)
      plots_res <- readLines(con, warn = FALSE)
      close(con)
      plots_res <- fromJSON(plots_res)
      hsize <- plots_res$state$hsize
      if(hsize > 0) {
        id <- as.integer(plots_res$plots$id[hsize]) + 1L
      } else {
        id <- 0L
      }
      list(
        active = plots_res$state$active,
        hsize = hsize,
        id = id,
        upid = plots_res$state$upid
      )
    },
    formats = character(0),
    mime_types = character(0),
    binary_formats = logical(0),
    width = 7,
    height = 7,
    dpi = 72,
    render = function(format = "svgp", 
                      plot_id = integer(0),
                      width = getOption("jupyter.plot.width",self$width),
                      height = getOption("jupyter.plot.height",self$height),
                      resolution = getOption("jupyter.plot.resolution",self$dpi),
                      zoom = getOption("jupyter.plot.zoom",1)) {
      if(self$internal) {
        data <- self$render_internal(
                                format = format,
                                plot_id = plot_id,
                                width = width,
                                height = height,
                                resolution = resolution,
                                zoom = zoom)
      } else {
        gurl <- self$render_url(format = format,
                                plot_id = plot_id,
                                width = width,
                                height = height,
                                resolution = resolution,
                                zoom = zoom)
        data <- curl_fetch_memory(gurl)
      }
      ii <- match(format, self$formats)
      if(self$binary_formats[ii]) {
        data$content <- base64_enc(data$content)
      } else {
        data$content <- rawToChar_(data$content)
      }
      data$width <- width
      data$height <- height
      data$format <- format
      data$zoom <- zoom
      data$resolution <- resolution
      data
    },
    render_url = function(format = "svgp", 
                          plot_id = integer(0),
                          width = getOption("jupyter.plot.width",self$width),
                          height = getOption("jupyter.plot.height",self$height),
                          resolution = getOption("jupyter.plot.resolution",self$dpi),
                          zoom = getOption("jupyter.plot.zoom",1)) {
      if(format %in% c("png", "tiff", "png-base64")) {
        zoom <- resolution/self$dpi * zoom
        width  <- width * resolution
        height <- height * resolution
      }
      else {
        width  <- width * self$dpi
        height <- height * self$dpi
      }
      paste0(
        "http://",
        self$host,":",
        self$port,"/",
        "plot","?",
        if(length(plot_id)) paste0("id=",(plot_id - 1L),"&"),
        "token=", self$token,"&",
        "renderer=", format,"&",
        "width=", width,"&",
        "height=", height,"&",
        "zoom=",zoom)
    },
    render_internal = function(format = "svgp", 
                          plot_id = integer(0),
                          width = getOption("jupyter.plot.width",self$width),
                          height = getOption("jupyter.plot.height",self$height),
                          resolution = getOption("jupyter.plot.resolution",self$dpi),
                          zoom = getOption("jupyter.plot.zoom",1)) {
      if(format %in% c("png", "tiff", "png-base64")) {
        zoom <- resolution/self$dpi * zoom
        width  <- width * resolution
        height <- height * resolution
      }
      else {
        width  <- width * self$dpi
        height <- height * self$dpi
      }
      content <- ugd_render(
        page = 0,
        width = width,
        height = height,
        zoom = zoom,
        as = format,
        which = self$device
      )
      ii <- match(format,self$formats)
      type <- self$mime_types[ii]
      list(content = content,
           type = type)
    },
    display_res = 144,
    display_data = function(plot_id = integer(0),
                            display_id=UUIDgenerate(),
                            update=FALSE,
                            formats = getOption("jupyter.plot.formats",c("pdf","png","svgp")),
                            width = getOption("jupyter.plot.width",self$width),
                            height = getOption("jupyter.plot.height",self$height),
                            resolution = getOption("jupyter.plot.resolution",288),
                            zoom = getOption("jupyter.plot.zoom",1)) {
      renders <- lapply(formats,
                        self$render,
                        plot_id = plot_id,
                        width = width,
                        height = height,
                        resolution = resolution,
                        zoom = zoom)
      mime_data <- lapply(renders, "[[","content")
      mime_types <- lapply(renders, "[[","type")
      names(mime_data) <- mime_types
      mime_metadata <- lapply(renders, self$render_metadata, plot_id)
      names(mime_metadata) <- mime_types
      mime_data[["text/plain"]] <- self$render_url(
                                          plot_id = plot_id,
                                          width = width,
                                          height = height,
                                          resolution = resolution,
                                          zoom = zoom)
      d <- list(data = mime_data,
                metadata = mime_metadata)
      d$transient <- list(display_id=display_id)
      if(update) cl <- "update_display_data"
      else cl <- "display_data"
      self$store()
      structure(d,class=cl)
    },
    render_metadata = function(r, plot_id) {
      m <- list(plot_id = plot_id)
      if(r$format %in% c("png", "tiff", "png-base64")) {
        f <- self$dpi
        m$width <- r$width * f
        m$height <- r$height * f
      } else if(r$format %in% c("svg","svgp", "svgz", "svgzp")) {
        m$isolated <- TRUE
      }
      m
    },
    live_url = function() {
      if(self$internal) ""
      else paste0(
        "http://",
        self$host,":",
        self$port,"/",
        "live","?",
        "token=", self$token
      )
    }
  ))


#' @export
start_graphics <- function(){
    options(device=httpgd::hgd)
    add_sync_options(c(
          "jupyter.plot.width",
          "jupyter.plot.height",
          "jupyter.plot.res",
          "jupyter.plot.formats",
          "jupyter.update.graphics"))
}

dev_is_unigd <- function(which = dev.cur()) {
  names(which) == "unigd"
}

rawToChar_ <- function(x) {
  if(is.raw(x)) rawToChar(x)
  else if(is.character(x)) x[1]
  else ""
}