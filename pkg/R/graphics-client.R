#' @importFrom jsonlite fromJSON
#' @importFrom curl curl_fetch_memory
#' @importFrom jsonlite base64_enc
#' @import unigd

GraphicsClient <- R6Class("GraphicsClient",
  public = list(
    host = NULL,
    port = NULL,
    token = NULL,
    internal = FALSE,
    device = NULL,
    session = NULL,
    interface = NULL,
    initialize = function(interface = NULL, internal = FALSE) {
      self$internal <- internal
      if(internal) {
        self$device <- dev.cur()
      } else {
        # log_out("graphics$initialize()")
        self$interface <- interface
        session <- interface$session
        self$session <- session
        # details <- session$graphics_details()
        # log_print(details)
        self$host <- "localhost"
        self$port <- session$http_port
      }
      ur <- subset(ugd_renderers(),type=="plot")
      tikz <- which(ur$id == "tikz")
      ur$mime[tikz] <- "text/latex"
      self$formats <- ur$id
      self$mime_types <- ur$mime
      self$binary_formats <- !ur$text
    },
    restart = function() {
      if(self$internal) {
        ugd()
        self$device <- dev.cur()
      } else {
        # log_out("graphics$restart()")
        self$session$send_receive("ugd()")
      }
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
    active_id = function() {
      state <- self$get_current_state()
      state$id
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
                "graphics/",
                "state")
      con <- url(gurl)
      resp <- readLines(con, warn = FALSE)
      close(con)
      resp <- fromJSON(resp)
      state <- resp$state
      list(
        active = state$active,
        hsize = state$hsize,
        id = state$hsize,
        upid = state$upid
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
        data <- self$render_http(
                                format = format,
                                plot_id = plot_id,
                                width = width,
                                height = height,
                                resolution = resolution,
                                zoom = zoom)
      } 
      ii <- match(format, self$formats)
      if(is.raw(data$content)) {
        if(self$binary_formats[ii]) {
          data$content <- base64_enc(data$content)
        } else {
          data$content <- rawToChar_(data$content)
        }
      }
      data$width <- width
      data$height <- height
      data$format <- format
      data$zoom <- zoom
      data$resolution <- resolution
      data
    },

    render_http = function(format = "svgp", 
                          plot_id = integer(0),
                          width = getOption("jupyter.plot.width",self$width),
                          height = getOption("jupyter.plot.height",self$height),
                          resolution = getOption("jupyter.plot.resolution",self$dpi),
                          zoom = getOption("jupyter.plot.zoom",1)) {
        gurl <- self$get_render_url(format = format,
                                plot_id = plot_id,
                                width = width,
                                height = height,
                                resolution = resolution,
                                zoom = zoom)
        con <- url(gurl)
        resp <- tryCatch(curl_fetch_memory(gurl),
                         error = function(e) invokeRestart("continue"),
                         interrupt = function(e) invokeRestart("continue"))
        list(
          content = resp$content,
          type = resp$type
        )
    },
    get_render_url = function(format = "svgp", 
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
      plot_id <- as.integer(plot_id)
      paste0(
        "http://",
        self$host,":",
        self$port,"/",
        "graphics/",
        "plot","?",
        if(length(plot_id)) paste0("id=",(plot_id - 1L),"&"),
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
    display_obs = list(),
    current_display = character(0),
    new_display = function(plot_id,
                           width = getOption("jupyter.plot.width",self$width),
                           height = getOption("jupyter.plot.height",self$height),
                           resolution = getOption("jupyter.plot.resolution",288),
                           zoom = getOption("jupyter.plot.zoom",1)) {
          # log_out("++ new_display")
          display_id <- UUIDgenerate()
          gd <- GraphicsDisplay$new(
            plot_id = plot_id,
            width = width,
            height = height,
            resolution = resolution,
            zoom = zoom,
            manager = self,
            display_id = display_id
          )
          self$display_obs[[display_id]] <- gd
          self$current_display <- display_id
          gurl <- self$get_render_url(format = "svgp",
                                plot_id = plot_id,
                                width = width,
                                height = height,
                                resolution = resolution,
                                zoom = zoom)
          mime_data <- list("text/plain" = gurl)
          svg_tmpl <- "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\"></svg>"
          mime_data[["image/svg+xml"]] <- sprintf(svg_tmpl, width*self$dpi, height*self$dpi)
          d <- list(data = mime_data,
                    metadata = emptyNamedList,
                    transient = list(display_id = display_id))
          class(d) <- "display_data"
          self$interface$display_send(d)
    },
    display_res = 144,
    render_display = function(display_id,
                            update = TRUE,
                            formats = getOption("jupyter.plot.formats",c("png","svgp","pdf"))
                            ) {
      desc <- self$display_obs[[display_id]]
      formats <- intersect(formats, self$formats)
      plot_id <- desc$plot_id
      renders <- lapply(formats,
                        self$render,
                        plot_id = plot_id,
                        width = desc$width,
                        height = desc$height,
                        resolution = desc$resolution,
                        zoom = desc$zoom)
      mime_data <- lapply(renders, "[[","content")
      mime_types <- lapply(renders, "[[","type")
      names(mime_data) <- mime_types
      mime_metadata <- lapply(renders, self$render_metadata, plot_id)
      names(mime_metadata) <- mime_types
      mime_data[["text/plain"]] <- self$get_render_url(
                                        plot_id = plot_id,
                                        width = desc$width,
                                        height = desc$height,
                                        resolution = desc$resolution,
                                        zoom = desc$zoom)
      d <- list(data = mime_data,
                metadata = mime_metadata)
      d$transient <- list(display_id=display_id)
      if(update) cl <- "update_display_data"
      else cl <- "display_data"
      state <- self$get_current_state()
      if(state$id == plot_id) {
        desc$upid <- state$upid
      }
      structure(d,class=cl)
    },
    render_metadata = function(r, plot_id) {
      m <- list(plot_id = plot_id)
      if(r$format %in% c("png", "tiff", "png-base64")) {
        f <- self$dpi
        m$width <- r$width * f
        m$height <- r$height * f
      } else if(r$format %in% c("svg","svgp", "svgz", "svgzp")) {
        f <- self$dpi
        m$width <- r$width * f
        m$height <- r$height * f
      }
      m
    },
    update_displays = function() {
      if(length(self$current_display) &&
         self$needs_update(self$current_display)) {
        d <- self$render_display(self$current_display)
        self$interface$display_send(d)
      }
    },
    needs_update = function(display_id) {
      desc <- self$display_obs[[display_id]]
      state <- self$get_current_state()
      state$id != desc$plot_id || state$upid != desc$upid
    }
  ))

GraphicsDisplay <- R6Class("GraphicsDisplay",
  public = list(
    plot_id = -1,
    width = -1,
    height = -1,
    resolution = 300,
    zoom = 1,
    manager = NULL,
    display_id = character(0),
    upid = 0,
    initialize = function(
      plot_id,
      width,
      height,
      resolution,
      zoom,
      manager,
      display_id
    ) {
      self$plot_id <- plot_id
      self$width <- width
      self$height <- height
      self$resolution <- resolution
      self$zoom <- zoom
      self$manager <- manager
      self$display_id <- display_id
    }
  )
)
