GraphicsDisplay <- R6Class("GraphicsDisplay",
  public = list(
      display_id = character(0),
      render_info = list(),
      width = 7,
      height = 7,
      resolution = 300,
      dpi = 72,
      zoom = 1,
      rendered = FALSE,
      host = "",
      port = 0,
      kernel = list(),
      initialize = function(host,
                            port,
                            kernel,
                            render_info,
                            display_id=UUIDgenerate(),
                            width = getOption("jupyter.plot.width",self$width),
                            height = getOption("jupyter.plot.height",self$height),
                            resolution = getOption("jupyter.plot.resolution",288),
                            zoom = getOption("jupyter.plot.zoom",1)
                            ) {
          gurl <- make_render_url(host = host,
                                  port = port,
                                  uuid = render_info$uuid,
                                  page = render_info$page,
                                  format = "svg",
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
          self$kernel <- kernel
          self$host <- host
          self$port <- port
          self$display_data <- d
          self$render_info <- render_info
          self$display_id <- display_id
          self$width <- width
          self$height <- height
          self$resolution <- resolution
          self$zoom <- zoom
          self$formats <- names(graphics$mime_types)
          self$mime_types <- graphics$mime_types
          self$binary_formats <- graphics$binary_formats[self$formats]
     },
     display_data = list(),
     send = function() {
         d <- self$display_data
         self$kernel$display_send(d)
     },
     render = function(
                       formats = getOption("jupyter.plot.formats",c("png","svg","pdf")),
                       update = FALSE
                       ) {
         rendered <- lapply(formats,self$render1)
         mime_data <- lapply(rendered, "[[","content")
         mime_types <- unlist(lapply(rendered, "[[","type"))
         mime_metadata <- lapply(rendered, self$render1_metadata)
         self$display_data$data[mime_types] <- mime_data
         self$display_data$metadata[mime_types] <- mime_metadata
         if(update) {
             class(self$display_data) <- "update_display_data"
         }
         self$rendered <- TRUE
     },
     render1 = function(format) {
         gurl <- make_render_url(host = self$host,
                                 port = self$port,
                                 uuid = self$render_info$uuid,
                                 page = self$render_info$page,
                                 format = format,
                                 width = self$width,
                                 height = self$height,
                                 resolution = self$resolution,
                                 zoom = self$zoom)
         resp <- tryCatch(curl_fetch_memory(gurl),
                         error = function(e) invokeRestart("continue"),
                         interrupt = function(e) invokeRestart("continue"))
         data <- resp[c("content","type")]
         ii <- match(format, self$formats)
         if(is.raw(data$content)) {
             if(self$binary_formats[ii]) {
                 data$content <- base64_enc(data$content)
             } else {
                 data$content <- rawToChar_(data$content)
             }
         } 
         if(is.character(data$content)) {
             Encoding(data$content) <- "UTF-8"
         }
         data$meta_data <- list(
             format = format,
             uuid = self$render_info$uuid,
             page = self$render_info$page,
             width = self$width,
             height = self$height,
             resolution = self$resolution,
             zoom = self$zoom
         )
         data
     },
     render1_metadata = function(r) {
         m <- r$meta_data
         list(
             width = m$width * m$zoom * self$dpi,
             height = m$height * m$zoom * self$dpi
         )
     },
     formats = character(0),
     mime_types = character(0),
     binary_formats = logical(0),
     poll = function() {
         state <- self$get_state()
         updated <- state$upid > self$render_info$upid
         self$render_info <- state
         state$updated <- updated
         state
     },
     get_state = function() {
         gurl <- make_state_url(host = self$host,
                                port = self$port,
                                uuid = self$render_info$uuid)
         con <- url(gurl)
         on.exit(close(con))
         resp <- readLines(con, warn = FALSE)
         fromJSON(resp)
     },
     get_render_info = function(host, port) {
         gurl <- make_render_info_url(host, port)
         con <- url(gurl)
         on.exit(close(con))
         resp <- readLines(con, warn = FALSE)
         fromJSON(resp)
     }
  )
)

display_data.GraphicsDisplay <- function(x, ...) {
    x$display_data
}


make_render_url <- function(host,
                            port,
                            uuid,
                            page,
                            format,
                            width,
                            height,
                            resolution,
                            zoom) {
      paste0(
        "http://",
        host,":",
        port,"/",
        "graphics/",
        "render","?",
        "renderer_id=", uuid,"&",
        "page=", page, "&",
        "format=", format,"&",
        "width=", width,"&",
        "height=", height,"&",
        "resolution=", resolution,"&",
        "zoom=",zoom)
    
}


make_state_url <- function(host,port,uuid) {
      paste0(
        "http://",
        host,":",
        port,"/",
        "graphics/",
        "state","?",
        "renderer_id=", uuid)
}

make_render_info_url <- function(host,port) {
      paste0(
        "http://",
        host,":",
        port,"/",
        "render_info/")
}
