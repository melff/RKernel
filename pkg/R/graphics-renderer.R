#' @import svglite
#' @import R6
#' @import grDevices
#' @import rsvg
#' @importFrom digest digest

graphics <- new.env()
graphics$mime_types <- c(
    pdf="application/pdf",
    svg="image/svg+xml",
    png="image/png"
)
graphics$binary_formats <- c(
    pdf=TRUE,
    svg=FALSE,
    png=TRUE
)
graphics$renderers <- list()

GraphicsRenderer <- R6Class("GraphicsRenderer",
  public = list(
      device = NULL,
      get_svg = NULL,
      recordings = list(),
      prev_device = NULL,
      initialize = function(width = 7,
                            height = 7) {
          self$get_svg <- svgstring(standalone=FALSE,
                                    width=width,
                                    height=height)
          dev.control("enable")
          self$device <- dev.cur()
          graphics$renderers[[as.character(self$device)]] <- self
      },
      activate = function() {
          if(dev.cur() == self$device) return()
          self$prev_device <- dev.cur()
          dev.set(self$device)
      },
      suspend = function() {
          if(length(self$prev_device)) {
              dev.set(self$prev_device)
              self$prev_device <- NULL
          }
      },
      is_active = function() {
          dev_cur <- dev.cur()
          dev_cur == self$device && names(dev_cur) == "devSVG"
      },
      close = function() {
          dev.off(self$device)
      },
      render = function(page,
                        width = 7,
                        height = 7,
                        zoom = 1,
                        format = "svg",
                        resolution = 288,
                        ...) {
          s0 <- self$get_svg()
          n <- length(s0)
          if(page == 0 || page == n) {
              self$record()
              plt <- self$recordings[[n]]
          } else {
              plt <- self$recordings[[page]]
          }
          scale_correction <- 5/7 # svgstring pictures tend to be too large
          zoom <- zoom * scale_correction
          s <- svgstring(width      = width  * zoom,
                         height     = height * zoom,
                         scaling    = zoom)
          replayPlot(plt)
          data <- s()
          dev.off()

          if(format == "pdf") {
              pdf_res <- 72
              data <- charToRaw(data)
              data <- rsvg_pdf(data,
                               width  = width  * zoom * pdf_res,
                               height = height * zoom * pdf_res)
          } else if(format == "png") {
              data <- charToRaw(data)
              data <- rsvg_png(data,
                               width  = width  * zoom * resolution,
                               height = height * zoom * resolution)
          } else if(format == "webp") {
              data <- charToRaw(data)
              data <- rsvg_webp(data,
                                width  = width  * zoom * resolution,
                                height = height * zoom * resolution)
          } else if(format == "postscript") {
              data <- charToRaw(data)
              data <- rsvg_ps(data,
                              width  = width  * zoom,
                              height = height * zoom)
          } else if(format == "eps") {
              data <- charToRaw(data)
              data <- rsvg_eps(data,
                               width  = width  * zoom,
                               height = height * zoom)
          } else if(!(format %in% c("svg","svgp"))) {
              data <- NULL
          }
          return(data)
      },
      state = function() {
          s <- self$get_svg()
          n <- length(s)
          list(
              active = self$is_active(),
              id = n,
              upid = digest(unclass(s[n])))
      },
      record = function() {
          s <- self$get_svg()
          n <- length(s)
          cur_dev <- dev.cur()
          dev.set(self$device)
          plt <- recordPlot()
          self$recordings[[n]] <- plt
          dev.set(cur_dev)
      },
      par = NULL,
      save_par = function() {
          cur_dev <- dev.cur()
          dev.set(self$device)
          self$par <- par()
          dev.set(cur_dev)
      },
      restore_par = function() {
          cur_dev <- dev.cur()
          dev.set(self$device)
          par(self$par)
          dev.set(cur_dev)
      }
  )
)
