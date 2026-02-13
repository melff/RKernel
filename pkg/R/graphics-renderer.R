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


GraphicsRenderer <- R6Class("GraphicsRenderer",
  public = list(
      device = NULL,
      get_svg = NULL,
      initialize = function(width = 7,
                            height = 7) {
          self$get_svg <- svgstring(standalone=FALSE,
                                    width=width,
                                    height=height)
          dev.control("enable")
          self$device <- dev.cur()
      },
      activate = function() {
          dev.set(self$device)
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
                        ...) {
          dev.set(self$device)
          plt <- recordPlot()
          s <- svgstring(width=width,height=height,standalone=FALSE)
          replayPlot(plt)
          data <- s()
          dev.off()
          
          if(format == "pdf") {
              data <- charToRaw(data)
              data <- rsvg_pdf(data,
                               width=width,
                               height=height)
          } else if(format == "png") {
              data <- charToRaw(data)
              data <- rsvg_png(data, width=width, height=height)
          } else if(format == "webp") {
              data <- charToRaw(data)
              data <- rsvg_webp(data, width=width, height=height)
          } else if(format == "postscript") {
              data <- charToRaw(data)
              data <- rsvg_ps(data, width=width, height=height)
          } else if(format == "eps") {
              data <- charToRaw(data)
              data <- rsvg_eps(data, width=width, height=height)
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
      }
  )
)
