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
    initialize = function(details) {
      self$host <- details$host
      self$port <- details$port
      self$token <- details$token
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
    formats = c("png", "pdf", "svgp"),
    fmt_bin = c(TRUE,  TRUE,  FALSE),
    width = 7,
    height = 7,
    png_res = 288,
    dpi = 72,
    render = function(format = "svgp", 
                      plot_id = integer(0),
                      width = getOption("jupyter.plot.width",self$width),
                      height = getOption("jupyter.plot.height",self$height)) {
      gurl <- self$render_url(format = format,
                              plot_id = plot_id,
                              width = width,
                              height = height)
      data <- curl_fetch_memory(gurl)
      data$width <- width
      data$height <- height
      data$format <- format
      data
    },
    render_url = function(format = "svgp", 
                          plot_id = integer(0),
                          width = getOption("jupyter.plot.width",self$width),
                          height = getOption("jupyter.plot.height",self$height)) {
      if(format == "png") {
        zoom <- self$png_res/self$dpi
        width  <- width * self$png_res
        height <- height * self$png_res
      }
      else {
        zoom <- 1
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
    display_res = 144,
    display_data = function(plot_id = integer(0),
                            display_id=UUIDgenerate(),
                            update=FALSE,
                            width = getOption("jupyter.plot.width",self$width),
                            height = getOption("jupyter.plot.height",self$height)
                            ) {
      renders <- lapply(self$formats,
                        self$render,
                        plot_id = plot_id,
                        width = width,
                        height = height)
      mime_data <- lapply(renders, "[[","content")
      mime_types <- lapply(renders, "[[","type")
      mime_data[self$fmt_bin] <- lapply(mime_data[self$fmt_bin],base64_enc)
      mime_data[!self$fmt_bin] <- lapply(mime_data[!self$fmt_bin],rawToChar)
      names(mime_data) <- mime_types
      display_res <- getOption("jupyter.plot.res",self$display_res)
      mime_metadata <- lapply(renders, 
                              function(r) {
                                list(
                                  width   = r$width * display_res,
                                  height  = r$height * display_res,
                                  plot_id = plot_id
                                )
                              })
      names(mime_metadata) <- mime_types
      mime_data[["text/plain"]] <- self$render_url(plot_id = plot_id)
      d <- list(data = mime_data,
                metadata = mime_metadata)
      d$transient <- list(display_id=display_id)
      if(update) cl <- "update_display_data"
      else cl <- "display_data"
      self$store()
      structure(d,class=cl)
    }
  ))


#' @export
start_graphics <- function(){
    options(device=httpgd::hgd)
    add_sync_options(c(
          "jupyter.plot.width",
          "jupyter.plot.height",
          "jupyter.plot.res",
          "jupyter.graphics.types",
          "jupyter.update.graphics"))
}

dev_is_unigd <- function(which = dev.cur()) {
  names(which) == "unigd"
}

