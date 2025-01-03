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
                      id = character(0)) {
      if(format == "png") {
        zoom <- self$png_res/self$dpi
        width <- self$width * self$png_res
        height <- self$height * self$png_res
      }
      else {
        zoom <- 1
        width <- self$width * self$dpi
        height <- self$height * self$dpi
      }
      gurl <- paste0(
                "http://",
                self$host,":",
                self$port,"/",
                "plot","?",
                if(length(id)) paste0("id=",(id - 1L),"&"),
                "token=", self$token,"&",
                "renderer=", format,"&",
                "width=", width,"&",
                "height=", height,"&",
                "zoom=",zoom)
      # gurl
      data <- curl_fetch_memory(gurl)
      data$width <- self$width
      data$height <- self$height
      data$format <- format
      data
    },
    display_res = 144,
    display_data = function() {
      renders <- lapply(self$formats,
                        self$render)
      mime_data <- lapply(renders, "[[","content")
      mime_types <- lapply(renders, "[[","type")
      mime_data[self$fmt_bin] <- lapply(mime_data[self$fmt_bin],base64_enc)
      mime_data[!self$fmt_bin] <- lapply(mime_data[!self$fmt_bin],rawToChar)
      names(mime_data) <- mime_types
      mime_metadata <- lapply(renders, 
                              function(r) {
                                list(
                                  width = r$width * self$display_res,
                                  height = r$height * self$display_res
                                )
                              })
      names(mime_metadata) <- mime_types
      list(data = mime_data,
           metadata = mime_metadata)
    }
  ))

get_hgd_host <- function() {
  info <- httpgd::hgd_details()
  cat(info$host)
}

get_hgd_port <- function() {
  info <- httpgd::hgd_details()
  cat(info$port)
}

get_hgd_token <- function() {
  info <- httpgd::hgd_details()
  cat(info$token)
}


graphics_formats <- c(
  "image/svg+xml"   = "svgp",
  "image/png"       = "png",
  "application/pdf" = "pdf"
)

#' @export
start_graphics <- function(){
    options(device=httpgd::hgd)
    setHook('plot.new', plot_new_hook)
    setHook('grid.newpage', send_new_plot)
    setHook('before.plot.new', send_before_new_plot)
    setHook('before.grid.newpage', send_before_new_plot)
    add_sync_options(c(
          "jupyter.plot.width",
          "jupyter.plot.height",
          "jupyter.plot.res",
          "jupyter.graphics.types",
          "jupyter.update.graphics"))
}

plot_new_hook <- function() {
  if(par("page")) send_new_plot()
}

send_new_plot <- function() {
  # log_out("send_new_plot")
  if(dev_is_unigd()){
    id <- ugd_id()$id
    msg <- list(type = "new_plot", plot_id = id)
    msg_send(msg)
    }
}

send_before_new_plot <- function() {
  # log_out("before_send_new_plot")
  if(dev_is_unigd()){
    msg <- list(type = "before_new_plot")
    msg_send(msg)
  }
}

dev_is_unigd <- function(which = dev.cur()) {
  names(which) == "unigd"
}

