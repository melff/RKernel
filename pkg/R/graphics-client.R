#' @import httpgd
#' @importFrom jsonlite fromJSON
#' @importFrom curl curl_fetch_memory
#' @importFrom jsonlite base64_enc
#' @import unigd
#' @export
GraphicsClient <- R6Class("GraphicsClient",
  public = list(
    initialize = function(session){
      private$session <- session
    },
    host = NULL,
    port = NULL,
    token = NULL,
    start = function(){
      private$session$run_cmd("RKernel::start_graphics()")
      self$host <- self$get_host()
      self$port <- self$get_port()
      self$token <- self$get_token()
      self$last_state <- self$get_state()
          add_sync_options(c(
          "jupyter.plot.width",
          "jupyter.plot.height",
          "jupyter.plot.res",
          "jupyter.graphics.types",
          "jupyter.update.graphics"))
    },
    get_host = function(){
      private$session$run_cmd("RKernel:::get_hgd_host()")$stdout
    },
    get_port = function(){
      private$session$run_cmd("RKernel:::get_hgd_port()")$stdout
    },
    get_token = function(){
      private$session$run_cmd("RKernel:::get_hgd_token()")$stdout
    },
    get_state = function(){
      gurl <- paste0(
                "http://",
                self$host,":",
                self$port,"/",
                "state","?",
                "token=",self$token)
      con <- url(gurl)
      res <- readLines(con, warn = FALSE)
      close(con)
      res <- fromJSON(res)
      res
    },
    get_plots = function(){
      gurl <- paste0(
                "http://",
                self$host,":",
                self$port,"/",
                "plots","?",
                "token=",self$token)
      con <- url(gurl)
      res <- readLines(con, warn = FALSE)
      close(con)
      res <- fromJSON(res)
    },
    last_state = NULL,
    current_plot = character(0),
    latest_plot = character(0),
    displayed_plots = character(0),
    # send display data if plot has changed
    display_changed = function() {
      # log_out("display_changed?")
      if(self$new_plot_created() || self$current_plot_changed()){
        d <- self$display_data()
        self$update_done()
        return(d)
      } else return(NULL)
    },
    # Check whether a new plot was created by plot_new or similar mechanism
    new_plot_created = function() {
      # log_out("new_plot_created?")
      if(!length(self$current_plot) && !length(self$latest_plot)) FALSE
      else if(!length(self$current_plot) && length(self$latest_plot)) stop("This should not happen")
      else {
        !length(self$latest_plot) ||self$current_plot != self$latest_plot
      }
    },
    latest_upid = 0,
    current_plot_changed = function() {
      if(!length(self$current_plot)) return(FALSE)
      # log_out("current_plot_changed?")
      pl <- self$get_plots()
      hgd_plots <- pl$plots$id
      # log_out(hgd_plots, use.str=TRUE)
      current_upid <- pl$state$upid
      latest_hgd_plot <- tail(hgd_plots, n=1L)
      # log_out("self$current_plot")
      # log_out(self$current_plot)
      # log_out("latest_hgd_plot")
      # log_out(latest_hgd_plot, use.str =TRUE)
      # log_out("current_upid")
      # log_out(current_upid, use.print =TRUE)
      # log_out("self$latest_upid")
      # log_out(self$latest_upid, use.print = TRUE)
      self$current_plot == latest_hgd_plot && current_upid != self$latest_upid
    },
    new_cell = TRUE,
    new_page = function() {
      # log_out("new_page?")
      np <- if(length(self$current_plot))
        !(self$current_plot %in% self$displayed_plots)
      else FALSE
      # log_out(np)
      return(np)
    },
    update_done = function() {
      # log_out("update_done")
      pl <- self$get_plots()
      current_upid <- pl$state$upid
      self$latest_upid <- current_upid
      self$latest_plot <- self$current_plot
    },
    display_id = NULL,
    dpi = 72,
    render = function(format, 
        id = character(0), 
        width=7,height=7,res=72) {
        zoom <- 1L
        width <- width * self$dpi
        height <- height * self$dpi
        if(format == "png") {
          zoom <- res/self$dpi
          width <- width
          height <- height
        }
        gurl <- paste0(
                "http://",
                self$host,":",
                self$port,"/",
                "plot","?",
                if(length(id)) "id=",id,"&",
                "token=", self$token,"&",
                "renderer=", format,"&",
                "width=", width,"&",
                "height=", height,"&",
                "zoom=",zoom)
        # log_out(gurl)
        curl_fetch_memory(gurl)
    },
    new_plot = function(msg) {
      # log_out("new_plot")
      # log_out(msg, use.print = TRUE)
      self$current_plot <- as.character(msg$plot_id)
    },
    before_new_plot = function(msg) {
      # log_out("before_new_plot")
    },
    display_data = function(width=getOption("jupyter.plot.width",7),
                            height=getOption("jupyter.plot.height",7),
                            resolution=getOption("jupyter.plot.res",144),
                            id=NULL,
                            update=!self$new_page() && (
                            getOption("jupyter.update.graphics", FALSE) ||
                            !self$new_cell),
                            ...)
        {
        # log_out("GraphicsClient$display_data")
        update <- force(update)
        rkernel_graphics_types <- getOption("jupyter.graphics.types",
                                            c("image/svg+xml",
                                              #"image/png",
                                              "application/pdf"))
        mime_binary <- c("image/svg+xml" = FALSE,
                        "image/png" = TRUE,
                        "application/pdf" = TRUE)
        mime_data <- list()
        mime_metadata <- list()
        for(mime_type in rkernel_graphics_types) {
          g_fmt <- graphics_formats[mime_type]
          rendered <- self$render(format=g_fmt,width=width,height=height,
                                  res=resolution,id=self$current_plot)
          if(mime_binary[mime_type]) {
            content <- base64_enc(rendered$content)
          }
          else {
            content <- rawToChar(rendered$content)
          }
          mime_data[[mime_type]] <- content
          mime_metadata[[mime_type]] <- list(
            width = width * resolution,
            height = height * resolution
          )
        }
        if(update) {
          cl <- "update_display_data"
          if(is.null(id))
            id <- self$display_id
          }
        else {
          cl <- "display_data"
          if(is.null(id))
            id <- UUIDgenerate()
          self$display_id <- id
        }
        d <- list(data = mime_data,
                  metadata = mime_metadata,
                  transient = list(display_id = id))
        # log_out(structure(d,class=cl), use.str = TRUE)
        self$displayed_plots <- union(self$displayed_plots, self$current_plot)
        return(structure(d,class=cl))
      }
  ),
  private = list(
    session = NULL
  )
)

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
    httpgd::hgd()
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