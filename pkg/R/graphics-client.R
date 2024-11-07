#' @import httpgd
#' @importFrom jsonlite fromJSON
#' @importFrom curl curl_fetch_memory
#' @importFrom jsonlite base64_enc
#' @importFrom unigd ugd_id
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
    last_state = NULL,
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
      res$plots
    },
    # send display data if plot has changed
    display_changed = function() {
      log_out("display_changed")
      if(self$changed()){
        d <- self$display_data()
        self$update_done()
        return(d)
      } else return(NULL)
    },
    changed = function() {
      cur_state <- self$get_state()
      log_out(self$last_state,use.print=TRUE)
      log_out(cur_state,use.print=TRUE)
      cur_state$upid != self$last_state$upid
    },
    new_cell = TRUE,
    new_page = function() {
      # log_out("new_page?")
      if(is.null(self$last_state)) return(TRUE)
      if(self$last_state$hsize == 0) return(TRUE)
      cur_state <- self$get_state()
      # log_out(self$last_state, use.print = TRUE)
      # log_out(self$cur_state, use.print = TRUE)
      # log_out(cur_state$hsize != self$last_state$hsize)
      cur_state$hsize != self$last_state$hsize
    },
    update_done = function() {
      log_out("update_done")
      self$last_state <- self$get_state()
      self$new_cell <- FALSE
    },
    display_id = NULL,
    plot_id = character(0),
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
        log_out(gurl)
        curl_fetch_memory(gurl)
    },
    new_plot = function(msg) {
      log_out("new_plot")
      log_out(msg, use.print = TRUE)
      self$plot_id <- msg$plot_id
    },
    before_new_plot = function(msg) {
      log_out("before_new_plot")
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
        log_out("GraphicsClient$display_data")
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
                                  res=resolution)
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
        structure(d,class=cl)
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
    setHook('plot.new', send_new_plot)
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

send_new_plot <- function() {
  log_out("send_new_plot")
  id <- ugd_id()$id
  msg <- list(type = "new_plot", plot_id = id)
  msg_send(msg)
}

send_before_new_plot <- function() {
  msg <- list(type = "before_new_plot")
  msg_send(msg)
}