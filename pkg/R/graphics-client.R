#' @import httpgd
#' @importFrom jsonlite fromJSON
#' @importFrom curl curl_fetch_memory
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
      private$session$run_cmd("options(device=hgd)")
      private$session$run_cmd("httpgd::hgd()")
      self$host <- self$get_host()
      self$port <- self$get_port()
      self$token <- self$get_token()
      self$last_state <- self$get_state()
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
                "state",
                "?token=",self$token)
      con <- url(gurl)
      res <- readLines(con, warn = FALSE)
      close(con)
      res <- fromJSON(res)
      res
    },
    last_state = NULL,
    changed = function() {
      cur_state <- self$get_state()
      cur_state$upid != self$last_state$upid
    },
    update_done = function() {
      self$last_state <- self$get_state()
    },
    render = function(format) {
        gurl <- paste0(
                "http://",
                self$host,":",
                self$port,"/",
                "plot?",
                "token=", self$token,
                "&renderer=", format)
        curl_fetch_memory(gurl)
    },
    getOption = function(n, default = NULL) {
      private$session$getOption(n, default = default)
    }
  ),
  private = list(
    session = NULL
  )
)

get_hgd_host <- function() {
  state <- httpgd::hgd_state()
  cat(state$host)
}

get_hgd_port <- function() {
  state <- httpgd::hgd_state()
  cat(state$port)
}

get_hgd_token <- function() {
  state <- httpgd::hgd_state()
  cat(state$token)
}

#' @include display.R
#' @importFrom uuid UUIDgenerate
#' @export
display_data.GraphicsClient <- function(x,
                                  width=x$getOption("jupyter.plot.width",7),
                                  height=x$getOption("jupyter.plot.height",7),
                                  resolution=x$getOption("jupyter.plot.res",144),
                                  id=UUIDgenerate(),
                                  update=FALSE,
                                  ...)
{
    rkernel_graphics_types <- x$getOption("jupyter.graphics.types",
                                        c("image/svg+xml",
                                          "image/png",
                                          "application/pdf"))
    mime_binary <- c("image/svg+xml" = FALSE,
                     "image/png" = TRUE,
                     "application/pdf" = TRUE)
    mime_data <- list()
    mime_metadata <- list()
    for(mime_type in rkernel_graphics_types) {
      g_fmt <- graphics_formats[mime_type]
      rendered <- x$render(format=g_fmt)
      if(mime_binary[mime_type]) {
        content <- rendered$content
      }
      else {
        content <- rawToChar(rendered$content)
      }
      mime_data[[mime_type]] <- content
    }
    d <- list(data = mime_data,
              metadata = mime_metadata,
              transient = list(display_id = id))
    if(update) cl <- "update_display_data"
    else cl <- "display_data"
    structure(d,class=cl)
}

graphics_formats <- c(
  "image/svg+xml"   = "svgp",
  "image/png"       = "png",
  "application/pdf" = "pdf"
)