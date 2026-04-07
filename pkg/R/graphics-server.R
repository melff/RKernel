#' @include graphics-renderer.R

http_graphics <- function(path, query, ...) {
  # log_out("http_graphics")
  # log_out(path)
  # log_out(query)
  split_path <- strsplit(path,"/",fixed=TRUE)[[1]]
  path1 <- tail(split_path,1)
  switch(path1,
    render_info = http_graphics_render_info(),
    state = http_graphics_state(query),
    render = http_graphics_render(query)
  )
}

http_graphics_render_info <- function() {
    # log_out("http_graphics_render_info")
    renderer <- get_current_renderer()
    if(length(renderer)) {
        info <- renderer$state()
        info$uuid <- renderer$uuid
        payload <- to_json(info,
                           pretty=TRUE)
        list(payload=payload,
             `content-type`="application/json",
             headers="Access-Control-Allow-Origin: *",
             `status code`=200L)
    } else {
        info <- list(active=FALSE)
        list(payload="No current renderer",
             `content-type`="text/plain",
             headers="Access-Control-Allow-Origin: *",
             `status code`=404L)
    }
}

from_query <- function(query, name, default) {
  if(name %in% names(query)) {
    query[name]
  } else default
}


http_graphics_state <- function(query) {
    # log_out("http_graphics_state")
    # log_print(query)
    uuid <- from_query(query, "renderer_id")
    renderer <- get_renderer(uuid)
    # log_print(renderer)
    if(length(renderer)) {
        info <- renderer$state()
        # log_print(info)
        info$uuid <- uuid
        payload <- to_json(info,
                           pretty = TRUE)
        resp <- list(payload = payload,
             `content-type` = "application/json",
             headers = "Access-Control-Allow-Origin: *",
             `status code` = 200L)
    }
    else {
        info <- list(active = FALSE)
        resp <- list(payload = "Renderer not found",
             `content-type` = "text/plain",
             headers = "Access-Control-Allow-Origin: *",
             `status code` = 404L)
    }
    # log_print(resp)
    return(resp)
}


#'@importFrom rsvg rsvg_pdf rsvg_png

http_graphics_render <- function(query) {
    # log_out("http_graphics_render")
    # log_print(query)
    uuid <- from_query(query, "renderer_id")
    renderer <- get_renderer(uuid)
    page <- as.integer(from_query(query, "page",0))
    format <- from_query(query, "format", "svg")
    width <- as.numeric(from_query(query, "width", 
                                   getOption("jupyter.plot.width",7)))
    height <- as.numeric(from_query(query, "height", 
                                    getOption("jupyter.plot.height",7)))
    zoom <- as.numeric(from_query(query, "zoom", 
                                  getOption("jupyter.plot.zoom",1)))
    resolution <- as.integer(from_query(query, "resolution", 
                                        getOption("jupyter.plot.resolution",288)))

    payload <- renderer$render(
      page       = page,
      width      = width,
      height     = height,
      zoom       = zoom,
      format     = format,
      resolution = resolution
    )

    type <- graphics$mime_types[format]
    list(payload        = payload,
         `content-type` = type,
         headers        = "Access-Control-Allow-Origin: *",
         `status code`  = 200L)
}

#' importFrom svglite svgstring

start_graphics <- function(){
    setHook('plot.new', send_new_plot)
    setHook('grid.newpage', send_new_plot)
    setHook('before.plot.new', send_before_new_plot)
    setHook('before.grid.newpage', send_before_new_plot)
    options(device=new_jupyter_dev)
    add_sync_options(c(
          "jupyter.plot.width",
          "jupyter.plot.height",
          "jupyter.plot.resolution",
          "jupyter.plot.formats",
          "jupyter.plot.zoom",
          "jupyter.update.graphics"))
}

#' @importFrom graphics par
send_new_plot <- function() {
    # log_out("send_new_plot")
    # log_print(dev.list())
    current_renderer <- get_current_renderer()
    # log_print(current_renderer)
    if(!length(current_renderer)) return()
    if(current_renderer$is_active() && par("page")){
        state <- current_renderer$state()
        msg <- list(type="event",
                    content = list(event = "new_plot",
                                   state = state
                               ))
        msg_send(msg)
    }
}

send_before_new_plot <- function() {
    # log_out("send_before_new_plot")
    current_renderer <- get_current_renderer()
    if(!length(current_renderer)) return()
    if(current_renderer$is_active()){
        state <- current_renderer$state()
        current_renderer$record()
        msg <- list(type="event",
                    content = list(event = "before_new_plot", 
                                   state = state))
        msg_send(msg)
    }
}


rawToChar_ <- function(x) {
  if(is.raw(x)) rawToChar(x)
  else if(is.character(x)) x[1]
  else ""
}

