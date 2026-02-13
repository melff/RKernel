#' @include graphics-renderer.R

http_graphics <- function(path, query, ...) {
  split_path <- strsplit(path,"/",fixed=TRUE)[[1]]
  path1 <- tail(split_path,1)
  switch(path1,
    state = http_graphics_state(),
    plot = http_graphics_plot(query)
  )
}

http_graphics_state <- function() {
    if(graphics$renderer$is_active()) {
        info <- graphics$renderer$state()
    } else {
        info <- list(state=list(active=FALSE))
    }
    payload <- to_json(info,
                       pretty=TRUE)
    list(payload=payload,
         `content-type`="application/json",
         headers="Access-Control-Allow-Origin: *",
         `status code`=200L)
}

from_query <- function(query, name, default) {
  if(name %in% names(query)) {
    query[name]
  } else default
}

#'@importFrom rsvg rsvg_pdf rsvg_png

http_graphics_plot <- function(query) {
  # log_out("http_graphics_plot")
  # log_print(query)
    plot_id <- as.integer(from_query(query, "id", 0))
    format <- from_query(query, "renderer", "svg")
    width <- as.numeric(from_query(query, "width", 
                                   getOption("jupyter.plot.width",7)))
    height <- as.numeric(from_query(query, "height", 
                                    getOption("jupyter.plot.height",7)))
    zoom <- as.numeric(from_query(query, "zoom", 
                                  getOption("jupyter.plot.zoom",1)))
    resolution <- as.integer(from_query(query, "resolution", 
                                        getOption("jupyter.plot.resolution",288)))
  
    data <- graphics$renderer$render(
      page       = plot_id,
      width      = width,
      height     = height,
      zoom       = zoom,
      format     = format,
      resolution = resolution
    )

    type <- graphics$mime_types[format]
    list(payload=data,
         `content-type`=type,
         headers="Access-Control-Allow-Origin: *",
         `status code`=200L)
}

#' importFrom svglite svgstring

start_graphics <- function(){
    setHook('plot.new', send_new_plot)
    setHook('grid.newpage', send_new_plot)
    # setHook('before.plot.new', send_before_new_plot)
    # setHook('before.grid.newpage', send_before_new_plot)
    options(device=svgstring)
    add_sync_options(c(
          "jupyter.plot.width",
          "jupyter.plot.height",
          "jupyter.plot.resolution",
          "jupyter.plot.formats",
          "jupyter.plot.zoom",
          "jupyter.update.graphics"))
    graphics$renderer <- GraphicsRenderer$new(
                                              width=getOption("jupyter.plot.width",7),
                                              height=getOption("jupyter.plot.height",7)
                                          )
}

#' @importFrom graphics par
send_new_plot <- function() {
    if(graphics$renderer$is_active()){
        state <- graphics$renderer$state()
        msg <- list(type="event",
                    content = list(event = "new_plot",
                                   plot_id = state$id,
                                   state = state
                               ))
        msg_send(msg)
    }
}

send_before_new_plot <- function() {
    if(graphics$renderer$is_active()){
        state <- graphics$renderer$state()
        msg <- list(type="event",
                    content = list(event = "before_new_plot", 
                                   plot_id = state$id))
        msg_send(msg)
    }
}


rawToChar_ <- function(x) {
  if(is.raw(x)) rawToChar(x)
  else if(is.character(x)) x[1]
  else ""
}

