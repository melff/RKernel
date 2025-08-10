graphics <- new.env()

http_graphics <- function(path, query, ...) {
  split_path <- strsplit(path,"/",fixed=TRUE)[[1]]
  path1 <- tail(split_path,1)
  switch(path1,
    state = http_graphics_state(),
    plot = http_graphics_plot(query)
  )
}

http_graphics_state <- function() {
  if(names(dev.cur()) == "unigd") {
    info <- try(ugd_id(limit=0, state=TRUE),silent=TRUE)
    if(inherits(info,"try-error")){
      info <- list(state=list(active=FALSE))
    } else {
      info <- unclass(info)
      info$plots <- lapply(info$plots, unclass)
    }
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
  dpi <- 72
  plot_id <- as.integer(from_query(query, "id", 0))
  format <- from_query(query, "renderer", "svgp")
  width <- as.numeric(from_query(query, "width", 
                      getOption("jupyter.plot.width",7)))
  height <- as.numeric(from_query(query, "height", 
                      getOption("jupyter.plot.height",7)))
  zoom <- as.numeric(from_query(query, "zoom", 1))
  resolution <- as.integer(from_query(query, "resolution", 
                      getOption("jupyter.plot.res",dpi)))

  # log_print(format)
  if(format %in% c("png", "tiff", "png-base64")) {
    zoom <- resolution/dpi * zoom
    width  <- width * resolution
    height <- height * resolution
  }
  else {
    width  <- width * dpi
    height <- height * dpi
  }
  data <- ugd_render(
      page = plot_id,
      width = width,
      height = height,
      zoom = zoom,
      as = "svgp",
      which = graphics$device
  )

  if(format == "pdf") {
    data <- charToRaw(data)
    data <- rsvg_pdf(data, width=width, height=height)
  } else if(format == "png") {
    log_out(sprintf("Zoom = %s", zoom))
    log_out(sprintf("Resolution = %s", resolution))
    data <- charToRaw(data)
    data <- rsvg_png(data, width=width, height=height)
  }
  else {
    data <- ugd_render(
      page = plot_id,
      width = width,
      height = height,
      zoom = zoom,
      as = format,
      which = graphics$device
    )
  }
  # if(is.character(data)) {
  #   payload <- data
  # }
  # else if(is.raw(data)) {
  #   payload <- base64_enc(data)
  # } else {
  #   payload <- as.character(data)
  # }
  payload <- data
  type <- graphics$mime_types[format]
  list(payload=payload,
      `content-type`=type,
      headers="Access-Control-Allow-Origin: *",
      `status code`=200L)
}

start_graphics <- function(){
    setHook('plot.new', send_new_plot)
    setHook('grid.newpage', send_new_plot)
    # setHook('before.plot.new', send_before_new_plot)
    # setHook('before.grid.newpage', send_before_new_plot)
    options(device=ugd)
    add_sync_options(c(
          "jupyter.plot.width",
          "jupyter.plot.height",
          "jupyter.plot.res",
          "jupyter.plot.formats",
          "jupyter.update.graphics"))
    ugd()
    graphics$device <- dev.cur()
    #ur <- subset(ugd_renderers(),type=="plot")
    ur <- ugd_renderers()
    ur <- ur[ur$type=="plot",]
    tikz <- which(ur$id == "tikz")
    ur$mime[tikz] <- "text/latex"
    graphics$mime_types <- structure(ur$mime,
                                     names=ur$id)
    graphics$binary <- structure(!ur$text,
                                 names=ur$id)
}

#' @importFrom graphics par
send_new_plot <- function() {
  # log_out("send_new_plot")
  if(dev_is_main()){
    id <- ugd_id()$id + 1L
    state <- ugd_state()
    msg <- list(type="event",
                content = list(event = "new_plot", 
                               plot_id = id,
                               state = state
                               ))
    msg_send(msg)
  }
}

send_before_new_plot <- function() {
  # log_out("before_send_new_plot")
  if(dev_is_main()){
    id <- ugd_id()$id
    msg <- list(type="event",
                content = list(event = "before_new_plot", 
                               plot_id = id))
    msg_send(msg)
  }
}

#' @importFrom grDevices dev.cur
dev_is_unigd <- function(which = dev.cur()) {
  names(which) == "unigd"
}

dev_is_main <- function(which = dev.cur()) {
  names(which) == "unigd" && which == graphics$device
}


rawToChar_ <- function(x) {
  if(is.raw(x)) rawToChar(x)
  else if(is.character(x)) x[1]
  else ""
}

#' @importFrom jsonlite base64_enc
ugd_wrap <- function(x) {
  # log_out("ugd_wrap")
  # log_str(x)
  if(is.raw(x)) cat(base64_enc(x))
  else if(is.character(x)) cat(x)
}