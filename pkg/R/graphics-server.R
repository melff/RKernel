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
  info <- unclass(ugd_id(limit=0, state=TRUE))
  info$plots <- lapply(info$plots, unclass)
  payload <- to_json(info,
                     pretty=TRUE)
  list(payload=payload,
      `content-type`="application/json",
      headers="Access-Control-Allow-Origin: *",
      `status code`=200L)
}

http_graphics_plot <- function(query) {
  plot_id <- query["id"]
  format <- query["renderer"]
  width <- query["width"]
  height <- query["height"]
  zoom <- query["zoom"]
  data <- ugd_render(
    page = 0,
    width = as.numeric(width),
    height = as.numeric(height),
    zoom = as.numeric(zoom),
    as = format,
    which = graphics$device
  )
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
    setHook('plot.new', plot_new_hook)
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
    ur <- subset(ugd_renderers(),type=="plot")
    tikz <- which(ur$id == "tikz")
    ur$mime[tikz] <- "text/latex"
    graphics$mime_types <- structure(ur$mime,
                                     names=ur$id)
    graphics$binary <- structure(!ur$text,
                                 names=ur$id)
    log_print(graphics$mime_types)
}

#' @importFrom graphics par
plot_new_hook <- function() {
  if(par("page")) send_new_plot()
}

send_new_plot <- function() {
  # log_out("send_new_plot")
  if(dev_is_unigd()){
    id <- ugd_id()$id + 1L
    msg <- list(type="event",
                content = list(event = "new_plot", 
                               plot_id = id))
    msg_send(msg)
  }
}

send_before_new_plot <- function() {
  # log_out("before_send_new_plot")
  if(dev_is_unigd()){
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