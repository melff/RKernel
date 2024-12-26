add_http_handler <- function(name, handler){
  assign(name, handler, tools:::.httpd.handlers.env)
}

has_http_handler <- function(name) {
  exists(name, envir = tools:::.httpd.handlers.env)
}

http_echo <- function(path, query, ...){
  payload <- list(path=path, query=query,...)
  payload[[4]] <- rawToChar(payload[[4]])
  payload <- capture.output(dput(payload))
  list(
    payload = paste0(payload,collapse="\n"),
    "content-type" = "text/plain",
    headers=NULL,
    "status code" = 200L
  )
}

http_eval <- function(path, query, ...){
    payload <- ""
    if(length(query)) {
      expr <- str2expression(query["expr"])
      env <- new.env()
      res <- try(eval(expr,envir=env,enclos=.GlobalEnv))
      payload <- switch(query["format"],
        deparse = deparse1(res),
        str = capture.output(str(res)),
        raw = res,
        cat = capture.output(cat(res)),
        json = toJSON(res),
        print = , capture.output(print_orig(res))
      )
      content_type <- if("content-type" %in% names(query)) {
        query["content-type"]
      } else switch(query["format"],
        json = "application/json",
        raw = "text/html",
        "text/plain"
      )
    }
    list(
      payload = paste0(payload,collapse="\n"),
      "content-type" = content_type,
      headers=NULL,
     "status code" = 200L
      )
  }

get_range <- function(x){
    rng <- strsplit(x,"-")[[1]]
    from <- as.integer(rng[1])
    to <- as.integer(rng[2])
    if(is.finite(from) && is.finite(to))
        seq.int(from,to)
    else
        0
}

http_data <- function(path, query, ...){
    content_type <- "text/plain"
    fmt <- if("format" %in% names(query)) query["format"]
            else "print"
    content_type <- if("content-type" %in% names(query)) {
                        query["content-type"]
                    } else switch(fmt,
                        json = "application/json",
                        raw = "text/html",
                        "text/plain"
                    )
    split_path <- strsplit(path,"/")[[1]]
    name <- tail(split_path,1)
    res <- ""
    if(name != "data") try({
        dta <- get0(name, envir=.GlobalEnv)
        row_sel <- NULL
        col_sel <- NULL
        if("rows" %in% names(query)) {
          row_sel <- get_range(query["rows"])
        }
        if("cols" %in% names(query)) {
          col_sel <- get_range(query["cols"])
        }
        if(length(row_sel) && length(col_sel)) {
          res <- dta[row_sel,col_sel]
        } else if (length(row_sel)) {
          res <- dta[row_sel,]
        }else if (length(col_sel)) {
          res <- dta[,col_sel]
        } else 
          res <- dta
        if("by-columns" %in% query) {
          res <- as.list(res)
        }
    })
    payload <- switch(fmt,
        deparse = deparse1(res),
        str = capture.output(str(res)),
        raw = res,
        cat = capture.output(cat(res)),
        json = toJSON(res),
        print = , capture.output(print_orig(res))
    )
    list(
      payload = paste0(payload,collapse="\n"),
      "content-type" = content_type,
      headers=NULL,
      "status code" = 200L
      )
  }


# httpd_env <- new.env()

#' @export
install_httpd_handlers <- function() {
  add_http_handler("echo",http_echo)
  add_http_handler("eval",http_eval)
  add_http_handler("data",http_data)
  #suppressMessages(httpd_env$port <- tools::startDynamicHelp(start=NA))
}

#' @export
httpd_port <- function() get_help_port()
#httpd_port <- function() get0("port",httpd_env)

#' @export 
httpd_url <- function() paste0(get_help_url(),"/custom/")
#httpd_url <- function() paste0("http://localhost:",httpd_port(),"/")

#' @export
http_get <- function(x) {
  con <- url(x)
  on.exit(close(con))
  paste(readLines(con, warn = FALSE),collapse="\n")
}

http_req_handlers <- new.env()

# #' @title Generalized http server
# #' @description
# #' A variant of 'tools:::httpd' that adapts the paths used in HTML-pages if 
# #' the help system is proxied. Also allows to implement handlers
# #' for specific URL patterns via the event mechanism (see \code{\link{EventManager}})
# #' @param path The \emph{relative} url, e.g. "/doc/html/something/index.html"
# #' @param query The query string (that appeared after '?' in the http request)
# #' @param ... Any further arguments, passed on to specific handlers
# httpd = function(path,query,...){
#     split_path <- strsplit(path,"/")[[1]]
#     response <- NULL
#     if(length(split_path) > 1){
#         slug <- split_path[2]
#         handler <- get0(slug,http_req_handlers)
#         if(is.function(handler))
#         response <- handler(path,query,...)
#     }
#     if(!length(response)){
#         response <- orig_httpd(path=path,query=query,...)
#         payload <- response$payload
#         payload <- gsub("/doc/html/",paste0(private$http_url,
#                                             "/doc/html/"),payload,fixed=TRUE)
#         response$payload <- payload
#     }
#     return(response)
# }