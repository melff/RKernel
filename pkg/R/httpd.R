#' @importFrom utils getFromNamespace

HTTPServer <- R6Class("HTTPServer",
    public = list(
        initialize = function(port = getOption("help.ports")[0]){
            private$http_port <- port # Provided by the frontend
        },
        get_url = function(){
            if(!length(private$http_url))
                self$start()
            return(private$http_url)
        },
        get_port = function(){
            return(private$http_port)
        },
        get_prefix = function() {
            return(private$url_prefix)
        },
        start = function(){
            options(help.ports = private$http_port)
            suppressMessages(port <- tools::startDynamicHelp(NA))
            if (private$http_port != port) {
                warning(sprintf(
                    "Changed help port from %d to %d",
                    private$http_port,
                    port
                ))
                options(http.port = port)
                private$http_port <- port
            }
            jupyterhub_prefix <- Sys.getenv("JUPYTERHUB_SERVICE_PREFIX")
            if(nzchar(jupyterhub_prefix)) private$use_proxy <- TRUE
            if(private$use_proxy){
                url_prefix <- "/proxy/"
                if(nzchar(jupyterhub_prefix)){
                    url_prefix <- paste0(jupyterhub_prefix,url_prefix)
                    url_prefix <- gsub("//","/",url_prefix,fixed=TRUE)
                }
            }
            else {
                url_prefix <- "http://localhost:"
            }
            http_url <- paste0(url_prefix,port)
            private$http_url <- http_url
            private$url_prefix <- url_prefix
            private$httpd_orig <- getFromNamespace("httpd","tools")
            replace_in_package("tools","httpd",private$httpd)
            self$add_http_handler("echo",http_echo)
            self$add_http_handler("eval",http_eval)
            self$add_http_handler("data",http_data)
            self$add_http_handler("graphics",http_graphics)
            self$add_http_handler("msg",http_msg)
        },
        add_http_handler = function(name, handler){
            private$handlers[[name]] <- handler
        },
        has_http_handler = function(name) {
            name %in% names(private$handlers)
        },
        proxied = function() {
            private$use_proxy
        }
    ),
    private = list(
        http_url = NULL,
        http_port = 0,
        use_proxy = FALSE,
        url_prefix = "http://localhost:",
        httpd_orig = NULL,
        handlers = NULL,
        httpd = function(path,query,...){
            # log_out("httpd")
            # log_out(path)
            http_log(path)
            split_path <- split_string1(path,"/")
            response <- NULL
            if(length(split_path) == 1) {
              response <- redirect2docroot()
            }
            else if(length(split_path) > 1){
                slug <- split_path[2]
                handler <- private$handlers[[slug]]
                if(is.function(handler))
                  response <- handler(path,query,...)
            }
            if(!length(response)){
                response <- private$httpd_orig(path=path,query=query,...)
                #log_print(response)
                payload <- response$payload
                payload <- gsub("/doc/html/",paste0(httpd_url(),
                                                    "/doc/html/"),payload,fixed=TRUE)
                response$payload <- payload
            }
            # log_print(response)
            return(response)
        }
    )
)

#' Set the port the HTTP server listens to. For internal use only.
#' @param port An integer
#' @export
set_http_port <- function(port){
    # log_out("set_http_port")
    http_server$current <- HTTPServer$new(port)
    http_server$current$start()
    options(help.ports=port)
    replace_in_package("utils","help.start",help_start)
    replace_in_package("tools","example2html",example_html)
    replace_in_package("tools","demo2html",demo_html)
}

http_server <- new.env()

#' Get the port the HTTP server listens to.
#' @export
httpd_url <- get_help_url <- function(){
    if(!length(http_server$current)) {
      port <- random_open_port()
      http_server$current <- HTTPServer$new(port)
      http_server$current$start()
    }
    url <- http_server$current$get_url()
    return(url)
}

#' Get the port of the HTTP server.
#' @export
httpd_port <- get_http_port <- function(){
    if(!length(http_server$current)) {
      port <- random_open_port()
      http_server$current <- HTTPServer$new(port)
      http_server$current$start()
    }
    url <- http_server$current$get_port()
    return(url)
}

#' Get the prefix for internal URLs
#' @export
url_prefix <- function() {
  url_prefix <- "http://localhost:"
  jupyterhub_prefix <- Sys.getenv("JUPYTERHUB_SERVICE_PREFIX")
  if(nzchar(jupyterhub_prefix)) {
      url_prefix <- "/proxy/"
      url_prefix <- paste0(jupyterhub_prefix,url_prefix)
      url_prefix <- gsub("//","/",url_prefix,fixed=TRUE)
  }
  url_prefix
}

#' Adapt an localhost URL so that it works from behind Jupyterhub
#' @param url A character string
#' @export
fix_localhost <- function(url) {
  # log_out("fix_localhost")
  if(http_is_proxied()) {
      prefix <- url_prefix()
      # prefix <- "somewhere/proxy/"
      pattern <- character(0)
      if(startsWith(url,"http://localhost:")) {
        pattern <- "http://localhost:([0-9]+)"
      }
      if(startsWith(url,"http://127.0.0.1:")) {
        pattern <- "http://127.0.0.1:([0-9]+)"
      }
      if(length(pattern)) {
        replacement <- paste0(prefix,"\\1/")
        url <- sub(pattern,replacement,url)
        url <- gsub("//","/",url)
      }
  } 
  url
}

#' Add a handler for HTTP requests with slug given by `slug`.
#' @param slug The first part (slug) of the URL to be handled
#' @param handler A function that handles GET requests with URLs that 
#'    have `slug` as path component
#' @export
add_http_handler <- function(slug, handler) {
    http_server$current$add_http_handler(slug, handler)
}

#' Check whether a handler exists for URLs that have `slug` as path component.
#' @param slug The first part (slug) of the URL to be handled
#' @export
has_http_handler <- function(slug) {
    http_server$current$has_http_handler(slug)
}

#' Check whether HTTP requests are proxied. This is the case if the kernel
#' is started from Jupyterhub and the Python package "jupyter_server_proxy"
#' is installed. 
#' 
#' If requests are proxied and the HTTP server of the R session listens at
#' port `<PORT>` then the appropriate URL is of the form 
#' `<JUPYTERHUB_SERVICE_PREFIX>/proxy/<PORT>` in addition to (or instead of)
#' `http://localhost:<PORT>`. 
#' @export
http_is_proxied <- function() {
    # http_server$current$proxied()
    jupyterhub_prefix <- Sys.getenv("JUPYTERHUB_SERVICE_PREFIX")
    nzchar(jupyterhub_prefix)
}

#' Handle requests with slug "echo".
#' 
#' A GET request from URL `http://localhost:XYZ/echo` will just
#' return the request.
#' @importFrom utils capture.output
#' @param path The url, excluding the hostname and port. Will be "/echo".
#' @param query The query string translated into a character vector.
#' @param ... Any other arguments, ignored.
#' @export
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

#' Handle requests with slug "eval".
#' 
#' These request can be used to have R evaluate an expression
#' @importFrom utils capture.output
#' @param path The url, excluding the hostname and port. 
#' @param query The query string translated into a character vector.
#'    The query argument named "expr" will be parsed and evaluated in a temporary environment.
#'    The result of evaluated expression will be shown in a format
#'    specified by the "format" query parameter. The value of the "format" query
#'    parameter should be one of "deparse", "str", "raw", "cat", "json", or 
#'    "print".
#' @param ... Any other arguments, ignored.
#' @examples 
#' \dontrun{
#'  browseURL(paste0(httpd_url(),"/eval?expr=Sys.time()&format=json"))
#' } 
#' @importFrom utils capture.output
#' @export
http_eval <- function(path, query, ...){
    payload <- ""
    if(length(query)) {
      expr <- str2expression(query["expr"])
      env <- new.env()
      res <- try(eval(expr,envir=env,enclos=.GlobalEnv))
      payload <- switch(query["format"],
        deparse = deparse0(res),
        str = capture.output(str(res)),
        raw = res,
        cat = capture.output(cat(res)),
        json = toJSON(res),
        print = , capture.output(orig_func$print(res))
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

#' Handle requests with slug "data".
#' 
#' These request can be used to get the value of an R object, usually a data frame.
#' @importFrom utils capture.output
#' @param path The url, excluding the hostname and port, but including
#'    the slug "data" and the name of an R object.
#' @param query The query string translated into a character vector.
#'    The value of the "format" query
#'    parameter should be one of "deparse", "str", "raw", "cat", "json", or 
#'    "print". The query parameters "rows" and "cols" can be used to select
#'    rows and columns.
#' @param ... Any other arguments, ignored.
#' @examples 
#' \dontrun{
#'   browseURL(paste0(httpd_url(),"/data/iris?rows=1-5&format=json"))
#' }
#' @importFrom utils capture.output
#' @export
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
        } else if (length(col_sel)) {
          res <- dta[,col_sel]
        } else 
          res <- dta
        if("by-columns" %in% query) {
          res <- as.list(res)
        }
    })
    payload <- switch(fmt,
        deparse = deparse0(res),
        str = capture.output(str(res)),
        raw = res,
        cat = capture.output(cat(res)),
        json = toJSON(res),
        print = , capture.output(orig_func$print(res))
    )
    list(
      payload = paste0(payload,collapse="\n"),
      "content-type" = content_type,
      headers=NULL,
      "status code" = 200L
      )
  }

redirect2docroot <- function() {
    payload <- '<head>
  <meta http-equiv="Refresh" content="0; URL=/doc/html/index.html" />
</head>
'
    content_type <- "text/html"
    list(
      payload = paste0(payload,collapse="\n"),
      "content-type" = content_type,
      headers=NULL,
      "status code" = 200L
      )
}

#' Handle a generic message request sent via getting from an
#' url with the slug 'msg'.
#' @param path The url, excluding the hostname and port, but including
#'    the slug "data" and the name of an R object.
#' @param query The query string translated into a character vector.
#' @param ... Any other arguments, ignored.
http_msg <- function(path, query, ...) {
  msg <- json_unwrap(query)
  resp <- handle_request(msg)
  if(length(resp)) {
    #payload <- to_json(resp)
    #content_type <- "application/json"
    # payload <- deparse0(resp)
    # content_type <- "text/x-r-source"
    payload <- serialize(resp,connection=NULL)
    content_type <- "application/x-r-data"
  } else {
    payload <- "OK"
    content_type <- "text/plain"
  }
  list(
      payload = payload,
      "content-type" = content_type,
      headers=NULL,
      "status code" = 200L
  )
}

#' Send a GET request to an URL and read the response.
#' @param x A character string with an URL.
#' @examples 
#' \donttest{
#'  http_get(paste0(httpd_url(),"/data/iris?rows=1-5&format=json"))
#' }
#' @export
http_get <- function(x) {
  con <- url(x)
  on.exit(close(con))
  paste(readLines(con, warn = FALSE),collapse="\n")
}

#' Add a message to the log of HTTP requests
#' 
#' This is intended to be used internally.
#' @param msg A character string
#' @export
http_log <- function(msg) {
  msg <- c(format(Sys.time()),msg)
  msg <- paste(msg, collapse = "\n")
  if(!("log" %in% ls(http_server))) {
    http_server$log <- msg
  }
  else {
    http_server$log <- c(http_server$log, msg)
  }
}

#' Show the log of the HTTP requests.
#' @examples show_http_log()
#' @export
show_http_log <- function() {
  for(msg in http_server$log) {
    cat(msg, "\n")
  }
}

