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
                http_url <- sprintf("/proxy/%d",port)
                if(nzchar(jupyterhub_prefix)){
                    http_url <- paste0(jupyterhub_prefix,http_url)
                    http_url <- gsub("//","/",http_url,fixed=TRUE)
                }
            }
            else {
                http_url <- sprintf("http://localhost:%d",port)
            }
            private$http_url <- http_url
            private$httpd_orig <- getFromNamespace("httpd","tools")
            replace_in_package("tools","httpd",private$httpd)
            self$add_http_handler("echo",http_echo)
            self$add_http_handler("eval",http_eval)
            self$add_http_handler("data",http_data)
        },
        add_http_handler = function(name, handler){
            private$handlers[[name]] <- handler
        },
        has_http_handler = function(name) {
            name %in% names(private$handlers)
        }
    ),
    private = list(
        http_url = NULL,
        http_port = 0,
        use_proxy = FALSE,
        httpd_orig = NULL,
        handlers = NULL,
        httpd = function(path,query,...){
            # log_out("httpd")
            # log_out(path)
            split_path <- strsplit(path,"/")[[1]]
            response <- NULL
            if(length(split_path) > 1){
                slug <- split_path[2]
                handler <- private$handlers[[slug]]
                if(is.function(handler))
                  response <- handler(path,query,...)
            }
            if(!length(response)){
                response <- private$httpd_orig(path=path,query=query,...)
                log_print(response)
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

#' @export
set_help_port <- function(port){
    http_server$current <- HTTPServer$new(port)
    options(help.ports=port)
    replace_in_package("utils","help.start",help_start)
    replace_in_package("tools","example2html",example_html)
    replace_in_package("tools","demo2html",demo_html)
}

http_server <- new.env()

httpd_url <- get_help_url <- function(){
    url <- http_server$current$get_url()
    return(url)
}

httpd_port <- get_help_port <- function(){
    url <- http_server$current$get_port()
    return(url)
}

#' @export
add_http_handler <- function(name, handler) {
    http_server$current$add_http_handler(name, handler)
}

#' @export
has_http_handler <- function(name) {
    http_server$current$has_http_handler(name)
}

#' @importFrom utils capture.output
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

#' @importFrom utils capture.output
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

#' @importFrom utils capture.output
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
        print = , capture.output(print_orig(res))
    )
    list(
      payload = paste0(payload,collapse="\n"),
      "content-type" = content_type,
      headers=NULL,
      "status code" = 200L
      )
  }


#' @export
http_get <- function(x) {
  con <- url(x)
  on.exit(close(con))
  paste(readLines(con, warn = FALSE),collapse="\n")
}

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

#' @export
show_http_log <- function() {
  for(msg in http_server$log) {
    cat(msg, "\n")
  }
}