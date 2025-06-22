orig_func <- new.env()

#' @importFrom uuid UUIDgenerate

uuid <- function() {
    id <- UUIDgenerate()
    gsub("-","",id)
}

#' @import R6 
#' @importFrom jsonlite prettify

#' @include json.R

fromRawJSON <- function(raw_json,...) {
    json <- rawToChar(raw_json)
    Encoding(json) <- "UTF-8"
    fromJSON(json,...)
}

toRawJSON <- function(x,...){
  json <- to_json(x,...)
  charToRaw(json)
}

namedList <- function() structure(list(),names=character(0))
emptyNamedList <- structure(list(),names=character(0))

#' @importFrom rappdirs user_log_dir
log_fn <- function() {
  log_dir <- user_log_dir()
  if(!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE,
                                      mode="700")
  file.path(log_dir,"RKernel.log")
}


truncate_log <- function(){
  log_con <- file(log_fn(),open="wr")
  truncate(log_con)
  close(log_con)
}

is_kernel <- function() !is.null(kernel$current)

#' @importFrom crayon green red yellow
#' @importFrom utils capture.output
log_out <- function(message,...,use.print=FALSE,use.str=FALSE,serialize=FALSE){
  dcl <- deparse0(match.call())
  tryCatch({
    if(use.print)
      message <- paste0("\n",paste0(capture.output(orig_func$print(message)),collapse="\n"))
    else if(serialize){
        message <- to_json(message,pretty=TRUE,force=TRUE)
    }
    else if(use.str){
        message <- paste0("\n",paste0(capture.output(str(message)),collapse="\n"))
    }
    else message <- paste(message,...,collapse="")
    message <- paste(green(format(Sys.time()),"\t",message,"\n"))
    message <- paste("INFO:", message)
    if (is_kernel()) 
      message <- paste("R KERNEL -", message)
    else 
      message <- paste("          R SESSION -", message)
        # self$cat(message,file=stderr())
    cat(message,file=log_fn(),append=TRUE)
  },error=function(e){
    log_error(sprintf("Error in %s",dcl))
    msg <- conditionMessage(e)
    log_error(msg)
  })
}

log_print <- function(x,...) log_out(x, use.print = TRUE)
log_str <- function(x,...) log_out(x, use.str = TRUE)


log_warning <- function(message){
  message <- paste(yellow(format(Sys.time()),"\t",message,"\n"))
  message <- paste("R WARNING:",message)
  if (is_kernel()) 
    message <- paste("R KERNEL -", message)
  else 
    message <- paste("          R SESSION -", message)
  cat(message,file=log_fn(),append=TRUE)
}

#' @importFrom utils limitedLabels
log_error <- function(message, traceback = FALSE){
  message <- red(format(Sys.time()),"\t",message,"\n")
  message <- paste("ERROR:",message)
  if (is_kernel()) {
    message <- paste("R KERNEL -", message)
    indent <- ""
  }
  else {
    message <- paste("R SESSION -", message)
    indent <- "        "
    message <- paste0(indent, message)
  }
  if(isTRUE(traceback)) {
    calls <- limitedLabels(sys.calls())
    calls <- paste0(indent,"  ",calls)
    calls <- paste(calls,collapse="\n")
    calls <- paste0(calls,"\n")
    message <- paste0(message,calls)
  } else if(is.character(traceback)) {
    traceback <- paste(traceback, collapse = "\n")
    traceback <- paste0(traceback,"\n")
    message <- paste0(message,traceback)
  }
  cat(message,file=log_fn(),append=TRUE)
}

log_check <- function(...){
  tryCatch(...,
      error = function(e) log_error(conditionMessage(e)))
}

replace_in_package <- function(pkg_name,name,value,update.env=FALSE){
  # This changes only the exported bit
  env_name <- paste0("package:",pkg_name)
  if(env_name %in% search()) {
    env <- as.environment(env_name)
    .BaseNamespaceEnv$unlockBinding(name, env)
    assign(name, value, envir = env)
    .BaseNamespaceEnv$lockBinding(name, env)
  }
  if(pkg_name != "base") { # Base is liberal
    # This additional dark magic is necessary to change the func *inside* the namespace ...
    ns <- getNamespace(pkg_name) 
    .BaseNamespaceEnv$unlockBinding(name, ns)
    assign(name, value, envir = ns)
    .BaseNamespaceEnv$lockBinding(name, ns)
    if(update.env){
      environment(value) <- ns
    }
    # Now menu(), utils::menu(), and utils:::menu() do the same :)
  }
}


#' Create an HTML iframe tag that refers to some (usually HTML) code
#' @param code The code to be shown in the iframe
#' @param resize Logical; should the iframe be resizeable?
#' @param width The intended width of the iframe, a string or a number
#' @param aspect_ratio The intended aspect ratio of the iframe, a string
#' @param height The intended height, a string. Overrides the aspect ratio if given.
#' @param class The DOM class attribute the iframe, a string
#' @param style The CSS style attribte of the iframe, a string
#' @param use_srcdoc A logical value, whether the 'srcdoc' attribute should
#'    be used to set the iframe content, rather than the 'src' attribute.
#' @param ... Other arguments, ignored.
str2iframe <- function(code,
                      resize = FALSE,
                      width = "100%",
                      aspect_ratio = "16 / 10",
                      height = character(0),
                      class = "rkernel-iframe",
                      style = "border-style:none",
                      use_srcdoc = FALSE,
                      ...){
            # cl <- match.call()
            # log_out(deparse0(cl))
    # id <- UUIDgenerate()
    # path <- paste0("/iframe/",id,"/")
    # url <- paste0(httpd_url(),path)

    if(use_srcdoc) {
      srcdoc_iframe(code,
                    resize = resize,
                    width = width,
                    aspect_ratio = aspect_ratio,
                    height = height,
                    class = class,
                    style = style,
                    ...)
    } else {
      url <- dataURI(charToRaw(code),mime="text/html")
      url2iframe(url,resize,width,aspect_ratio,height,class,style,...)
    }

}

#' Create an HTML iframe tag that refers to some (usually HTML) code
#' @param url The URL of the page to be shown in the iframe (can also be a data URI)
#' @param resize Logical; should the iframe be resizeable?
#' @param width The intended width of the iframe, a string or a number
#' @param aspect_ratio The intended aspect ratio of the iframe, a string
#' @param height The intended height, a string. Overrides the aspect ratio if given.
#' @param class The DOM class attribute the iframe, a string
#' @param style The CSS style attribte of the iframe, a string
#' @param ... Other arguments, ignored.
url2iframe <- function(url,
                      resize = FALSE,
                      width = "100%",
                      aspect_ratio = "16 / 10",
                      height = character(0),
                      class = "rkernel-iframe",
                      style = "border-style:none",
                      ...){

    resize_style <-"<style>
    .resizer { display:flex; margin:0; padding:0; resize:both; overflow:hidden }
    .vresizer { display:flex; margin:0; padding:0; resize:vertical; overflow:hidden }
    .hresizer { display:flex; margin:0; padding:0; resize:horizontal; overflow:hidden }
    .resizer > .resized,
    .hresizer > .resized,
    .vresizer > .resized { flex-grow:1; margin:0; padding:0; border:0 }
    </style>
    "

      if(length(height)) {
        dimens <- fill_tmpl("width:(( width )); height:(( height ))",
                           width=width,height=height)
      } else if(resize == "vertical") {
        dimens <- fill_tmpl("width:(( width )); height:300px")
      } else {
        dimens <- fill_tmpl("width:(( width )); aspect-ratio:(( aspect_ratio ))",
                            width=width,aspect_ratio=aspect_ratio)
      }

    if(isTRUE(resize) || resize == "both") {
      style <- paste0("width:100%; height:100%;",style)
      if_tmpl <- "<div class='resizer' style= '(( dimens ))'>
                  <iframe src='(( url ))' class='(( class )) resized' style='(( style ))'>
                  </iframe>
                  </div>
                "
      iframe <- fill_tmpl(if_tmpl, 
                          url = url, class = class, dimens = dimens, style = style)
    } 
    else if(resize == "vertical") {
      style <- paste0("width:100%; height:100%;",style)
      if_tmpl <- "<div class='vresizer' style= '(( dimens ))'>
                  <iframe src='(( url ))' class='(( class )) resized' style='(( style ))'>
                  </iframe>
                  </div>
                "
      iframe <- fill_tmpl(if_tmpl, 
                          url = url, class = class, dimens = dimens, style = style)
    }
    else if(resize == "horizontal") {
      style <- paste0("width:100%; height:100%;",style)
      if_tmpl <- "<div class='hresizer' style= '(( dimens ))'>
                  <iframe src='(( url ))' class='(( class )) resized' style='(( style ))'>
                  </iframe>
                  </div>
                "
      iframe <- fill_tmpl(if_tmpl, 
                          url = url, class = class, dimens = dimens, style = style)
    }
    else {
      style <- paste0(dimens,";",style)
      if_tmpl <- "<iframe src='(( url ))' class='(( class ))' style='(( style ))'>
                </iframe>
                "
      iframe <- fill_tmpl(if_tmpl, url = url, class = class, style = style)
    }

    if(!isFALSE(resize)) {
      iframe <- paste(resize_style, iframe, sep="\n")
    }

    iframe
}

srcdoc_iframe <- function(srcdoc,
                      resize = FALSE,
                      width = "100%",
                      aspect_ratio = "16 / 10",
                      height = character(0),
                      class = "rkernel-iframe",
                      style = "border-style:none",
                      escaped = FALSE,
                      ...){

    resize_style <-"<style>
    .resizer { display:flex; margin:0; padding:0; resize:both; overflow:hidden }
    .vresizer { display:flex; margin:0; padding:0; resize:vertical; overflow:hidden }
    .hresizer { display:flex; margin:0; padding:0; resize:horizontal; overflow:hidden }
    .resizer > .resized,
    .hresizer > .resized,
    .vresizer > .resized { flex-grow:1; margin:0; padding:0; border:0 }
    </style>
    "

    if(!escaped) {
      srcdoc <- escape4srcdoc(srcdoc)
    }

    if(length(height)) {
      dimens <- fill_tmpl("width:(( width ));height:(( height ))",
                          width=width,height=height)
    } else if(resize == "vertical") {
      dimens <- fill_tmpl("width(( width ));height:300px")
    } else {
      dimens <- fill_tmpl("width(( width ));aspect-ratio:(( aspect_ratio ))",
                          width=width,aspect_ratio=aspect_ratio)
    }

    if(isTRUE(resize) || resize == "both") {
      style <- paste0("width:100%;height:100%;",style)
      if_tmpl <- "<div class='resizer' style= '(( dimens ))'>
                  <iframe srcdoc=\"(( srcdoc ))\" class='(( class )) resized' style='(( style ))'>
                  </iframe>
                  </div>
                "
      iframe <- fill_tmpl(if_tmpl, 
                          srcdoc = srcdoc, class = class, dimens = dimens, style = style)
    } 
    else if(resize == "vertical") {
      style <- paste0("width:100%;height:100%;",style)
      if_tmpl <- "<div class='vresizer' style= '(( dimens ))'>
                  <iframe srcdoc=\"(( srcdoc ))\" class='(( class )) resized' style='(( style ))'>
                  </iframe>
                  </div>
                "
      iframe <- fill_tmpl(if_tmpl, 
                          srcdoc = srcdoc, class = class, dimens = dimens, style = style)
    }
    else if(resize == "horizontal") {
      style <- paste0("width:100%;height:100%;",style)
      if_tmpl <- "<div class='hresizer' style= '(( dimens ))'>
                  <iframe srcdoc=\"(( srcdoc ))\" class='(( class )) resized' style='(( style ))'>
                  </iframe>
                  </div>
                "
      iframe <- fill_tmpl(if_tmpl, 
                          srcdoc = srcdoc, class = class, dimens = dimens, style = style)
    }
    else {
      style <- paste0(dimens,";",style)
      if_tmpl <- "<iframe srcdoc=\"(( srcdoc ))\" class='(( class ))' style='(( style ))'>
                </iframe>
                "
      iframe <- fill_tmpl(if_tmpl, srcdoc = srcdoc, class = class, style = style)
    }

    if(!isFALSE(resize)) {
      iframe <- paste(resize_style, iframe, sep="\n")
    }

    iframe
}

escape4srcdoc <- function(x) {
  x <- gsub("&","&amp;amp;", x)
  gsub("\"","&quot;", x)
}

q_defunct <- function(save = "default", status = 0, runLast = TRUE) {
    warning("\n'q()' and 'quit()' are deactivated for safety reasons.",
            "\nPlease use the Jupyter frontend to shut down the kernel.",
            call.=FALSE,immediate.=TRUE)
}

#' @importFrom utils getFromNamespace
install_safe_q <- function(){
    orig_func$q <- getFromNamespace("q","base")
    replace_in_package("base","q",q_defunct)
    replace_in_package("base","quit",q_defunct)
}


BEL <- '\x07'
SO <- '\x0E'
SI <- '\x0F'
READLINE_PROMPT <- paste0(SO,"READLINE",SI)
SCAN_BEGIN <- paste0(SO,"SCAN_BEGIN",SI,BEL)
SCAN_END <- paste0(SO,"SCAN_END",SI,BEL)


readline <- function(prompt = "") {
  prompt <- paste0(READLINE_PROMPT,prompt,BEL)
  orig_func$readline(prompt = prompt)
}

install_readline <- function(){
  orig_func$readline <- getFromNamespace("readline","base")
  replace_in_package("base", "readline", readline)
}

read_asset <- function(path) {
  path <- system.file(path,package="RKernel")
  paste(readLines(path),collapse="\n")
}

deparse0 <- function(expr, width.cutoff = 500L) {
  paste(deparse(expr, width.cutoff = width.cutoff),
        collapse = "\n")
}

#' @importFrom utils head limitedLabels
send_error_condition <- function(e) {
  # log_out("send_error_condition")
  calls <- head(limitedLabels(sys.calls()),-2)
  msg <- list(
    type = "condition",
    content = list(
      condition = "error",
      message = conditionMessage(e),
      call = deparse0(conditionCall(e)),
      traceback = calls,
      options=list(
          rkernel_stop_on_error = getOption("rkernel_stop_on_error",TRUE),
          rkernel_show_traceback = getOption("rkernel_show_traceback",FALSE)
      )
    )
  )
  msg_send(msg, file=stderr())
}
install_globalCallingHandlers <- function() {
  globalCallingHandlers(
    error = send_error_condition
  )
}



preproc_code <- function(code) {
  # log_out("preproc_code")
  parsed <- str2expression(code)
  lapply(parsed, deparse)
  # unlist(lapply(res,pasteCR))
}

pasteCR <- function(x) {
  paste0(x,"\n", collapse="")
}


update_list <- function(l1, l2) {
  n <- names(l2)
  n <- n[nzchar(n) & !is.na(n)]
  l1[n] <- l2[n]
  l1
}

rkernel_urlbrowser <- function(url) {
    if(jupyter_gui()) {
      if(startsWith(url,"/")) {
        url <- paste0("file://",url)
      }
      if(startsWith(url, "file://")) {
        data <- curl_fetch_memory(url)
        text_html <- rawToChar(data$content)
      }
      else {
        text_html <- url2iframe(url)
      }
      d <- display_data(
              "text/plain" = url,
              "text/html" = text_html
            )
      display(d)
    }
}

jupyter_gui <- function() {
  "JPY_SESSION_NAME" %in% names(Sys.getenv())
}

install_browseURL <- function(){
    if(jupyter_gui()) {
      options(browser=rkernel_urlbrowser)
    }
}

scan <- function(file = "", ...) {
    if(is.character(file) && file=="") {
      cat(SCAN_BEGIN)
      on.exit(cat(SCAN_END))
      r <- orig_func$scan(file=file, ...)
      r
    }
    else orig_func$scan(file=file, ...)
}

install_scan <- function(){
    orig_func$scan <- getFromNamespace("scan","base")
    replace_in_package("base", "scan", scan)
}