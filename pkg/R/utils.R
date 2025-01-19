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

R.unsername <- function() {
  Sys.info()["user"]
}

# log_fn <- "/tmp/RKernel.log"
log_fn <- function() file.path(dirname(tempdir()),
                               paste(R.unsername(),"RKernel.log",sep="-"))

truncate_log <- function(){
  log_con <- file(log_fn(),open="wr")
  truncate(log_con)
  close(log_con)
}

is_kernel <- function() !is.null(kernel$current)

log_out <- function(message,...,use.print=FALSE,use.str=FALSE,serialize=FALSE){
  dcl <- deparse0(match.call())
  tryCatch({
    if(use.print)
      message <- paste0("\n",paste0(capture.output(print_orig(message)),collapse="\n"))
    else if(serialize){
        message <- to_json(message,pretty=TRUE,force=TRUE)
    }
    else if(use.str){
        message <- paste0("\n",paste0(capture.output(str(message)),collapse="\n"))
    }
    else message <- paste(message,...,collapse="")
    message <- paste(crayon::green(format(Sys.time()),"\t",message,"\n"))
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


#' @description
#' Show a warning in the Jupyter server log
#' @param message A string to be shown in the log
log_warning <- function(message){
  message <- paste(crayon::yellow(format(Sys.time()),"\t",message,"\n"))
  message <- paste("R WARNING:",message)
  if (is_kernel()) 
    message <- paste("R KERNEL -", message)
  else 
    message <- paste("          R SESSION -", message)
  cat(message,file=log_fn(),append=TRUE)
}
#' @description
#' Show an error message in the Jupyter server log
#' @param message A string to be shown in the log
log_error <- function(message, traceback = FALSE){
  message <- crayon::red(format(Sys.time()),"\t",message,"\n")
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
  env_name <- paste0("package:",pkg_name)
  if(env_name %in% search())
    env <- as.environment(env_name)
  else
    env <- getNamespace(pkg_name)
  .BaseNamespaceEnv$unlockBinding(name, env)
  if(update.env){
      environment(value) <- env
  }
  assign(name, value, env)
  .BaseNamespaceEnv$lockBinding(name, env)
}


#' @description
#' Create an HTML iframe tag that refers to some (usually HTML) code
#' @param code The code to be shown in the iframe
#' @param resize Logical; should the iframe be resizeable?
#' @param width The intended width of the iframe, a string or a number
#' @param aspect_ratio The intended aspect ratio of the iframe, a string
#' @param height The intended height, a string. Overrides the aspect ratio if given.
#' @param class The DOM class attribute the iframe, a string
#' @param style The CSS style attribte of the iframe, a string
#' @param ... Other arguments, ignored.
str2iframe <- function(code,
                      resize = FALSE,
                      width = "100%",
                      aspect_ratio = "16 / 10",
                      height = character(0),
                      class = "rkernel-iframe",
                      style = "border-style:none",
                      ...){
            # cl <- match.call()
            # log_out(deparse0(cl))
    # id <- UUIDgenerate()
    # path <- paste0("/iframe/",id,"/")
    # url <- paste0(httpd_url(),path)

    url <- dataURI(charToRaw(code),mime="text/html")

    url2iframe(url,resize,width,aspect_ratio,height,class,style,...)
}

#' @description
#' Create an HTML iframe tag that refers to some (usually HTML) code
#' @param code The code to be shown in the iframe
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
      style <- sprintf("width:%s;height:%s;%s",width,height,style)
    } else {
      style <- sprintf("width:%s;aspect-ratio:%s;%s",width,aspect_ratio,style)
    }

    if(isTRUE(resize) || resize == "both") {
      if_tmpl <- "<div class=\"resizer\">
                  <iframe src=\"%s\" class=\"%s resized\" style=\"%s\">
                  </iframe>
                  </div>
                "
    } 
    else if(resize == "vertical") {
      if_tmpl <- "<div class=\"vresizer\">
                  <iframe src=\"%s\" class=\"%s resized\" style=\"%s\">
                  </iframe>
                  </div>
                "
    }
    else if(resize == "horizontal") {
      if_tmpl <- "<div class=\"hresizer\">
                  <iframe src=\"%s\" class=\"%s resized\" style=\"%s\">
                  </iframe>
                  </div>
                "
    }
    else {
      if_tmpl <- "<iframe src=\"%s\" class=\"%s\" style=\"%s\">
                </iframe>
                "
    }
    iframe <- sprintf(if_tmpl,url,class,style)

    iframe
}



#' @export
q_orig <- getFromNamespace("q","base")

#' @export
q_defunct <- function(save = "default", status = 0, runLast = TRUE) {
    warning("\n'q()' and 'quit()' are deactivated for safety reasons.",
            "\nPlease use the Jupyter frontend to shut down the kernel.",
            call.=FALSE,immediate.=TRUE)
}

#' @export
install_safe_q <- function(){
    replace_in_package("base","q",q_defunct)
    replace_in_package("base","quit",q_defunct)
}

read_asset <- function(path) {
  path <- system.file(path,package="RKernel")
  paste(readLines(path),collapse="\n")
}

deparse0 <- function(expr, width.cutoff = 500L) {
  paste(deparse(expr, width.cutoff = width.cutoff),
        collapse = "\n")
}

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
          rkernel_show_traceback = getOption("rkernel_show_traceback",TRUE)
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

Sys.sleep.orig <- getFromNamespace("Sys.sleep","base")

Sys_sleep <- function(time) {
  cat(XOFF)
  Sys.sleep.orig(time)
  cat(XON)
}

install_sleep <- function(){
    replace_in_package("base", "Sys.sleep", Sys_sleep)
}

update_list <- function(l1, l2) {
  n <- names(l2)
  n <- n[nzchar(n) & !is.na(n)]
  l1[n] <- l2[n]
  l1
}

rkernel_urlbrowser <- function(url) {
    if(jupyter_gui()) {
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