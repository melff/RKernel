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
  dcl <- deparse1(match.call())
  tryCatch({
    if(use.print)
      message <- paste0("\n",paste0(capture.output(print_orig(message)),collapse="\n"))
    else if(serialize){
        message <- to_json(message,pretty=TRUE,force=TRUE)
    }
    else if(use.str){
        message <- paste0("\n",paste0(capture.output(str_(message)),collapse="\n"))
    }
    else message <- paste(message,...,collapse="")
    message <- paste(crayon::green(format(Sys.time()),"\t",message,"\n"))
    message <- paste("INFO:", message)
    if (is_kernel()) 
      message <- paste("R KERNEL -", message)
    else 
      message <- paste("          R SESSION -", message)
        # self$cat(message,file=stderr())
    cat_(message,file=log_fn(),append=TRUE)
  },error=function(e){
    log_error(sprintf("Error in %s",dcl))
    msg <- conditionMessage(e)
    log_error(msg)
  })
}

#' @description
#' Show a warning in the Jupyter server log
#' @param message A string to be shown in the log
log_warning = function(message){
  message <- paste(crayon::yellow(format(Sys.time()),"\t",message,"\n"))
  message <- paste("R WARNING:",message)
  if (is_kernel()) 
    message <- paste("R KERNEL -", message)
  else 
    message <- paste("          R SESSION -", message)
  cat_(message,file=log_fn(),append=TRUE)
}
#' @description
#' Show an error message in the Jupyter server log
#' @param message A string to be shown in the log
log_error = function(message){
  message <- crayon::red(format(Sys.time()),"\t",message,"\n")
  message <- paste("ERROR:",message)
  if (is_kernel()) 
    message <- paste("R KERNEL -", message)
  else 
    message <- paste("          R SESSION -", message)
  cat_(message,file=log_fn(),append=TRUE)
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
#' @param height The intended height of the iframe, a string or a number
#' @param class The DOM class attribute the iframe, a string
#' @param style The CSS style attribte of the iframe, a string
#' @param ... Other arguments, ignored.
str2iframe <- function(code,
                      resize = TRUE,
                      width = "100%",
                      height = 400L,
                      class = "rkernel-iframe",
                      style = "border-style:none",
                      ...){
            # cl <- match.call()
            # log_out(deparse1(cl))
    # id <- UUIDgenerate()
    # path <- paste0("/iframe/",id,"/")
    # url <- paste0(httpd_url(),path)

    url <- dataURI(charToRaw(code),mime="text/html")

    url2iframe(url,resize,width,height,class,style,...)
}

#' @description
#' Create an HTML iframe tag that refers to some (usually HTML) code
#' @param code The code to be shown in the iframe
#' @param resize Logical; should the iframe be resizeable?
#' @param width The intended width of the iframe, a string or a number
#' @param height The intended height of the iframe, a string or a number
#' @param class The DOM class attribute the iframe, a string
#' @param style The CSS style attribte of the iframe, a string
#' @param ... Other arguments, ignored.
url2iframe <- function(url,
                      resize = FALSE,
                      width = "100%",
                      height = 400L,
                      class = "rkernel-iframe",
                      style = "border-style:none",
                      ...){

    resize_style <-"<style>
    .resizer { display:flex; margin:0; padding:0; resize:both; overflow:hidden }
    .resizer > .resized { flex-grow:1; margin:0; padding:0; border:0 }
    .ugly { background:red; border:4px dashed black; }
    </style>
    "


    if(resize) {
      if_tmpl <- "<div class=\"resizer\">
                  <iframe src=\"%s\" width=\"%s\" height=\"%s\" class=\"%s resized\" style=\"%s\">
                  </iframe>
                  </div>
                "
      iframe <- sprintf(if_tmpl,url,width,height,class,style)
    }
    else {
      if_tmpl <- "<iframe src=\"%s\" width=\"%s\" height=\"%s\" class=\"%s\" style=\"%s\">
                </iframe>
                "
      iframe <- sprintf(if_tmpl,url,width,height,class,style)
    }

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
install_save_q <- function(){
    replace_in_package("base","q",q_defunct)
    replace_in_package("base","quit",q_defunct)
}

#' @export
readline_orig <- getFromNamespace("readline","base")

READLINE_prompt <- "[$$READLINE$$]"

readline_ <- function(prompt = "") {
  prompt <- paste0(prompt, READLINE_prompt)
  readline_orig(prompt = prompt)
}

#' @export
install_readline <- function(){
    replace_in_package("base", "readline", readline_)
}

menu_ <- function(choices,title=NULL,...) {
    # Needed because it is possible to spot menues based on a specific
    # prompt without encountering many false positives.
    nc <- length(choices)
    if (length(title) && nzchar(title[1L])) 
        cat(title[1L], "\n")
    op <- paste0(format(seq_len(nc)), ": ", choices)
    if (nc > 10L) {
        fop <- format(op)
        nw <- nchar(fop[1L], "w") + 2L
        ncol <- getOption("width")%/%nw
        if (ncol > 1L) 
            op <- paste0(fop, c(rep.int("  ", min(nc, ncol) - 
                1L), "\n"), collapse = "")
    }
    cat("", op, "", sep = "\n")
    repeat {
        resp <- readline()
        if(grepl("[0-9]+",resp)) {
            ind <- as.integer(resp)
            if (ind <= nc) 
                return(ind)
        }
        cat(gettext("Enter an item from the menu, or 0 to exit\n"))
    }
}

#' @export
install_menu <- function(){
    replace_in_package("utils", "menu", menu_)
}
