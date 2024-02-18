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


log_fn <- "/tmp/RKernel.log"



log_out <- function(message,...,use.print=FALSE,use.str=FALSE,serialize=FALSE){
  dcl <- deparse1(match.call())
  tryCatch({
    if(use.print)
      message <- paste0("\n",paste0(capture.output(print_(message)),collapse="\n"))
    else if(use.str || serialize){
        message <- to_json(message,pretty=TRUE,force=TRUE)
    }
    else message <- paste(message,...,collapse="")
    message <- paste(crayon::green(format(Sys.time()),"\t",message,"\n"))
    message <- paste("R INFO:",message)
        # self$cat(message,file=stderr())
    cat_(message,file=log_fn,append=TRUE)
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
      # self$cat(message,file=stderr())
  cat_(message,file=log_fn,append=TRUE)
}
#' @description
#' Show an error message in the Jupyter server log
#' @param message A string to be shown in the log
log_error = function(message){
  message <- crayon::red(format(Sys.time()),"\t",message,"\n")
  message <- paste("R ERROR:",message)
      # self$cat(message,file=stderr())
  cat_(message,file=log_fn,append=TRUE)
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
#' @param width The intended width of the iframe, a string or a number
#' @param height The intended height of the iframe, a string or a number
#' @param class The DOM class attribute the iframe, a string
#' @param style The CSS style attribte of the iframe, a string
#' @param ... Other arguments, ignored.
str2iframe <- function(code,
                      width = "100%",
                      height = 400L,
                      class = "rkernel-iframe",
                      style = "border-style:none",...){
            # cl <- match.call()
            # log_out(deparse1(cl))
    id <- UUIDgenerate()
    path <- paste0("/iframe/",id,"/")
    url <- paste0(self$get_url(),path)
    if(!eventmanagers$http$has("iframe"))
        eventmanagers$http$on("iframe",private$handle_iframe)
    private$iframes[[id]] <- code
    if_tmpl <- "
                   <iframe src=\"%s\" width=\"%s\" height=\"%s\" class=\"%s\" style=\"%s\">
                   </iframe>
                   "
    iframe <- sprintf(if_tmpl,url,width,height,class,style)
            # mime_data <- list(
            #     "text/plain" = "",
            #     "text/html"  = iframe
            # )
            # metadata <- emptyNamedList
            # transient <- list(display_id=id)
            # display_data(data=mime_data,
            #              metadata=metadata,
            #              transient=transient)
    structure(iframe,class="iframe",id=id)
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

readline_ <- function(prompt = "") zmq_input_request(prompt=prompt)

#' @export
install_readline <- function(){
    replace_in_package("base","readline",readline_)
}
