#' @importFrom htmltools htmlEscape
#' @importFrom uuid UUIDgenerate

get2 <- function(nm,env,parent=NULL){
    if(exists(nm,envir=env)) get(nm,envir=env)
    else if(is.environment(parent) && exists(nm,envir=parent)) get(nm,envir=parent)
}

str_ <- function(nm,envir,parent=NULL){
    res <- tryCatch(capture.output(str(get2(nm,envir,parent))),
                    error=function(e)e)
    if(inherits(res,"error")){
        res <- "<missing>"
    }
    res <- gsub("\\t","",res)
    htmlEscape(trimws(res))
}

#' A HTML version of 'ls.str()'
#'
#' This function is deprecated. Use \code{\link{env_browser}} instead.
#'
#' @param pos integer indicating \code{\link[base]{search}} path position, or -1 for the current environment.
#' @param name an optional name indicating search path position, see \code{\link[base]{ls}}.
#' @param envir the environment to look into
#' @param all.names logical value, if FALSE objects with names that start with a dot are ignored
#' @param pattern a character string, the pattern of the names of the objects to show
#' @param mode a character string, the mode of the objects to be shown
#' @seealso \code{\link{ls.str}}
#' @export
ls_str <- function(pos = -1, name, envir, all.names = FALSE, pattern, 
    mode = "any"){
    if (missing(envir)) 
        envir <- as.environment(pos)
    table <- env_browser_table(pos=pos,name=name,envir=envir,all.names=all.names,
                         pattern=pattern,mode=mode)
    html <- paste(
        "<div class='ls_str'>",
        table,
        "</div>",
        sep="\n")
    raw_html(html)
}


env_browser_css <- function(scoped=FALSE){
    env_browser_css <- readLines(system.file("css/env-browser.css",
                                                 package="RKernel"))
    env_browser_css <- paste0(env_browser_css,collapse="\n")
    if(scoped)
        paste("<style scoped=''>",env_browser_css,"</style>",sep="\n")
    else
        paste("<style>",env_browser_css,"</style>",sep="\n")
}

init_env_browser <- function(){
    raw_html(env_browser_css())
}

.env_browser_table <- new.env()
.env_browser_table$inited <- FALSE


env_browser_table <- function(pos = -1, name, envir, parent=NULL, all.names = FALSE, pattern = NULL, 
    mode = "any", id=NULL, include_css = FALSE){
    if (missing(envir)) 
        envir <- as.environment(pos)
    if(length(pattern))
        nms <- ls(name, envir = envir, all.names = all.names, pattern = pattern)
    else
        nms <- ls(name, envir = envir, all.names = all.names)
    r <- vapply(nms, exists, NA, envir = envir, mode = mode, 
        inherits = FALSE)
    nms <- nms[r]
    log_out(nms,use.print=TRUE)
    if(is.environment(parent)){
        # log_out("Yes, parent is an environment")
        if(length(pattern))
            nms_p <- ls(envir = parent, all.names = all.names, pattern = pattern)
        else
            nms_p <- ls(envir = parent, all.names = all.names)
        r_p <- vapply(nms_p, exists, NA, envir = parent, mode = mode, 
                    inherits = FALSE)
        nms_p <- nms_p[r_p]
        log_out(nms_p,use.print=TRUE)
        nms <- union(nms,nms_p)
    } # else log_out("No, parent isn't an environment")
        
    n <- length(nms)
    m <- matrix("",ncol=3,nrow=n)
    str_nms <- lapply(nms,str_,envir=envir,parent=parent)
    str_nms1 <- lapply(str_nms,"[[",1)
    str_nms2 <- lapply(str_nms,"[",-1)
    str_nms2 <- lapply(str_nms2,paste0,collapse="\n")
    m[,1] <- nms
    m[,2] <- unlist(str_nms1)
    m[,3] <- unlist(str_nms2)
    result <- NULL
    thead <- c("Name","Value")
    thead <- c(paste0("<th class='object-name border-left border-top'>",thead[1],"</th>"),
               paste0("<th class='object-summary border-left border-right border-top'>",thead[2],"</th>"))
    thead <- c("<thead>",thead,"</thead>")           
    thead <- paste0(thead,collapse="")
    thead <- paste0("<tr>",thead,"</tr>")
    thead <- c("<table class='env-browser env-browser-head'>",thead,"</table>")
    thead <- paste(thead,collapse="\n") 
    result <- append(result,thead)
    if(n > 0){
        for(i in 1:n){
            row <- m[i,]
            summary <- c(
                paste0("<td class='object-name border-left'><code>",row[1],"</code></td>"),
                # if(nzchar(row[3])) paste0("<td class='toggle-container'>","&plus;","</td>"),
                paste0("<td class='object-summary border-left border-right'><code>",row[2],"</code></td>")
            )
            summary <- paste(summary,collapse="")
            summary <- paste0("<tr>",summary,"</tr>")
            summary <- c("<table class='env-browser'>",summary,"</table>")
            summary <- paste(summary,collapse="\n")    
            if(nzchar(row[3])){  
                details <- unlist(strsplit(row[3],"\n",fixed=TRUE))
                details <- paste0("<td class='object-details border-left border-right'><code>",
                                  details,
                                  "</code></td>")
                details <- sapply(details,paste0,collapse="")
                details <- paste0("<tr>",details,"</tr>")
                details <- paste(details,collapse="\n")
                details <- c("<table class='env-browser'>",details,"</table>")
                details <- paste(details,collapse="\n")
                result_i <- c(
                    "<details>",
                    "<summary>",
                    summary,
                    "</summary>",
                    details,
                    "</details>"
                )
                result_i <- paste(result_i,collapse="\n")
                result <- append(result,result_i)
            } else {
                result <- append(result,summary)
            }
        }
    }
    if(missing(id))
        result <- c("<div class='env-browser-wrapper'>",
                    result,
                    "</div>")
    else
        result <- c(paste0("<div class='env-browser-wrapper' id='",id,"'>"),
                    result,
                    "</div>")
    html <- paste(result,collapse="\n")
    if(!.env_browser_table$inited || include_css){
        css <- env_browser_css()
        html <- paste(css,html,sep="\n")
        .env_browser_table$inited <- TRUE
    }
    html
}

