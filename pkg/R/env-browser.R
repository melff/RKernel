#' @importFrom htmltools htmlEscape
#' @importFrom uuid UUIDgenerate

get2 <- function(nm,env,parent=NULL){
    if(exists(nm,envir=env)) get(nm,envir=env)
    else if(is.environment(parent) && exists(nm,envir=parent)) get(nm,envir=parent)
}

#' @importFrom utils str

str_str <- function(nm,envir,parent=NULL){
    res <- tryCatch(capture.output(str(get2(nm,envir,parent))),
                    error=function(e)e)
    if(inherits(res,"error")){
        res <- "<missing>"
    }
    res <- gsub("\\t","",res)
    htmlEscape(trimws(res))
}

#' @title A HTML version of 'ls.str()'
#'
#' @description This function is now just an alias for \code{\link{envBrowser}}.
#'
#' @param pos integer indicating \code{\link[base]{search}} path position, or -1 for the current environment.
#' @param name an optional name indicating search path position, see \code{\link[base]{ls}}.
#' @param envir the environment to look into
#' @param all.names logical value, if FALSE objects with names that start with a dot are ignored
#' @param pattern a character string, the pattern of the names of the objects to show
#' @param mode a character string, the mode of the objects to be shown
#' @seealso \code{\link{ls.str}}
#' @export
ls_str <- function(pos = -1, name = NULL, envir, all.names = FALSE, pattern = NULL, 
    mode = "any"){
    if (missing(envir)) 
        envir <- as.environment(pos)
    parent <- parent.frame()
    envBrowserClass$new(name=name,
                        envir=envir,
                        parent=parent,
                        all.names=all.names,
                        if(!missing(pattern))pattern=pattern,
                        mode=mode)
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


env_matrix <- function(pos = -1, name, envir, parent=NULL, all.names = FALSE, pattern = NULL, 
    mode = "any") {
    # log_out("env_matrix")
    if (missing(envir)) 
        envir <- as.environment(pos)
    if(length(pattern))
        nms <- ls(name, envir = envir, all.names = all.names, pattern = pattern)
    else
        nms <- ls(name, envir = envir, all.names = all.names)
    r <- vapply(nms, exists, NA, envir = envir, mode = mode, 
        inherits = FALSE)
    nms <- nms[r]
    # log_out(nms,use.print=TRUE)
    # log_out(ls.str(),use.print=TRUE)
    if(is.environment(parent)){
        # log_out("Yes, parent is an environment")
        if(length(pattern))
            nms_p <- ls(envir = parent, all.names = all.names, pattern = pattern)
        else
            nms_p <- ls(envir = parent, all.names = all.names)
        r_p <- vapply(nms_p, exists, NA, envir = parent, mode = mode, 
                    inherits = FALSE)
        nms_p <- nms_p[r_p]
        # log_out(nms_p,use.print=TRUE)
        nms <- union(nms,nms_p)
    } # else log_out("No, parent isn't an environment")
        
    n <- length(nms)
    m <- matrix("",ncol=3,nrow=n)
    str_nms <- lapply(nms,str_str,envir=envir,parent=parent)
    str_nms1 <- lapply(str_nms,"[[",1)
    str_nms2 <- lapply(str_nms,"[",-1)
    str_nms2 <- lapply(str_nms2,paste0,collapse="\n")
    m[,1] <- nms
    m[,2] <- unlist(str_nms1)
    m[,3] <- unlist(str_nms2)
    list(m=m, n=n)
}

session_env_matrix <- function(repl, 
                               pos = -1L,
                               envname = NULL) {
    cmd <- if(!missing(envname)) {
               sprintf("RKernel:::env_matrix(envir = %s)", envname)
           } 
           else {
               sprintf("RKernel:::env_matrix(pos = %d)", pos)
           }
    repl$eval_code(cmd)
}

env_browser_table <- function(pos = -1, name, envir, parent=NULL, all.names = FALSE, pattern = NULL, 
    mode = "any", id=NULL, repl = NULL, title = NULL){
    if(inherits(repl, "RSessionAdapter")) {
        if(!missing(name)) {
            em <- session_env_matrix(repl=repl,envname=name)
        }
        else {
            em <- session_env_matrix(repl=repl,pos=pos)
        }
    }
    else {
        em <- env_matrix(pos, name, envir, parent, all.names, pattern, mode)
    }
    m <- em$m
    n <- em$n
    result <- NULL
    thead <- c("Name","Value")
    thead <- c(paste0("<th class='object-name border-left border-top'>",thead[1],"</th>"),
               paste0("<th class='object-summary border-left border-right border-top'>",thead[2],"</th>"))
    thead <- paste0(thead,collapse="")
    if(is.character(title)) {
        title <- paste0("<th class='border-left border-right border-top no-border-bottom' colspan=\"2\">",title,"</th>")
        thead <- c(title,thead)
    }
    thead <- paste0("<tr>",thead,"</tr>")
    thead <- c("<thead>",thead,"</thead>")           
    thead <- c("<table class='env-browser env-browser-head'>",thead,"</table>")
    thead <- paste(thead,collapse="\n") 
    result <- append(result,thead)
    if(n > 0){
        for(i in 1:n){
            row <- m[i,]
            summary <- c(
                paste0("<td class='object-name border-left'><pre>",
                        row[1],
                        "</pre></td>"),
                # if(nzchar(row[3])) paste0("<td class='toggle-container'>","&plus;","</td>"),
                paste0("<td class='object-summary border-left border-right'><pre>",
                        row[2],
                        "</pre></td>")
            )
            summary <- paste(summary,collapse="")
            summary <- paste0("<tr>",summary,"</tr>")
            summary <- c("<table class='env-browser'>",summary,"</table>")
            summary <- paste(summary,collapse="\n")    
            if(nzchar(row[3])){  
                details <- unlist(strsplit(row[3],"\n",fixed=TRUE))
                details <- paste0("<td class='object-details border-left border-right'><pre>",
                             details,
                             "</pre></td>")
                details <- sapply(details,paste0,collapse="")
                details <- paste0("<tr>",
                                #   "<td class='object-name border-left'></td>",
                                  details,
                                  "</tr>")
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
    css <- env_browser_css()
    html <- paste(css,html,sep="\n")
    }
    html
}

