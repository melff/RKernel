#' @importFrom htmltools htmlEscape

str_ <- function(nm,envir){
    res <- capture.output(str(get(nm,envir)))
    res <- gsub("\\t","",res)
    htmlEscape(trimws(res))
}

#' @export
ls_str <- function(pos = -1, name, envir, all.names = FALSE, pattern, 
    mode = "any"){
    if (missing(envir)) 
        envir <- as.environment(pos)
    table <- env_browser_table(pos=pos,name=name,envir=envir,all.names=all.names,
                         pattern=pattern,mode=mode)
    raw_html(table)
}


#' @export
browse_env_fixed <- function(pos = -1, name, envir, all.names = FALSE, pattern, 
    mode = "any", id=uuid::UUIDgenerate(), tgl_id=uuid::UUIDgenerate()){
    if (missing(envir)) 
        envir <- as.environment(pos)
    table <- env_browser_table(pos=pos,name=name,envir=envir,all.names=all.names,
                               pattern=pattern,mode=mode)
    toggle <- paste0(paste0("<button data-toggle=\"collapse\" ",
                             "type=\"button\"",
                             "data-target=\"#",tgl_id,"\">"),
                      "&plus;",
                      "</button>")
    div <- c(paste0("<div class='browse_env' id='",id,"'>"),
             toggle,
             paste0("<div class='collapse in' id='",tgl_id,"'>"),
             table,
             "</div>",
             "</div>")
    div <- paste(div,collapse="\n")
    script_tmpl <- "
<script>
$( function() {
    $('#%s').detach()
         .appendTo('body')
         .css('position','fixed')
         /*.css('left','0')
         .css('bottom','0')*/
         .draggable()
         /*.resizable()*/
         .css('display','block')
         /*.css('background-color','#fff')*/
         .css('z-index','100')
         .css('opacity','1');
  });
</script>
"
    script <- sprintf(script_tmpl,id)
    res <- paste(div,script,sep="\n")
    raw_html(res,id=id)
}

browse_env <- function(pos = -1, name, envir, all.names = FALSE, pattern, 
    mode = "any", id=uuid::UUIDgenerate()){
    if (missing(envir)) 
        envir <- as.environment(pos)
    table <- env_browser_table(pos=pos,name=name,envir=envir,all.names=all.names,
                               pattern=pattern,mode=mode)
    d <- raw_html(table)
    payload <- list(source="page",
                    data=d$data,
                    start=1)
    structure(payload,class="payload")
}


init_env_browser <- function(){

    env_browser_css <- readLines(system.file("css/env-browser.css",
                                                 package="RKernel"))
    env_browser_css <- paste0(env_browser_css,collapse="\n")
    env_browser_css <- paste("<style>",env_browser_css,"</style>",sep="\n")
    raw_html(env_browser_css)
}

env_browser_table <- function(pos = -1, name, envir, all.names = FALSE, pattern, 
    mode = "any"){
    if (missing(envir)) 
        envir <- as.environment(pos)
    nms <- ls(name, envir = envir, all.names = all.names, pattern = pattern)
    r <- vapply(nms, exists, NA, envir = envir, mode = mode, 
        inherits = FALSE)
    nms <- nms[r]
    n <- length(nms)
    m <- matrix("",ncol=3,nrow=n)
    str_nms <- sapply(nms,str_,envir=envir)
    str_nms1 <- sapply(str_nms,"[[",1)
    str_nms2 <- sapply(str_nms,"[",-1)
    str_nms2 <- sapply(str_nms2,paste0,collapse="\n")
    m[,1] <- nms
    m[,2] <- str_nms1
    m[,3] <- str_nms2
    result <- c()
    thead <- c("Name","Value")
    thead <- c(paste0("<th class='object-name border-left border-top'>",thead[1],"</th>"),
               paste0("<th class='object-summary border-left border-right border-top'>",thead[2],"</th>"))
    thead <- c("<thead>",thead,"</thead>")           
    thead <- paste0(thead,collapse="")
    thead <- paste0("<tr>",thead,"</tr>")
    thead <- c("<table class='env-browser env-browser-head'>",thead,"</table>")
    thead <- paste(thead,collapse="\n") 
    result <- append(result,thead)
    for(i in 1:nrow(m)){
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
            details <- unlist(strsplit(row[3],"\n"))
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
    result <- c("<div class='env-browser-wrapper'>",result,"</div>")
    paste(result,collapse="\n")
}

