#' @importFrom htmltools htmlEscape
#' @importFrom uuid UUIDgenerate

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
    html <- paste(
        "<div class='ls_str'>",
        table,
        "</div>",
        sep="\n")
    raw_html(html)
}


#' @export
browse_env_fixed <- function(pos = -1, name, envir, all.names = FALSE, pattern, 
    mode = "any", id="browse_env", tgl_id=UUIDgenerate(),update=FALSE){
    if (missing(envir)) 
        envir <- as.environment(pos)
    table <- env_browser_table(pos=pos,name=name,envir=envir,all.names=all.names,
                               pattern=pattern,mode=mode,id=id)
    # div <- c(paste0("<div class='browse_env' id='",id,"'>"),
    #          table,
    #          "</div>")
    div <- paste(table,collapse="\n")
    script_tmpl <- "
<script>
$( function() {
    $('#%s').detach()
         .appendTo('body')
         .css('position','fixed')
         .css('left','calc(100perc - 100px)')
         .css('top','130px')
         .css('max-height','unset')
         .resizable({
             handles: 'e, s, w'
             })
         .draggable({
             handle: 'thead'
         })
         .css('display','block')
         .css('background-color','#fff')
         .css('z-index','50')
         .css('opacity','1');
  });
</script>
"
    rm_script_tmpl <- "
<script>
$( function() {
    $('#%s').remove()
  });
</script>
"

    script <- sprintf(script_tmpl,id)
    script <- sub("perc","%",script)
    if(update){
        rm_script <- sprintf(rm_script_tmpl,id)
        res <- paste(rm_script,div,script,sep="\n")
    }
    else 
        res <- paste(div,script,sep="\n")
    raw_html(res,id=id,update=update)
}

#' @export
remove_env_browser <- function(id="browse_env"){
    script_tmpl <- "$( function(){
    $('#%s').remove()
})"
    script <- sprintf(script_tmpl,id)
    Javascript(script)
}

#' @export
browse_env <- function(pos = -1, name, envir, all.names = FALSE, pattern, 
    mode = "any", id=UUIDgenerate()){
    if (missing(envir)) 
        envir <- as.environment(pos)
    table <- env_browser_table(pos=pos,name=name,envir=envir,all.names=all.names,
                               pattern=pattern,mode=mode)
    css <- env_browser_css()
    html <- paste(css,table,sep="\n")
    d <- raw_html(html)
    payload <- list(source="page",
                    data=d$data,
                    start=1)
    structure(payload,class="payload")
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


env_browser_table <- function(pos = -1, name, envir, all.names = FALSE, pattern, 
    mode = "any", id=NULL, include_css = FALSE){
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

output_wrapper <- function(on=TRUE){
    if(on){
        js <- system.file("js/output-wrapper.js",
                                         package="RKernel")
    }
    else {
        js <- system.file("js/output-wrapper-off.js",
                                         package="RKernel")
    } 
    Javascript(file=js)
}



ls_str_refresh <- function(on=TRUE){
    if(on){
        ls_str_refresh.js <- system.file("js/ls-str-refresh.js",
                                         package="RKernel")
    }
    else {
        ls_str_refresh.js <- system.file("js/ls-str-refresh-off.js",
                                         package="RKernel")
    }
    Javascript(file=ls_str_refresh.js)
}


.browse_env_dialog <- new.env()
.browse_env_dialog$inited <- FALSE

browse_env_dialog <- function(pos = -1, name, envir, all.names = FALSE, pattern, 
                              mode = "any") {
    if (missing(envir)) 
        envir <- as.environment(pos)    
    dialog_tmpl <- "
<div id=\"env-browser-dialog\" title=\"Environment browser\">
%s
</div>
"

    script_tmpl <- "
<script>
$( function() {
    $( \"#env-browser-dialog\" ).dialog({
                 height: 400,
                 width: 900,
                 classes: {
                    \"ui-dialog\": \"env-browser-dialog\"
                 }
                 });
  });
</script>
"    
    if(!.browse_env_dialog$inited){
        auto_refresh_browse_env_dialog()
        .browse_env_dialog$inited <- TRUE
    }

    bt <- browser_table(pos=pos,name=name,envir=envir,all.names=all.names,
                                      pattern=pattern,mode=mode,include_css=TRUE,
                                      id="ui-dialog-env-browser-table")
    box_bt <- sprintf(dialog_tmpl,bt)
    script_bt <- sprintf(script_tmpl)
    html <- paste(box_bt,script_bt,sep="\n")
    raw_html(html)
}

auto_refresh_browse_env_dialog <- function(){
    refresh.js <- system.file("js/browse-env-dialog-refresh.js",
                              package="RKernel")
    refresh.js <- Javascript(file=refresh.js)
    kernel <- get_current_kernel()
    kernel$display_send(refresh.js)
}
