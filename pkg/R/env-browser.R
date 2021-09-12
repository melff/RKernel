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
    tbody <- c()
    for(i in 1:nrow(m)){
        row <- m[i,]
        if(nzchar(row[3])){  
            tgl_id <- uuid::UUIDgenerate()
            toggler <- paste0(paste0("<button data-toggle=\"collapse\" ",
                                     "type=\"button\"",
                                     "data-target=\"#",tgl_id,"\">"),
                        "&plus;",
                        #"<i class=\"fa fa-angle-double-right\"></i>",
                        #"<i class=\"fa fa-chevron-right\"></i>",
                        "</button>")
            rows_out <- c(
                paste0("<td class='border-left'><code>",row[1],"</code></td>"),
                paste0("<td class=\"toggle-container\">",toggler,"</td>"),
                paste0("<td class='border-left border-right'><code>",row[2],"</code></td>")
            )
            rows_out <- paste(rows_out,collapse="")
            tbody <- append(tbody,"<tbody>")
            tbody <- append(tbody,paste0("<tr>",rows_out,"</tr>"))
            tbody <- append(tbody,"</tbody>")
            tbody <- append(tbody,paste0("<tbody class=\"collapse subtable\" id=\"",tgl_id,"\">"))
            rows_out <- unlist(strsplit(row[3],"\n"))
            rows_out <- paste0("<td colspan='3' class='border-left border-right'><code>",
                               rows_out,
                               "</code></td>")
            rows_out <- sapply(rows_out,paste0,collapse="")
            st_body <- paste0("<tr>",rows_out,"</tr>")
            st_body <- paste(st_body,collapse="\n")
            tbody <- append(tbody,st_body)
            tbody <- append(tbody,"</tbody>")
        }
        else {
            rows_out <- c(
                paste0("<td class='border-left'><code>",row[1],"</code></td>"),
                "<td></td>",
                paste0("<td class='border-left border-right'><code>",row[2],"</code></td>")
            )
            rows_out <- paste(rows_out,collapse="")
            tbody <- append(tbody,"<tbody>")
            tbody <- append(tbody,paste0("<tr>",rows_out,"</tr>"))
            tbody <- append(tbody,"</tbody>")
        }
    }
    tbody <- unlist(tbody)
    tbody <- tbody[nzchar(tbody)]
    thead <- c("Name","Value")
    thead <- c(paste0("<th class='border-left' colspan='2'>",thead[1],"</th>"),
               paste0("<th class='border-left border-right'>",thead[2],"</th>"))
    thead <- paste0(thead,collapse="")
    thead <- paste0("<tr>",thead,"</tr>")
    colgroup <- c("<col class='object-name-col'>",
                  "<col class='toggle-col'>",
                  "<col class='object-value-col'>")
    table <- c("<div class='env-browser-wrapper'>",
               "<table class='env-browser'>",
               "<colgroup>",
               colgroup,
               "</colgroup>",
               "<thead>",
               thead,
               "</thead>",
               tbody,
               "</table>",
               "</div>"
              )
    table <- paste0(table,collapse="\n")
    raw_html(table)
}


init_env_browser <- function(){

    env_browser_css <- readLines(system.file("css/env-browser.css",
                                                 package="RKernel"))
    env_browser_css <- paste0(env_browser_css,collapse="\n")
    env_browser_css <- paste("<style>",env_browser_css,"</style>",sep="\n")
    raw_html(env_browser_css)
}
