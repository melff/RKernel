annot_details <- function(x) {
    annot <- x$annotation
    if(length(annot)) {
        descr <- annot$description
        if(!length(descr)) descr <- "[Annotations]"
        annot$description <- NULL
        n <- length(annot)
        if(n > 0) {
            attl <- sprintf("<summary>%s</summary>",descr)
            atab <- paste0("<tr><td>", names(annot) ,":</td><td>", unlist(annot) , "</td></tr>",sep="")
            atab_colspec <- c("<colgroup>",
                     "<col class=\"annot-name\">",
                     "<col class=\"annot-value\">",
                     "</colgroup>")
            atab <- paste(c("<table class=\"codeplan-details\">" ,
                            atab_colspec, atab, "</table>"),collapse="\n")
            res <- paste(c(
                "<details>",
                attl,
                atab,
                "</details>"), 
                collapse="\n")
        } 
        else {
            res <- descr
        }
        sprintf("<td>%s</td>",res)
    } else  {
        "<td></td>"
    }
}

lab_details <- function(x) {
    labs <- x$labels  
    if(length(labs)) {
        n <- length(labs)
        sumry <- sprintf("<summary>Value labels: %d</summary>",n)
        colspec <- c("<colgroup>",
                     "<col class=\"vallab-label\">",
                     "<col class=\"vallab-value\">",
                     "</colgroup>")
        ltab <- paste0("<tr><td class=\"vallab-label\">", names(labs) ,":</td><td class=\"vallab-value\">", unlist(labs) , "</td></tr>",sep="")
        ltab <- paste(c("<table class=\"codeplan-details\">" , colspec, ltab, "</table>"),collapse="\n")
        ltab <- paste(c(
            "<details>",
            sumry,
            ltab,
            "</details>"), 
            collapse="\n")
        sprintf("<td>%s</td>",ltab)
    } else 
        "<td></td>"
    
}

vfilter_details <- function(x) {
    vfilter <- x$value.filter  
    if(length(vfilter)) {
        if(vfilter$class=="missing.values") {
            if(length(vfilter$values)) {
                sumry <- sprintf("Missing values: %d",length(vfilter$values))
            } else if(length(vfilter$range)) {
                sumry <- "Missing range"
            } else  sumry <- ""
        } else if(vfilter$class=="valid.values") {
            sumry <- sprintf("Valid values: %d",length(vfilter$values))
        } else if(vfilter$class=="missing.range") {
            sumry <- "Missing range"
        } else if(vfilter$class=="valid.range") {
            sumry <- "Valid range"
        }
        sumry <- sprintf("<summary>%s</summary>",sumry)
        vftab <- NULL
        if(length(vfilter$values)){
            vftab <- c(vftab, vfilter$values)
        }
        if(length(vfilter$range)){
            vftab <- c(vftab, sprintf("%s\u2013%s",
                                      vfilter$range[1],
                                      vfilter$range[2]
                                      ))
        }
        if(length(vftab)) {
            vftab <- c("<table class=\"codeplan-details\">",paste0("<tr><td class=\"vfilter-value\">",vftab,"</td></tr>"),"</table>")
        }
        vftab <- paste(c(   
            "<details>",
            sumry,
            vftab,
            "</details>"), 
            collapse="\n")
        sprintf("<td>%s</td>",vftab)
    } else 
        "<td></td>"
}

codeplan_css <- function(scoped = FALSE) {
    css <- readLines(system.file("css/codeplan.css", package="RKernel"))
    # css <- readLines("RKernel/pkg/inst/css/codeplan.css")
    css <- paste0(css,collapse="\n")
    if(scoped)
        paste("<style scoped=''>",css,"</style>",sep="\n")
    else
        paste("<style>",css,"</style>",sep="\n")
}

codeplan_html <- function(x) {
    css <- codeplan_css(scoped=FALSE)
    start <- c("<div class=\"codeplan-wrapper\">","<table class=\"codeplan-table\">")
    colspec <- c(
        "<colgroup>",
        "<col class=\"codeplan-varname\">",
        "<col class=\"codeplan-desc\">",
        "<col class=\"codeplan-labels\">",
        "<col class=\"codeplan-vfilter\">",
        "<col class=\"codeplan-measurement\">",
        "<col class=\"codeplan-mode\">",
        "</colgroup>"
    )
    end <- c("</table>","</div>")
    make_html_row <- function(i) {
        name <- names(x)[i]
        x_i <- x[[i]]
        res <- sprintf("<td>%s</td>",name)
        
        res <- c(res, annot_details(x_i))
        res <- c(res, lab_details(x_i))
        res <- c(res, vfilter_details(x_i))
        
        if(length(x_i$measurement)) {
            res <- c(res, sprintf("<td>%s</td>",x_i$measurement))
        } else 
            res <- c(res, "<td></td>")
        
        if(length(x_i$mode)) {
            res <- c(res, sprintf("<td>%s</td>",x_i$mode))
        } else 
            res <- c(res, "<td></td>")

        paste(c("<tr>", res, "</tr>"),collapse=" ")
    }
    res <- unlist(lapply(seq_along(x), make_html_row))
    res <- c("<thead>",
             "<tr>",
             "<th>Name</th>",
             "<th>Description</th>",
             "<th>Labels</th>",
             "<th>Value filter</th>",
             "<th>Measurement</th>",
             "<th>Mode</th>",
             "</tr>",
             "</thead>",
             "<tbody>",
             res,
             "<tbody>"
             )
    paste(c(css,start,colspec,res,end),collapse="\n")
}

#' @export
display_data.codeplan <- function(x,...) {
    display_data(
        "text/plain" = capture.output(print(x)),
        "text/html" = codeplan_html(x)
    )
}
