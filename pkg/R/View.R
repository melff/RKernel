#' @export
View <- function(x,title) UseMethod("View")

#' @export
View.default <- function(x,title){
    x0 <- head(as.data.frame(x),n=getOption("View_max_rows",1000))
    x <- as.list(format.data.frame(x0))
    rn <- row.names(x0)
    if (any(rn != seq_along(rn))) 
        x <- c(list(row.names = rn), x)
    if (!is.list(x) || !length(x) || !all(sapply(x, is.atomic)) || 
        !max(lengths(x))) 
        stop("invalid 'x' argument")
    nms <- names(x)
    nms[1] <- sub("row.names","",nms[1])
    tbody <- do.call(cbind,x)
    tbody <- structure(paste("<td>",tbody,"</td>"),dim=dim(tbody))
    tbody <- apply(tbody,1,function(row) paste0(c("<tr>",row,"</tr>"),collapse=""))
    tbody <- c("<tbody>",tbody,"</tbody>")
    thead <- paste("<th style='position: sticky; top: 0; background-color: white;'>",nms,"</th>")
    thead <- paste0(c("<tr>",thead,"</tr>"),collapse="")
    thead <- c("<thead>",thead,"</thead>")
    table <- c("<table class='scroll'>",thead,tbody,"</table>")
    table <- c("<div class='grid-wrapper' style='height: 40em; background: white; overflow: scroll;'>",table,"</div>")
    # style <- c("<style>
    #            body { margin: 0}
    #             .scroll {
    #               border: 0;
    #               border-collapse: collapse;
    #             }
    # 
    #             .scroll td {
    #               padding: 3px;
    #               //border: 1px solid #aaa;
    #               padding: 6px;
    #               word-wrap: break-word;
    #             }
    # 
    #             .scroll thead th {
    #               //border: 1px solid #aaa;
    #               position: sticky;
    #               top: 0;
    #               left: 0;
    #               padding: 6px;
    #               background-color: white;
    #               //color: white;
    #             }
    # 
    #             .scroll tbody {
    #               width: 100%;
    #               overflow-y: auto;
    #               height: 40em;
    #             }
    #            </style>")
    # 
    # style <- paste0(style,collapse="\n")               
    table <- paste0(table,collapse="\n")               
    # raw_html(paste0(style,table,sep="\n"))               
    raw_html(table)
}


str_ <- function(nm,envir) chartr(r"(\)",r"(\\)",
          trimws(capture.output(str(get(nm,envir)))))

#' @export
ls_str <- function(pos = -1, name, envir, all.names = FALSE, pattern, 
    mode = "any"){
    if (missing(envir)) 
        envir <- as.environment(pos)
    nms <- ls(name, envir = envir, all.names = all.names, pattern = pattern)
    r <- vapply(nms, exists, NA, envir = envir, mode = mode, 
        inherits = FALSE)
    nms <- nms[r]
    res <- matrix("",ncol=2,nrow=length(nms))
    res[,1] <- nms
    str_nms <- sapply(nms,str_,envir=envir)
    res[,2] <- sapply(str_nms,"[",1)
    #tbody[] <- paste0("<td style='text-align: left; background-color: white; border: 1px solid #888888;'>",
    #                  "<code style='background-color: white;'>",
    #                  tbody,"</code></td>")
    tbody <- array(paste0("<td style='text-align: left;'>",
                      "<code style='background-color: unset;'>",
                      htmltools::htmlEscape(res),
                      "</code></td>"),dim=dim(res))
    tbody <- apply(tbody,1,function(row) paste0(c("<tr>",row,"</tr>"),collapse=""))
    tbody <- c("<tbody>",tbody,"</tbody>")
    thead <- paste0("<th style='text-align: left;'>",c("Name","Object summary"),"</th>")
    thead <- paste0(c("<tr>",thead,"</tr>"),collapse="")
    thead <- c("<thead>",thead,"</thead>")
    table <- c("<table style='width: 100%; table-layout: auto;'>",thead,tbody,"</table>")                   
    table <- paste0(table,collapse="\n")               
    raw_html(table)                   
}

HTML_SKELETON <-
'<!doctype html>
<html>
	<head>
		<meta charset="utf-8">
		%s
	</head>
	<body>
		%s
	</body>
</html>
'


#' @export
View.default <- function(x,title){
    x0 <- head(as.data.frame(x),n=getOption("View_max_rows",1000))
    x <- as.list(format.data.frame(x0))
    rn <- row.names(x0)
    if (any(rn != seq_along(rn))) 
        x <- c(list(row.names = rn), x)
    if (!is.list(x) || !length(x) || !all(sapply(x, is.atomic)) || 
        !max(lengths(x))) 
        stop("invalid 'x' argument")
    nms <- names(x)
    nms[1] <- sub("row.names","",nms[1])
    tbody <- do.call(cbind,x)
    tbody <- structure(paste("<td>",tbody,"</td>"),dim=dim(tbody))
    tbody <- apply(tbody,1,function(row) paste0(c("<tr>",row,"</tr>"),collapse=""))
    tbody <- c("<tbody>",tbody,"</tbody>")
    thead <- paste("<th style='position: sticky; top: 0; background-color: white;'>",nms,"</th>")
    thead <- paste0(c("<tr>",thead,"</tr>"),collapse="")
    thead <- c("<thead>",thead,"</thead>")
    table <- c("<table id='datatable'>",thead,tbody,"</table>")
    table <- paste0(table,collapse="\n")               

    hdr <- c('<script type="text/javascript" src="https://code.jquery.com/jquery-3.5.1.js"></script>',
             '<link rel="stylesheet" type="text/css" href="https://cdn.datatables.net/v/dt/dt-1.11.0/datatables.min.css"/>',
             '<script type="text/javascript" src="https://cdn.datatables.net/v/dt/dt-1.11.0/datatables.min.js"></script>')

    js <- "$(document).ready(function() {
           $('#datatable').DataTable({
                \"ordering\":   false,
                \"paging\":   false,
                \"searching\":   false,
                });
           } );"
    js <- paste0("<script>\n",js,"\n</script>")
    
    hdr <- paste0(hdr,collapse="\n")
    body <- paste0(c(table,js),collapse="\n")
    srcdoc <- htmlEscape(sprintf(HTML_SKELETON,hdr,body),attribute=TRUE)
    
    # text_html <- paste0(c(hdr,body),collapse="\n")

    text_html <- c(
        sprintf("<iframe srcdoc='%s'",srcdoc),
        "style='width: 100%; height: 798px;'",
        "frameborder='0' seamless>",
        "</iframe>")
    text_html <- paste0(text_html,collapse="\n")
    
    raw_html(text_html)
}
