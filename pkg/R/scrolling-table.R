html_table <- function(x,id=uuid::UUIDgenerate(),
                       use.rownames=TRUE,
                       html_class="display", 
                       expand=FALSE){
    x <- as.data.frame(x)
    rn <- row.names(x)
    nms <- names(x)
    x <- format(x)
    x <- as.matrix(x)
    if(use.rownames){
        x <- cbind(rn,x)
        nms <- c("",nms)
    }
    tbody <- structure(paste("<td>",x,"</td>"),dim=dim(x))
    tbody <- apply(tbody,1,function(row) paste0(c("<tr>",row,"</tr>"),collapse=""))
    tbody <- c("<tbody>",tbody,"</tbody>")
    thead <- paste("<th>",nms,"</th>")
    thead <- paste0(c("<tr>",thead,"</tr>"),collapse="")
    thead <- c("<thead>",thead,"</thead>")
    table <- c(paste0("<table id='",id,"'",
                      " class='",html_class,"'",
                      if(expand) " width='100%'" else "",
                      ">"),
               thead,tbody,"</table>")
    table <- paste0(table,collapse="\n")
    raw_html(table,id=id)   
}

init_scrolling_table <- function(){

    scrolling_table_css <- readLines(system.file("css/scrolling-table.css",
                                                 package="RKernel"))
    scrolling_table_css <- paste0(scrolling_table_css,collapse="\n")
    scrolling_table_css <- paste("<style>",scrolling_table_css,"</style>",sep="\n")
    raw_html(scrolling_table_css)
}

scrolling_table <- function(x,id=uuid::UUIDgenerate(),
                       use.rownames=TRUE,
                       expand=FALSE,
                       html_class=NULL,
                       wrap_cells=NULL){
    x <- as.data.frame(x)
    rn <- row.names(x)
    nms <- names(x)
    x <- format(x)
    x <- as.matrix(x)
    if(use.rownames){
        x <- cbind(rn,x)
        nms <- c("",nms)
    }

    tbody <- array("",dim=dim(x))
    # tbody[,1] <- paste0("<td class='firstCol'>",x[,1],"</td>")
    tbody[,1] <- paste0("<th>",x[,1],"</td>")
    if(missing(wrap_cells)){
        tbody[,-1] <- paste0("<td>",x[,-1],"</td>")
    }
    else {
        tbody[,-1] <- paste0("<td>",wrap_cells[1],x[,-1],wrap_cells[2],"</td>")
    }
    tbody <- apply(tbody,1,function(row) paste0(c("<tr>",row,"</tr>"),collapse=""))
    tbody <- c("<tbody>",tbody,"</tbody>")
    if(use.rownames){
        thead <- c(paste0("<th class='firstCol'>",nms[1],"</th>"),
                   paste0("<th>",nms[-1],"</th>"))
    }
    else
        thead <- paste0("<th>",nms,"</th>")
    thead <- paste0(c("<tr>",thead,"</tr>"),collapse="")
    thead <- c("<thead>",thead,"</thead>")
    table <- c(paste0("<table id='",id,"'",
                      if(expand) " width='100%'" else "",
                      if(!missing(html_class)) paste0(" class='",html_class,"'")
                      else "",
                      ">"),
               thead,
               tbody,"</table>")
    table <- paste0(table,collapse="\n")
    html <- paste("<div class='tableFixHead'>",
                  table,
                  "</div>",
                  sep="\n")
    raw_html(html,id=id)   
}
