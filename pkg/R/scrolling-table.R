#' @importFrom uuid UUIDgenerate

grid_table <- function(x,id=UUIDgenerate(),
                       use.rownames=TRUE,
                       html_class="gridTable", 
                       expand=FALSE,
                       include_css=TRUE){
    x <- as.data.frame(x)
    rn <- row.names(x)
    nms <- names(x)
    x <- format(x)
    x <- as.matrix(x)
    tbody <- structure(paste("<td>",x,"</td>"),dim=dim(x))
    if(use.rownames){
        firstCol <- paste0("<th>",rn,"</th>")
        tbody <- cbind(firstCol,tbody)
        nms <- c("",nms)
    }
    tbody <- apply(tbody,1,function(row) paste0(c("<tr>",row,"</tr>"),collapse=""))
    tbody <- c("<tbody>",tbody,"</tbody>")
    thead <- paste0("<th>",nms,"</th>")
    thead <- paste0(c("<tr>",thead,"</tr>"),collapse="")
    thead <- c("<thead>",thead,"</thead>")
    table <- c(paste0("<table id='",id,"'",
                      " class='",html_class,"'",
                      if(expand) " width='100%'" else "",
                      ">"),
               thead,tbody,"</table>")
    html <- paste0(table,collapse="\n")
    grid_table_css <- readLines(system.file("css/grid-table.css",
                                            package="RKernel"))
    grid_table_css <- paste0(grid_table_css,collapse="\n")
    grid_table_css <- paste("<style>",grid_table_css,"</style>",sep="\n")
    html <- paste(grid_table_css,html,sep="\n")
    raw_html(html,id=id)   
}

scrolling_table <- function(x,id=UUIDgenerate(),
                       use.rownames=TRUE,
                       expand=FALSE,
                       html_class=NULL,
                       wrap_cells=NULL,
                       max_lines=getOption("view_max_lines", 100),
                       max_columns=getOption("view_max_columns",20)){
    
    x <- prep_data_frame(x,
                         max_lines = max_lines,
                         max_columns = max_columns,
                         use.rownames = use.rownames)
    nms <- x[1,]
    x <- x[-1,]

    tbody <- array("",dim=dim(x))
    # tbody[,1] <- paste0("<td class='firstCol'>",x[,1],"</td>")
    tbody[,1] <- paste0("<th>",x[,1],"</th>")
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
    scrolling_table_css <- readLines(system.file("css/scrolling-table.css",
                                                    package="RKernel"))
    scrolling_table_css <- paste0(scrolling_table_css,collapse="\n")
    scrolling_table_css <- paste("<style>",scrolling_table_css,"</style>",sep="\n")
    html <- paste(scrolling_table_css,html,sep="\n")
    html <- paste(
        "<div style='margin: 1px auto; display: table; padding: 3px;' >",
        html,
        "</div>",
        sep="\n"
    )
    raw_html(html,id=id)   
}

