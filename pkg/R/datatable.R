html_table <- function(x,id=UUIDgenerate(),
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
    table <- c(paste0("<table id='",id,
                      "' class='",html_class,"'",
                      if(expand) " width='100%'" else "",
                      ">"),
               thead,tbody,"</table>")
    table <- paste0(table,collapse="\n")
    #raw_html(table,id=id)   
    table
}

data_table <- function(x,id=UUIDgenerate(),
                       html_class="display",
                       expand=FALSE,
                       use.rownames = TRUE,
                       searching = FALSE,
                       scrollY = 250,
                       paging = TRUE,
                       scrollX = TRUE,
                       scrollCollapse = TRUE,
                       # fixedColumns = 1,
                       ...
                       ){
    table <- html_table(x,id,use.rownames,html_class,expand)
    dt_args <- list(
        searching=searching,
        scrollY=scrollY,
        paging=paging,
        scrollX=scrollX,
        scrollCollapse=scrollCollapse,
        # fixedColumns=fixedColumns,
        ...
    )
    dt_args <- toJSON(dt_args,auto_unbox=TRUE)
    js <- sprintf(c(
"require([\"jquery\",\"datatables\"], function (jquery,datatables) {
    $(document).ready(function () {
        dt_args = %s;
        table = $('#%s').DataTable(dt_args);
    });
});"),dt_args,id)
    html <- c(
        "<div>",
        table,
        "<script type='text/javascript'>",
        js,
        "</script>",
        "</div>"
    )
    html <- paste(html,collapse="\n")
    raw_html(html,id=id)
}
