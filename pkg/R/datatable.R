html_table <- function(x,id=UUIDgenerate()){
    x <- as.data.frame(x)
    rn <- row.names(x)
    nms <- names(x)
    x <- format(x)
    x <- as.matrix(x)
    x <- cbind(rn,x)
    nms <- c("",nms)
    tbody <- structure(paste("<td>",x,"</td>"),dim=dim(x))
    tbody <- apply(tbody,1,function(row) paste0(c("<tr>",row,"</tr>"),collapse=""))
    tbody <- c("<tbody>",tbody,"</tbody>")
    thead <- paste("<th>",nms,"</th>")
    thead <- paste0(c("<tr>",thead,"</tr>"),collapse="")
    thead <- c("<thead>",thead,"</thead>")
    table <- c(paste0("<table id='",id,"' class='display'>"),thead,tbody,"</table>")
    table <- paste0(table,collapse="\n")
    #raw_html(table,id=id)   
    table
}

data_table <- function(x,id=UUIDgenerate()){
    table <- html_table(x,id)
    js <- sprintf(c(
"require([\"datatables\"], function (datatables) {
    $(document).ready(function () {
        table = $('#%s').DataTable({
                \"searching\":   false,
                \"scrollY\": 250,
                \"scrollCollapse\": true,
                });
    });
});"),id)
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
