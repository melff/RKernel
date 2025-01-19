#' @importFrom utils tail file_test

#' @include httpd.R

asset_fetcher <- function(path,...){
    # log_out('asset_fetcher:',path)
    split_path <- strsplit(path,"/",fixed=TRUE)[[1]]
    # log_out(split_path, use.print = TRUE)
    pkgname <- split_path[4]
    basename <- tail(split_path,1)
    # log_out(basename, use.print = TRUE)
    # log_out(tail(split_path,-4), use.print = TRUE)
    file_path <- as.list(tail(split_path,-4))
    args <- c(file_path,list(package=pkgname,mustWork=TRUE))
    # log_out(args, use.print = TRUE)
    filename <- try(do.call("system.file",args))
    # log_out(sprintf("trying %s", filename))
    if(file_test("-f",filename)){
      split_basename <- strsplit(basename,".",fixed=TRUE)[[1]]
      filext <- tail(split_basename,1)
      mime_type <- switch(filext,
        "html"="text/html",
        "css"="text/css",
        "js"="application/javascript",
        "text/plain"
        )
      payload <- readLines(filename)
    #   log_out(sprintf('"%s" successfully loaded',filename))
      payload <- paste0(payload,collapse="\n")
      list(payload=payload,
         `content-type`=mime_type,
          headers=NULL,
          `status code`=200L)
    }
    else {
        # log_error(sprintf('"%s" not found',filename))
        list(payload=sprintf('"%s" not found',filename),
         `content-type`="text/plain",
          headers=NULL,
          `status code`=404L)
    }
}

dt_data <- new.env()

dt_data_fetcher <- function(path,query,postBody,headers){
    # log_out('data_fetcher:',path)
    # log_out('query:')
    # log_out(query,use.print=TRUE)
    # log_out('post body:')
    # log_out(postBody,use.print=TRUE)
    # log_out('headers:')
    # log_out(headers,use.str=TRUE)
    draw <- as.integer(postBody["draw"])
    start <- as.integer(postBody["start"])
    len <- as.integer(postBody["length"])
    split_path <- strsplit(path,"/",fixed=TRUE)[[1]]
    name <- split_path[4]
    obj <- get(name,envir=dt_data)
    if(len < 0) len <- nrow(obj)
    from <- start + 1
    to <- min(nrow(obj),start + len)
    ii <- seq(from=from,to=to)
    data <- obj[ii,,drop=FALSE]
    data <- cbind("row.names"=rownames(data),as.matrix(format(data)))
    # log_out(data,use.str=TRUE)
    payload <- to_json(list(
                     draw=draw,
                     recordsTotal=nrow(obj),
                     recordsFiltered=nrow(obj),
                     data=data
                     ),
                     pretty=TRUE)
    # log_out(payload, use.str=TRUE)
    list(payload=payload,
         `content-type`="application/json",
          headers="Access-Control-Allow-Origin: *",
          `status code`=200L)
}

fill_tmpl <- function(tmpl,...){
    substitutions <- c(...)
    res <- tmpl
    for(nm in names(substitutions)){
        pattern <- paste0("(( ",nm," ))")
        replacement <- substitutions[nm]
        res <- gsub(pattern,replacement,res,fixed=TRUE)
    }
    res
}

dt_head_tmpl <- '<link rel="stylesheet" type="text/css" href="(( url ))assets/RKernel/css/datatables.min.css">
<style>
table.dataTable tbody td {
    text-align: right;
}
table.dataTable thead th {
    text-align: center;
}
.datatable-wrapper {
    font-size: 14px;
    font-family: sans-serif;
}
.rownames {
    font-weight: bold;
    border-style-left: none;
}
div.dts div.dataTables_scrollBody {
    background: unset;
    background-color: rgba(250,250,250);
}
/* table.dataTable thead tr th:first-child,
table.dataTable thead tr td:first-child {
   border-left: 1px solid rgb(0,0,0,.15);
}*/
table.dataTable.cell-border tbody tr th:first-child,
table.dataTable.cell-border tbody tr td:first-child {
   border-left: none;
}
table.dataTable thead tr th,
table.dataTable thead tr td {
   /*border-top: 1px solid rgba(0,0,0,.15);*/
   border-right: 1px solid rgba(0,0,0,.15);
   border-bottom: 1px solid rgba(0,0,0,.15);
   background-color: rgba(250,250,250);
}
.dataTables_scroll {
  border-top: 1px solid rgba(0, 0, 0, 0.15);
  border-left: 1px solid rgba(0, 0, 0, 0.15);
  border-right: 1px solid rgba(0, 0, 0, 0.15);
}
.dataTables_scrollHead {
  background-color: rgb(250,250,250);
}
table.dataTable thead tr > .dtfc-fixed-left,
table.dataTable.cell-border tbody tr th:first-child,
table.dataTable.cell-border tbody tr td:first-child,
table.dataTable.cell-border tbody tr th:first-child,
table.dataTable.cell-border tbody tr td:first-child {
   background-color: rgb(250,250,250);
}
</style>
<script type="text/javascript" charset="utf8" 
        src="(( url ))assets/RKernel/js/jquery-3.6.0.min.js"></script>
<script type="text/javascript" charset="utf8" 
        src="(( url ))assets/RKernel/js/datatables.min.js"></script>'


dt_tmpl <- '<script type="text/javascript" charset="utf8">
    $(document).ready(function () {
        $("#(( id ))").DataTable({
            serverSide: true,
            processing: true,
            ordering: false,
            searching: false,
            scrollY: (( scrollY )),
            scrollCollapse: true,
            scrollX: true,
            fixedColumns: true,
            scroller: true,
            deferRender: true,
            ajax: {
              url: "(( url ))dt-data/(( name ))",
              type: "POST"
            },
            "columnDefs": [
                { className: "rownames", "targets": [ 0 ] }
             ],
        });
    });
</script>'

dt_table_tmpl <- '<div class="datatable-wrapper">
<table id="(( id ))" class="cell-border hover compact" style="width:auto;"><thead>
    <tr>
(( header ))
    </tr>
  </thead><tbody><tr><td>Loading... </td></tr></tbody></table>
</div>'

html_page_tmpl <- '<!DOCTYPE html>
<html>
<head>
(( head ))
</head>
<body>
(( table ))
(( script ))
</body>
</html>'

mk_tab_hdr <- function(obj){
    hdr <- c("",colnames(obj))
    hdr <- paste0("      <th>",hdr,"</th>")
    paste(hdr,collapse="\n")
}

datatable_page <- function(obj,
                         id=UUIDgenerate(),
                         scrollY=400,
                         size=50,
                         page_num=1){
    url <- httpd_url()
    if(!has_http_handler("assets"))
        add_http_handler("assets",asset_fetcher)
    if(!has_http_handler("dt-data"))
        add_http_handler("dt-data",dt_data_fetcher)
    n <- ncol(obj)
    m <- n%/%size
    r <- n%%size
    p0 <- page_num - 1
    from <- p0*size + 1
    to <- if(page_num > m) from - 1 + r else page_num*size
    ii <- seq(from=from,to=to)
    obj <- obj[ii]
    dt_data[[id]] <- obj
    code <- fill_tmpl(html_page_tmpl,
                        head=fill_tmpl(dt_head_tmpl,url=url),
                        table=fill_tmpl(dt_table_tmpl,id=id,header=mk_tab_hdr(obj)),
                        script=fill_tmpl(dt_tmpl,url=url,name=id,id=id,scrollY=scrollY)
                   )    
    code
}

#' @title HTML Tables with Interactive Controls
#'
#' @description Objects of class "dataTable" provide HTML tables with interactive
#'    controls powered by the DataTable Javascript library.
#' @name dataTable
NULL

#' @describeIn dataTable A dataTable constructor
#' @param x An object to be shown as a data table.
#' @param ... Other arguments passed to the initialization method of
#'   'dataTableClass' R6 objects
#' @export
dataTable <- function(x,...) UseMethod("dataTable")

#' @describeIn dataTable Default method
#' @export
dataTable.default <- function(x,...) dataTable.data.frame(as.data.frame(x),...)

#' @describeIn dataTable data.frame method 
#' @export
dataTable.data.frame <- function(x,...) 
    dataTableClass$new(x,...)

#' @rdname dataTable
#' @export
dataTableClass <- R6Class("dataTable",{
    public = list(
        #' @field w A container widget or NULL
        w = NULL,
        #' @field page Number of the current page
        page = 1,
        #' @field m Width of the object divided by 'size'
        m = 0,
        #' @field r Remainder of the widht of the object divided by 'size'
        r = 0,
        #' @field size Number of columns in each group for horizontal paging 
        size = 50,
        #' @field iframe An <iframe> container or NULL
        iframe = NULL,
        #' @field b_left Button to scroll left
        b_left = NULL,
        #' @field b_right Button to scroll right
        b_right = NULL,
        #' @field b_first Button to scroll to the first group of columns
        b_first = NULL,
        #' @field b_last Button to scroll to the last group of columns
        b_last = NULL,
        #' @field dt HTML code for the visible table
        dt = NULL,
        #' @field obj The tabular object being dispayed
        obj = NULL,
        #' @field label A string label that shows the columns being displayed
        label = NULL,
        #' @field style A string with CSS styling
        style = NULL,
        #' @field navigator A container widget that contains the navigator buttons
        navigator = NULL,
        #' @field scrollY The vertical scroll amount
        scrollY = NULL,
        #' @field height The height of the iframe
        height = NULL,
        # #' @field nlines_control A widget to control the number of rows being shown
        # nrows_control = NULL,
        # #' @field ncols_control A widget to control the number of cols being shown
        # ncols_control = NULL,
        #' @description
        #' Initialize the DataTable
        #' @param obj The object to be displayed
        #' @param size An integer, the number of columns pre-formatted on each page.
        #' @param nlines An integer, the approximate number of rows of each page
        #' @param ... Other arguments, ignored
        initialize = function(obj,
                              size=50,
                              nlines=min(nrow(obj),getOption("dataTable_lines",20)),
                              ...){
            self$size <- size
            self$m <- ncol(obj)%/%size
            self$r <- ncol(obj)%%size
            self$b_left <- Button(description="<")
            self$b_right <- Button(description=">")
            self$b_first <- Button(description="<<")
            self$b_last <- Button(description=">>")
            self$iframe <- HTML()
            # self$nrows_control <- BoundedIntText(value=nlines,
            #                                       min=1L,max=nrow(obj),
            #                                       description="Rows",
            #                                       description_tooltip = 
            #                                       "Number of rows being shown")
            # self$nrows_control$on_change(self$update_nlines)
            # self$ncols_control <- BoundedIntText(value=size,
            #                                     min=1L,max=ncol(obj),
            #                                     description="Columns",
            #                                     description_tooltip = 
            #                                    "Number of columns shown")
            if(ncol(obj) > size){
                self$label <- HTML()
                self$style <- HTML()
                self$style$value <- "<style>
div.datatable-navigation {
  align-self: center;
}
button.datatable-navigation-button {
  width: auto;
  height: auto;
  background: unset;
  padding: 0;
  line-height: 15px;
}
div.datatable-navigation div.widget-html-content {
  line-height: 15px;
}
</style>"
                self$b_left$add_class("datatable-navigation-button")
                self$b_right$add_class("datatable-navigation-button")
                self$b_first$add_class("datatable-navigation-button")
                self$b_last$add_class("datatable-navigation-button")

                self$b_left$on_click(self$page_left)
                self$b_right$on_click(self$page_right)
                self$b_first$on_click(self$page_first)
                self$b_last$on_click(self$page_last)

                self$navigator <- HBox(self$b_first,
                                       self$b_left,
                                       self$label,
                                       self$b_right,
                                       self$b_last)
                self$navigator$add_class("datatable-navigation")
                self$w <- VBox(self$style,
                               #self$nrows_control,
                               self$navigator,
                               self$iframe)
            }
            else {
                self$w <- self$iframe #VBox(self$nrows_control,self$iframe)
            }
            self$scrollY <- (nlines + 1) * 24 #+ nlines%/%2
            self$height <- paste0(self$scrollY + 64 + 12,"px")
            self$dt <- datatable_page(obj,size=size,scrollY=self$scrollY,...)
            self$obj <- obj
            self$draw()
            self$show_columns()
        },
        #' @description
        #' Show which columns are displayed
        show_columns = function(){
            from <- (self$page - 1)*self$size + 1
            if(self$page > self$m)
                to <- from + self$r
            else
                to <- self$page*self$size
            self$label$value <- sprintf("Columns %d-%d of %d",from,to,ncol(self$obj))
            invisible(NULL)
        },
        #' @description
        #' Go one page to the left
        page_left = function(){
            page <- max(1,self$page - 1)
            if(page < self$page){
                self$dt <- datatable_page(self$obj,size=self$size,page_num=page,scrollY=self$scrollY)
                self$draw()
                self$page <- page
                self$show_columns()
            }
            invisible(NULL)
        },
        #' @description
        #' Go one page to the right
        page_right = function(){
            page <- self$page + 1
            if(page > self$m){
                if(self$r > 0) page <- self$m + 1
                else page <- self$m
            }
            if(page > self$page){
                self$dt <- datatable_page(self$obj,size=self$size,page_num=page,scrollY=self$scrollY)
                self$draw()
                self$page <- page
                self$show_columns()
            }
            invisible(NULL)
        },
        #' @description
        #' Go to the first page (to the left)
        page_first = function(){
            page <- 1
            if(page < self$page){
                self$dt <- datatable_page(self$obj,size=self$size,page_num=page,scrollY=self$scrollY)
                self$draw()
                self$page <- page
                self$show_columns()
            }
            invisible(NULL)
        },
        #' @description
        #' Go to the last page (to the right)
        page_last = function(){
            if(self$r > 0) page <- self$m + 1
            else page <- self$m
            if(page > self$page){
                self$dt <- datatable_page(self$obj,size=self$size,page_num=page,scrollY=self$scrollY)
                self$draw()
                self$page <- page
                self$show_columns()
            }
            invisible(NULL)
        },
        draw = function() {
            self$iframe$value <- str2iframe(self$dt,
                                            #style="width:100%;height:100%;",
                                            height=self$height)
        },
        set_nlines = function(nlines) {
            self$scrollY <- (nlines + 1) * 24 #+ nlines%/%2
            self$height <- paste0(self$scrollY + 64 + 12,"px")
            self$dt <- datatable_page(self$obj,size=self$size,page_num=self$page,scrollY=self$scrollY)
            self$draw()
        },
        update_nlines = function(...){
            nlines <- self$nrows_control$value
            self$set_nlines(nlines)
        }
    )
})

#' @describeIn dataTable dataTable method for display_data
#' @param x A "dataTable" object
#' @param metadata A list of metadata strings
#' @param id An ID string
#' @param update A logical value; whether an existing display item will be updated
#' @export
display_data.dataTable <- function(x,...,
                                metadata=emptyNamedList,
                                id=attr(x,"id"),
                                update=FALSE){
    display_data(x$w,
            metadata=metadata,
            id=id,
            update=update)
}
