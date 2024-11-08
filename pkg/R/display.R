#' @include json.R

#' @name display
#' @title Display an R Object
#'
#' @description Sends a 'display_data' message to the frontend. 
#'  Allows users to create rich display of R objects.
#'
#' @param ... Arguments passed to 'display_data' methods
#' 
#' @export
display <- function(...){
    # log_out("=== DISPLAY ===")
    d <- display_data(...)
    # log_out(paste("Class:",class(d)))
    # log_out(paste("ID:",display_id(d)))
    # log_out(d,use.str=TRUE)
    msg <- list(type=class(d),
              content=unclass(d))
    msg_send(msg)
}

#' @export
print.display_data <- function(x,...) display(x,...)

#' Prepare an R Object for Being Displayed
#'
#' @description A generic function that prepares R objects for display using \code{display()}
#'
#' @param x An object to be prepared for display.
#' @param ... Optional arguments tagged by mime types with mime data
#' @return An object of class "display_data"
#' 
#' @export
display_data <- function(x,...) {
    UseMethod("display_data")
}

#' @describeIn display_data Default method
#'
#' @param x An object
#' @param ... Optional arguments tagged by mime types with mime data
#' @param data A list with named elements, containing mime data
#' @param metadata A list with named elements, containing metadata
#' @param id An identifier string
#' @param update A logical value, whether a new display item should be created
#'               or an existing one should be updated
#' 
#' @importFrom repr mime2repr
#' @importFrom uuid UUIDgenerate
#' @export
display_data.default <- function(x,...,
                                 data,
                                 metadata=emptyNamedList,
                                 id=UUIDgenerate(),
                                 update=FALSE){

    if(missing(x)){
        if(missing(data))
            d <- list(data=list(...))
        else
            d <- list(data=data)
    } 
    else if(length(dim(x))==2) {
        x <- as.data.frame(x)
        return(display_data(x))
    }
    else {
        rkernel_mime_types <- getOption("rkernel_mime_types",
                                        c("text/plain",
                                          "text/html",
                                          "text/latex",
                                          "text/markdown"))
        mime_data <- list()
        for(mime_type in rkernel_mime_types){
            repr_func <- mime2repr[[mime_type]]
            repr_result <- repr_func(x,...)
            mime_data[[mime_type]] <- repr_result
        }
        d <- list(data=mime_data)
    }
    if(!length(metadata))
        metadata <- emptyNamedList
    d$metadata <- metadata
    d$transient <- list(display_id=id)
    if(update) cl <- "update_display_data"
    else cl <- "display_data"
    structure(d,class=cl)
}


#' @describeIn display_data S3 method for html widgets
#' @importFrom uuid UUIDgenerate
#' @importFrom htmltools htmlEscape
#' @importFrom digest digest
#' @importFrom base64enc dataURI
#' @importFrom htmlwidgets saveWidget
#' @importFrom utils file_test
#' @export
display_data.htmlwidget <- function(x,...,
                            metadata=emptyNamedList,
                            id=UUIDgenerate(),
                            update=FALSE){

    path <- getOption("htmlwidgets_path","htmlwidgets")
    use_tmpdir <- getOption("htmlwidgets_tmpdir",FALSE)
    embed <- getOption("htmlwidgets_embed",FALSE)
    iframe <- getOption("htmlwidgets_iframe",TRUE) && length(path) || embed
    selfcontained <- getOption("htmlwidgets_selfcontained",TRUE) || embed
    assets <- getOption("htmlwigets_assets",NULL)
    show_button <- getOption("htmlwidgets_showbutton",FALSE)

    if(use_tmpdir) {
        htmlfile <- tempfile(pattern = "widget",fileext = ".html")
        url <- htmlfile
        file_url <- paste0("file://",htmlfile)
        iframe <- FALSE
        show_button <- FALSE
    }
    else { 
        url <- path
        hash <- digest(as.character(x))
        htmlfile <- paste0(hash,".html")
        url <- paste(url,htmlfile,sep="/")
        htmlfile <- file.path(path,htmlfile)
        if(length(assets))
            assets <- file.path(path,assets)
        # path <- file.path(getwd(),path)
        if(!file_test("-d",path))
            dir.create(path)
        file_url <- paste0("file://",getwd(),"/",htmlfile)
    }

    saveWidget(x,
               file=htmlfile,
               selfcontained=selfcontained,
               libdir=assets
               )
    if(iframe){
        if_tmpl <- "
            <iframe src=\"%s\" width=\"%s\" height=\"%s\" class='html-widget-iframe'  style=\"border-style:none\">
            </iframe>
            "
        width <- getOption("htmlwidget_iframe_width","100%")
        height <- getOption("htmlwidget_iframe_height",600L)
        if(embed)
            url <- dataURI(mime="text/html",file=htmlfile)
        iframe <- sprintf(if_tmpl,url,width,height)
        res <- iframe
    }
    else {
        if(!show_button)
            res <- sprintf("Use <a href=\"%s\" target=\"_blank\">this link</a> to view widget",url)
        else 
            res <- ""
    }
    if(show_button){
        button_tmpl <- '
        <form class="help-popout-button" action="%s" method="get" target="_blank">
                                                                      <button type="submit">Open in new tab</button>
                                                                                                                 </form>'
        button <- sprintf(button_tmpl,url)
        res <- c(res,button)
    }
    text_html <- paste(res,collapse="\n")
    
    mime_data <- list(
        "text/plain"="",
        "text/html"=text_html
    )

    d <- list(data=mime_data)
    d$metadata <- metadata
    d$transient <- list(display_id=id)
    if(update) cl <- "update_display_data"
    else cl <- "display_data"
    structure(d,class=cl)
}

#' @describeIn display_data S3 method for "display_data" objects
#' @export
display_data.display_data <- function(x,...) x

#' @describeIn display_data S3 method for "update_display_data" objects 
#' @export
display_data.update_display_data <- function(x,...) x


#' Get the id of an object display
#'
#' @description This function returns the id of an object created by
#'     \code{display_data()} or \code{update_display_data()}.
#' 
#' @param x An object of class "display_data" or "update_display_data"
#' @return a character string with the id.
#' 
#' @export
display_id <- function(x) UseMethod("display_id")

#' @describeIn display_id S3 method for "display_data" objects
#' @export
display_id.display_data <- function(x) x$transient$display_id

#' @describeIn display_id S3 method for "update_display_data" objects
#' @export
display_id.update_display_data <- function(x) x$transient$display_id

#' @describeIn display_data "update" method for "display_data" objects
#' @param object An object of class "display_data"
#' @export
update.display_data <- function(object,...){
    id <- display_id(object)
    display_data(...,id=id,update=TRUE)
}

#' Display an object using the Jupyter notebook pager
#'
#' @description This function allows to display an \emph{R} object in the pager
#'     of a Jupyter notebook. Note that acts like \code{display()} when Jupyter
#'     Lab is used.
#'
#' @param x An object to be displayed in the Notebook pager
#' @param ... Other arguments, ignored or passed to specific methods.
#' @export
Page <- function(x,...) UseMethod("Page")

#' @describeIn Page S3 default method -- calls \code{display_data} and marks it as pager payload
#' @param start Integer, where to start the output.
#' @export
Page.default <- function(x,start=1,...){
    if(missing(x)){
        data <- list(...)
    } 
    else {
        displayed <- display_data(x=x,...)
        data <- displayed$data
    }
    p <- list(source="page",
              data=data,
              start=start)
    structure(p,class="payload")
}

#' Add a Class to the 'Displayed' Ones
#'
#' @description Add a class to those who are output using \code{display()} when
#'     they are autoprinted, i.e. returned as the value of the last expression
#'     in a Jupyter notebook cell.
#'
#' @param x A character string, the name of a class.
#' @export
add_displayed_classes <- function(x){
    if(is.character(x)) classes <- x
    else classes <- class(x)
    if(length(classes)){
        displayed_classes <- getOption("rkernel_displayed_classes")
        options(rkernel_displayed_classes=union(displayed_classes,classes))
    }
}

#' Remove a Class to the 'Displayed' Ones
#'
#' @description Remove a class from those who are output using \code{display()} when
#'     they are autoprinted, i.e. returned as the value of the last expression
#'     in a Jupyter notebook cell.
#'
#' @param x A character string, the name of a class.
#' @export
remove_displayed_classes <- function(x){
    if(is.character(x)) classes <- x
    else classes <- class(x)
    if(length(classes)){
        displayed_classes <- getOption("rkernel_displayed_classes")
        options(rkernel_displayed_classes=setdiff(displayed_classes,classes))
    }
}

#' Add a Class to the 'Paged' Ones
#'
#' @description Add a class to those who are output using \code{Page()} when
#'     they are autoprinted, i.e. returned as the value of the last expression
#'     in a Jupyter notebook cell.
#'
#' @param x A character string, the name of a class.
#' @export
add_paged_classes <- function(x){
    if(is.character(x)) classes <- x
    else classes <- class(x)
    if(length(classes)){
        paged_classes <- getOption("rkernel_paged_classes")
        options(rkernel_paged_classes=union(paged_classes,classes))
    }
}


#' Remove a Class to the 'Paged' Ones
#'
#' @description Remove a class from those who are output using \code{Page()} when
#'     they are autoprinted, i.e. returned as the value of the last expression
#'     in a Jupyter notebook cell.
#'
#' @param x A character string, the name of a class.
#' @export
remove_paged_classes <- function(x){
    if(is.character(x)) classes <- x
    else classes <- class(x)
    if(length(classes)){
        paged_classes <- getOption("rkernel_paged_classes")
        options(rkernel_paged_classes=setdiff(paged_classes,classes))
    }
}





#' Send Javascript to the frontend
#'
#' @description Send Javascript code in a character string or a text file to the
#'     frontend.
#'
#' @param text A character string with Javascript code
#' @param file Path of a file with Javascript code
#' @param as_tag Logical, whether to return a '<script>' tag
#' @return An S3 object of class "display_data" with mime data of type "application/javascript"
#' @export
Javascript <- function(text,file,as_tag=FALSE){
    if(missing(text)){
        if(!missing(file))
            text <- readLines(file)
    }
    text <- paste(text,collapse="\n")
    # text_plain <- paste("Javascript: ",text,sep="\n")
    if(as_tag){
        text_html <- paste("<script>",text,"</script>",sep="\n")
        display_data(#"text/plain"="",
            "text/html"=text_html)
    }
    else {
        display_data(#"text/plain"="",
            "application/javascript"=text)
    }
}

#' Send CSS code to the frontend
#'
#' @description Send CSS code in a character string or a text file to the
#'     frontend.
#'
#' @param text A character string with CSS styling information
#' @param file Path of a file with CSS styling information
#' @export
CSS <- function(text,file){
    if(missing(text)){
        text <- readLines(file)
    }
    text <- paste(text,collapse="\n")
    text_html <- paste("<style>",text,"</style>",sep="\n")
    display_data("text/html"=text_html)
}

#' Send LaTeX math to the frontend
#'
#' @description Send LaTeX code for math in a character string to the
#'     frontend to be formatted by MathJax 
#'
#' @param text A character string with LaTeX code for math
#' @export
LaTeXMath <- function(text){
    text <- paste(text,collapse="\n")
    text_plain <- paste("Math:",text,sep="\n")
    text_latex <- paste0("$$",text,"$$")
    display_data("text/plain"=text_plain,
            "text/latex"=text_latex)
}

#' Include HTML content using in an iframe
#'
#' @description Display the contents of a webpage or other HTML content
#'   by including output using an [iframe](https://html.spec.whatwg.org/multipage/iframe-embed-object.html).
#' @param url A character string, the URL of the content to be included
#' @param width A character string that specifies the width of the iframe
#' @param height A character string that specifies the width of the iframe
#' @param class An optional character string with DOM classes to be assigned to the iframe.
#' @param srcdoc Logical, whether to use a 'src' (FALSE, the default)  or 'srcdoc' attribute.
#' @export
IFrame <- function(url,width="100%",height="70ex",class=NULL,srcdoc=FALSE){
    tmpl <- paste(
        "<iframe",
        paste0(if(srcdoc) "srcdoc" else "src", "='%s'"),
        "style='width:%s;height:%s;'",
        if(length(class)) "class='%s'",
        "</iframe>",
        sep="\n")
    if(length(class))
        text_html <- sprintf(tmpl,url,width,height,class)
    else
        text_html <- sprintf(tmpl,url,width,height)
    display_data("text/plain"="",
                 "text/html"=text_html)
}

#' Send raw HTML code to the frontend
#'
#' @description Send raw HTML code in a character string to the
#'     frontend
#'
#' @param text A character string with LaTeX code for math
#' @param id A character string with the display id
#' @param update A logical value, should an existing display_data option?
#' @export
raw_html <- function(text,id=UUIDgenerate(),update=FALSE){
    text <- paste(text,collapse="\n")
    display_data("text/plain"="",
            "text/html"=text,
            id=id,
            update=update)
}

#' @describeIn display_data S3 method for class 'data.frame'
#' @importFrom repr mime2repr
#' @importFrom uuid UUIDgenerate
#' @export
display_data.data.frame <- function(x,...,
                            metadata=emptyNamedList,
                            id=UUIDgenerate(),
                            update=FALSE){

    rkernel_mime_types <- getOption("rkernel_mime_types",
                                    c("text/plain",
                                      "text/html",
                                      "text/latex",
                                      "text/markdown"))
    mime_data <- list()
    for(mime_type in rkernel_mime_types){
        if(mime_type=="text/html"){
            r_html <- scrolling_table(x)$data[["text/html"]]
            mime_data[[mime_type]] <- r_html
        }
        else{
            repr_func <- mime2repr[[mime_type]]
            repr_result <- repr_func(x,...)
            mime_data[[mime_type]] <- repr_result
        }
    }
    d <- list(data=mime_data)
    d$metadata <- metadata
    d$transient <- list(display_id=id)
    if(update) cl <- "update_display_data"
    else cl <- "display_data"
    structure(d,class=cl)
}

#' @describeIn display_data S3 method for matrices
#' @export
display_data.matrix <- display_data.data.frame


#' @describeIn display_data S3 method for "html_elem" objects (see \code{\link[memisc]{html}})
#' @export
display_data.html_elem <- function(x,...,
                              metadata=emptyNamedList,
                              id=UUIDgenerate(),
                              update=FALSE){
    text <- as.character(x)
    text <- paste(text,collapse="\n")
    display_data("text/plain"="",
            "text/html"=text,
            metadata=metadata,
            id=id,
            update=update)
}

#' @describeIn display_data S3 methods for "shiny.tab" objects
#' @export
display_data.shiny.tag <- function(x,...,
                              metadata=emptyNamedList,
                              id=UUIDgenerate(),
                              update=FALSE){
    text <- as.character(x)
    text <- paste(text,collapse="\n")
    display_data("text/plain"="",
            "text/html"=text,
            metadata=metadata,
            id=id,
            update=update)
}

#' @describeIn display_data S3 methods for "shiny.tab.list" objects
#' @export
display_data.shiny.tag.list <- function(x,...,
                              metadata=emptyNamedList,
                              id=UUIDgenerate(),
                              update=FALSE){
    text <- as.character(x)
    text <- paste(text,collapse="\n")
    display_data("text/plain"="",
            "text/html"=text,
            metadata=metadata,
            id=id,
            update=update)
}

#' @describeIn display_data S3 methods for "iframe" objects
#' @export
display_data.iframe <- function(x,...,
                                metadata=emptyNamedList,
                                id=attr(x,"id"),
                                update=FALSE){
    display_data("text/plain"="",
            "text/html"=unclass(x),
            metadata=metadata,
            id=id,
            update=update)
}

#' @describeIn display_data S3 methods for "htmlTable" objects
#' @export
display_data.htmlTable <- function(x,...,
                              metadata=emptyNamedList,
                              id=UUIDgenerate(),
                              update=FALSE){
    text <- as.character(x)
    text <- c("<div style=\"margin: 0 auto; display: table; margin-top: 1em;\">",
              text,
              "</div>")
    text <- paste(text,collapse="\n")
    display_data("text/plain"="",
            "text/html"=text,
            metadata=metadata,
            id=id,
            update=update)
}

#' @describeIn display_data S3 methods for "tableHTML" objects
#' @export
display_data.tableHTML <- function(x,...,
                              metadata=emptyNamedList,
                              id=UUIDgenerate(),
                              update=FALSE){
    text <- as.character(x)
    text <- c("<div style=\"margin: 0 auto; display: table; margin-top: 1em;\">",
              text,
              "</div>")
    text <- paste(text,collapse="\n")
    display_data("text/plain"="",
            "text/html"=text,
            metadata=metadata,
            id=id,
            update=update)
}



#' Create an alert in the frontend
#'
#' @description Creates Javascript code and sends it to the frontend that opens
#'     an alert box in the browser.
#' @param text A character string with the text that appears in the 
#' @export
alert <- function(text){
    alert.js <- sprintf("alert('%s')",text)
    d <- Javascript(alert.js)
    kernel <- get_current_kernel()
    kernel$display_data(data=d$data,
                        metadata=d$metadata)
}
