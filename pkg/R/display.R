get_help_url <- function(){
    evaluator$current$get_help_url()
}
get_help_port <- function(){
    evaluator$current$get_help_port()
}

#' @include json.R

DLE <- '\x10'
DISPLAY_START <- '[!display]'

#' Display an R Object
#'
#' @param ... Arguments passed to 'display_data' methods
#' @export
display <- function(...){
    # log_out("display")
    d <- display_data(...)
    
    d <- list(type=class(d),
              content=unclass(d))
        # log_out("display")
        # log_out("display_data:")
        # log_out(d,use.str=TRUE)
    cat_(DLE)
    cat_(DISPLAY_START)
    # log_out(d,use.str=TRUE)
    zmq_send(d)
    cat_(DLE)
        # cat_('')
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

#' @describeIn display_data S3 method for plots saved with 'recordPlot()'
#'
#' @param width Width of the diplayed plot 
#' @param height Height of the displayed plot
#' @param pointsize Point size, see \code{\link[grDevices]{png}}
#' @param resolution Resolution in ppi, see \code{\link[grDevices]{png}}
#' @param scale The amount by which the plots are scaled in the frontend
#' @param units Units of width and height, see \code{\link[grDevices]{png}}
#' 
#' @importFrom uuid UUIDgenerate
#' @export
display_data.recordedplot <- function(x,
                                      width=getOption("jupyter.plot.width",6),
                                      height=getOption("jupyter.plot.height",6),
                                      pointsize=getOption("jupyter.plot.pointsize",12),
                                      resolution=getOption("jupyter.plot.res",150),
                                      scale=getOption("jupyter.plot.scale",.5),
                                      units=getOption("jupyter.plot.units","in"),
                                      metadata=emptyNamedList,
                                      id=UUIDgenerate(),
                                      update=FALSE,
                                      ...){

    rkernel_graphics_types <- getOption("jupyter.graphics.types",
                                        c("image/png","application/pdf"))

    mime_data <- list()
    mime_metadata <- list()

    for(mime in rkernel_graphics_types){
        mime_data[[mime]] <- mime_graphics(x,
                                           mime=mime,
                                           width=width,
                                           height=height,
                                           pointsize=pointsize,
                                           scale=scale,
                                           res=resolution,
                                           units=units)
        mime_metadata[[mime]] <- list(
            width=width*resolution*scale,
            height=height*resolution*scale)
        if(mime=="image/svg+xml")
            mime_metadata[[mime]]$isolated <-TRUE
    }

    d <- list(data=mime_data)
    d$metadata <- mime_metadata
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


popout_button_style <- '<style>
form.help-popout-button button {
     border-style:solid;
     border-width:1px;
     border-radius:0;
     border-color:#cfcfcf;
}
#pager-container form.help-popout-button {
     display:none;
}
iframe.manpage {
   border-style:solid;
   border-width:1px;
   border-color:#cfcfcf;
   border-radius:2px;
}
#pager-container iframe.manpage {
     height: 5000em;
}
</style>
'
popout_button <- '<form class="help-popout-button" action="%s" method="get" target="_blank">
             <button type="submit">Open in new tab</button>
</form>'


#' @describeIn display_data S3 method for help pages
#' @importFrom uuid UUIDgenerate
#' @importFrom utils URLencode
#' @importFrom tools Rd2HTML Rd2txt Rd2latex
#' @export 
display_data.help_files_with_topic <- function(x,...,
                                          id=UUIDgenerate(),
                                          update=FALSE){

    paths <- as.character(x)
    topic <- attr(x,"topic")

    utils_ns <- asNamespace("utils")
    getHelpFile <- get(".getHelpFile",utils_ns)

    if(length(paths)>=1){
        text_plain <- character(0)
        text_latex <- character(0)
        help_urls <- character(0)
        help_labels <- character(0)
        
        help_page_height <- getOption("help_page_height","60ex")

        for(file in paths){
            pkgname <- basename(dirname(dirname(file)))
            Rd <- getHelpFile(file)
            text_plain1 <- capture.output(Rd2txt(Rd, package = pkgname, outputEncoding = 'UTF-8'))
            # text_latex1 <- capture.output(Rd2latex(Rd, package = pkgname, outputEncoding = 'UTF-8'))
            text_plain <- c(text_plain,text_plain1)
            # text_latex <- c(text_latex,text_latex1)
            help_label1 <- paste0(pkgname,"::",topic)
            help_labels <- c(help_labels,help_label1)
            help_url1 <- paste0(get_help_url(),"/library/",pkgname,"/html/",basename(file),".html")
            help_urls <- c(help_urls,help_url1)
        }

        if(length(paths) == 1){
            help_url <- paste0(get_help_url(),"/library/",pkgname,"/html/",basename(paths),".html")
            text_html <- paste(paste0(
                "<iframe src='",help_url,"'"),
                "style='width:100%;height:",help_page_height,";'",
                "class='manpage'>",
                # "frameborder='0' onload=window.parent.scrollTo(0,0)>",
                "</iframe>",
                sep="\n")
        } 
        else {
            help_url <- paste0(get_help_url(),"/library/NULL/help/",URLencode(topic,reserved=TRUE))
            text_html <- paste(paste0("<iframe src='",help_url,"'"),
                "style='width:100%;height:",help_page_height,"'",
                "class='manpage'>",
                # "frameborder='0' onload=window.parent.scrollTo(0,0)>",
                "</iframe>",
                sep="\n")
        }

        popout_button <- sprintf(popout_button,help_url)
        text_html <- paste(text_html,popout_button_style,popout_button,sep="\n")

    } 
    else {
        text_plain <- paste(gettextf('No documentation for %s in specified packages and libraries:', sQuote(topic)),
                            gettextf('you could try %s', sQuote(paste0('??', topic))),
                            sep = '\n')
        text_html <- text_plain
        # text_latex <- text_plain
    }

    mime_data <- list(
        "text/plain"=paste(text_plain,collapse="\n"),
        "text/html"=paste(text_html,collapse="\n"),
        "text/latex"="" # paste(text_latex,collapse="\n")
    )


    d <- list(data=mime_data)
    d$metadata <- emptyNamedList
    d$transient <- list(display_id=id)
    if(update) cl <- "update_display_data"
    else cl <- "display_data"
    structure(d,class=cl)
}


#' Start interactive help system
#'
#' @description A variant of \code{\link[utils]{help.start}} that works when called from inside a
#'     Jupyter notebook.
#'
#' @param update A logical value. This formal argument exists for compatibility
#'     reasons only.
#' @param gui A character string. This formal argument exists for compatibility
#'     reasons only.
#' @param browser A character string. This formal argument exists for compatibility
#'     reasons only.
#' @param remote A character string. This formal argument exists for compatibility
#'     reasons only.
#' @export
help.start <- function(update = FALSE, 
                       gui = "irrelevant", 
                       browser = getOption("browser"), 
                       remote = NULL){
    help_url <- paste0(get_help_url(),"/doc/html/index.html")
    text_html <- paste(paste0("<iframe src='",help_url,"'"),
        "style='width:100%;height:70ex;'",
        "class='manpage'>",
        # "onload=window.parent.scrollTo(0,0)>",
        "</iframe>",
        sep="\n")
    popout_button <- sprintf(popout_button,help_url)
    text_html <- paste(text_html,popout_button_style,popout_button,sep="\n")
    mime_data <- list(
        "text/plain"=character(0),
        "text/html"=paste(text_html,collapse="\n"),
        "text/latex"=""
    )

    d <- list(data=mime_data)
    d$metadata <- emptyNamedList
    d$transient <- list(display_id=UUIDgenerate())
    if(update) cl <- "update_display_data"
    else cl <- "display_data"
    structure(d,class=cl)
}

help_start <- help.start





#' @describeIn display_data S3 method for results of 'help.search()'
#' @export
display_data.hsearch <- function(x,..., 
    id=UUIDgenerate(), 
    update=FALSE){
    
    matches <- x$matches
    matches <- unique(matches[c("Topic","Title","Package","Type")])
    matches$Name <- paste0(matches$Package,"::",matches$Topic)
    
    matches <- split(matches,matches$Type)
    text_out_grp <- lapply(matches,tformat_mgroup)
    text_plain <- character(0)
    if("help" %in% names(text_out_grp)){
        text_plain <- c(text_plain,"Help pages:","",text_out_grp$help,"")
    }
    if("vignette" %in% names(text_out_grp)){
        text_plain <- c(text_plain,"Vignettes:","",text_out_grp$vignette,"")
    }
    if("demo" %in% names(text_out_grp)){
        text_plain <- c(text_plain,"Demos:","",text_out_grp$demo,"")
    }

    # http://127.0.0.1:10006/doc/html/Search?pattern=regression&
    #fields.alias=1&fields.title=1&fields.concept=1&ignore.case=1&types.help=1&types.vignette=1&types.demo=1

    help_search_url <- paste0(get_help_url(),
                  "/doc/html/Search?pattern=",
                  gsub(" ","+",x$pattern,fixed=TRUE))
    for(field in x$fields){
        help_search_url <- paste0(help_search_url,paste0("&fields.",field,"=1"))
    }
    if(x$ignore.case)
        help_search_url <- paste0(help_search_url,"&ignore.cases=1")
    if(x$type=="fuzzy")
        help_search_url <- paste0(help_search_url,"&agrep=1")
    else if(is.numeric(x$agrep))
        help_search_url <- paste0(help_search_url,"&agrep=",x$agrep)
    for(type in c("help","vignette","demo")){
        if(type %in% x$types)
            help_search_url <- paste0(help_search_url,paste0("&types.",type,"=1"))
    }
    if(length(x$package)){
        help_search_url <- paste0(help_search_url,paste0("&package",type,"=",x$package))
    }
    text_html <- paste(paste0("<iframe src='",help_search_url,"'"),
                        "style='width: 100%;'",
                        "class='manpage'",
                        "frameborder='0' seamless onload=window.parent.scrollTo(0,0)>",
                        "</iframe>",
                        sep="\n")
    style <- '<style>
    form.help-popout-button {
         border-style:revert;
    }
    #pager-container form.help-popout-button {
         display:none;
    }
    iframe.manpage {
         height: 30rem;
    }
    #pager-container iframe.manpage {
         height: 5000em;
    }
    </style>
    '
    popout_button <- '<form class="help-popout-button" action="%s" method="get" target="_blank">
         <button type="submit">Open in new tab</button>
    </form>'
    popout_button <- sprintf(popout_button,help_search_url)
    text_html <- paste(style,popout_button,text_html,sep="\n")

    mime_data <- list("text/plain"=paste(text_plain,collapse="\n"),
                      "text/html"=paste(text_html,collapse="\n"))
    d <- list(data=mime_data)
    #d$metadata <- emptyNamedList
    d$transient <- list(display_id=id)
    if(update) cl <- "update_display_data"
    else cl <- "display_data"
    structure(d,class=cl)
}

tformat_mgroup <- function(matches){
    text_out <- matches[c("Topic","Title")]
    apply(text_out,1,format_item)
}

format_item <- function(x){
    x2 <- x[2]
    x2 <- strwrap(x2,120-7)
    x2 <- paste0("      ",x2)
    paste0(c(x[1],x2,""),collapse="\n")
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

#' @describeIn display_data S3 methods for \code{\link[shiny]{shiny}} objects
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


## From R base
cat_ <- function (..., file = "", sep = " ", fill = FALSE, labels = NULL, 
    append = FALSE) 
{
    if (is.character(file)) 
        if (file == "") 
            file <- stdout()
        else if (startsWith(file, "|")) {
            file <- pipe(substring(file, 2L), "w")
            on.exit(close(file))
        }
        else {
            file <- file(file, ifelse(append, "a", "w"))
            on.exit(close(file))
        }
    .Internal(cat(list(...), file, sep, fill, labels, append))
}

cat_with_hooks  <- function (..., file = "", sep = " ", fill = FALSE, labels = NULL, 
    append = FALSE) 
{
    if (is.character(file)) 
        if (file == "") 
            file <- stdout()
        else if (startsWith(file, "|")) {
            file <- pipe(substring(file, 2L), "w")
            on.exit(close(file))
        }
        else {
            file <- file(file, ifelse(append, "a", "w"))
            on.exit(close(file))
        }
    run_output_hooks()
    .Internal(cat(list(...), file, sep, fill, labels, append))
}

print_ <- getFromNamespace("print","base")

print_with_hooks <- function(x,...){
    run_output_hooks()
    print_(x,...)
}

str_ <- getFromNamespace("str","utils")

str_with_hooks <- function(object,...){
    run_output_hooks()
    suspend_output_hooks()
    str_(object,...)
    restore_output_hooks()
}

#' @export
install_output_hooks <- function() {
    replace_in_package("base","cat",cat_with_hooks)
    replace_in_package("base","print",print_with_hooks)
    replace_in_package("utils","str",str_with_hooks)
}

output_hooks <- new.env()
attr(output_hooks,"suspended") <- FALSE

#' @export
add_output_hook <- function(FUN,name,...){
    output_hooks[[name]] <- FUN
}

#' @export
remove_output_hook <- function(name){
    output_hooks[[name]] <- NULL
}

run_output_hooks <- function(...){
    nms <- sort(names(output_hooks))
    if(!isTRUE(attr(output_hooks,"suspended"))){
        for(n in nms){
            FUN <- output_hooks[[n]]
            FUN(...)
        }
    }
}

suspend_output_hooks <- function(){
    attr(output_hooks,"suspended") <- TRUE
}

restore_output_hooks <- function(){
    attr(output_hooks,"suspended") <- FALSE
}




# add_output_hook(function(...){
#     cat_("[Salut, vieux Jules!] ")
# },"ave")
