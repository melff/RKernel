#' Display an R Object
#'
#' @export
display <- function(...){
    # log_out("display")
    d <- display_data(...)
    kernel <- get_current_kernel()
    kernel$display_data(data=d$data,
                        metadata=d$metadata)
}

#' Prepare an R Object for being displayed
#'
#' @description A generic function that prepares R objects for display using \code{display()}
#'
#' @param x An object to be prepared for display.
#' @return An object of class "display_data"
#' 
#' @export
display_data <- function(x,...) UseMethod("display_data")

#' @describeIn display_data 
#' @importFrom repr mime2repr
#' @importFrom uuid UUIDgenerate
#' @export
display_data.default <- function(x,...,
                            metadata=emptyNamedList,
                            id=UUIDgenerate(),
                            update=FALSE){

    if(missing(x)){
        if(!length(metadata)) metadata <- emptyNamedList
        d <- list(data=list(...))
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
    d$metadata <- metadata
    d$transient <- list(display_id=id)
    if(update) cl <- "update_display_data"
    else cl <- "display_data"
    structure(d,class=cl)
}

#' @describeIn display_data 
#' @importFrom uuid UUIDgenerate
#' @importFrom htmltools htmlEscape
#' @importFrom digest digest
#' @importFrom base64enc dataURI
#' @export
display_data.htmlwidget <- function(x,...,
                            metadata=emptyNamedList,
                            id=UUIDgenerate(),
                            update=FALSE){

    hash <- digest::digest(as.character(x))
    url <- paste0(hash,".html")

    embed <- getOption("htmlwidgets_embed",FALSE) 
    assets <- getOption("htmlwigets_assetsdir","assets")
    path <- getOption("htmlwidget_path","htmlwidgets")
    if(length(path)){
        url <- paste(path,url,sep="/")
        path <- file.path(getwd(),path)
        if(!file_test("-d",path))
            dir.create(path)
    }
    else
        path <- getwd()
    htmlfile <- file.path(path,paste0(hash,".html"))
    assets <- file.path(path,assets)

    htmlwidgets::saveWidget(x,
                            file=htmlfile,
                            selfcontained=embed,
                            libdir=assets,
                            )

    res <- "<div>"
    if_tmpl <- "
            <iframe src=\"%s\" width=\"%s\" height=\"%s\" seamless  style=\"border-style:none\">
            </iframe>
            "
    width <- getOption("embed_htmlwidget_width","100%")
    height <- getOption("embed_htmlwidget_height",600L)

    if(embed)
        url <- dataURI(mime="text/html",file=htmlfile)
    iframe <- sprintf(if_tmpl,url,width,height)
    res <- paste0(res,iframe)
    res <- paste0(res,"</div>")
    text <- paste(res,collapse="\n")
    mime_data <- list(
        "text/plain"="",
        "text/html"=text
    )

    d <- list(data=mime_data)
    d$metadata <- metadata
    d$transient <- list(display_id=id)
    if(update) cl <- "update_display_data"
    else cl <- "display_data"
    structure(d,class=cl)
}

#' @describeIn display_data 
#' @importFrom repr mime2repr
#' @importFrom uuid UUIDgenerate
#' @export
display_data.recordedplot <- function(x,
                                      width=getOption("jupyter.plot.wdith",6),
                                      height=getOption("jupyter.plot.height",6),
                                      pointsize=getOption("jupyter.plot.pointsize",12),
                                      resolution=getOption("jupyter.plot.res",150),
                                      scale=getOption("jupyter.plot.scale",.5),
                                      units=getOption("jupyter.plot.units","in"),
                                      metadata=emptyNamedList,
                                      id=UUIDgenerate(),
                                      update=FALSE){

    rkernel_graphics_types <- getOption("jupyter.graphics.types",
                                        c("image/png","application/pdf"))

    mime_data <- list()
    mime_metadata <- list()

    for(mime in rkernel_graphics_types){
        repr_func <- mime2repr[[mime]]
        mime_data[[mime]] <- repr_func(x,
                                       width=width,
                                       height=height,
                                       pointsize=pointsize,
                                       res=resolution)
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

getMatch <- function(x,match){
    if(length(match) < 0) return(character(0))
    if(is.list(match))
        match <- match[[1]]
    lens <- attr(match,"match.length")
    n <- length(match)
    res <- character(n)
    for(i in 1:n){
        start <- match[i]
        end <- start + lens[i] - 1
        res[i] <- substr(x,start=start,stop=end)
    }
    res
}

#' @describeIn display_data 
#' @export
display_data.display_data <- function(x,...) x

#' @describeIn display_data 
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

#' @describeIn display_id
#' @export
display_id.display_data <- function(x) x$transient$display_id

#' @describeIn display_id
#' @export
display_id.update_display_data <- function(x) x$transient$display_id

#' @describeIn display_data 
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
#' @export
Page <- function(x,...) UseMethod("Page")

#' @describeIn Page
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

#' Add a Class to the 'Displayed' ones
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

#' Remove a Class to the 'Displayed' ones
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

#' Add a Class to the 'Paged' ones
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


#' Remove a Class to the 'Paged' ones
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

#' @describeIn display_data 
#' @importFrom uuid UUIDgenerate
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

        for(file in paths){
            pkgname <- basename(dirname(dirname(file)))
            Rd <- getHelpFile(file)
            text_plain1 <- capture.output(Rd2txt(Rd, package = pkgname, outputEncoding = 'UTF-8'))
            text_latex1 <- capture.output(Rd2latex(Rd, package = pkgname, outputEncoding = 'UTF-8'))
            text_plain <- c(text_plain,text_plain1)
            text_latex <- c(text_latex,text_latex1)
            help_label1 <- paste0(pkgname,"::",topic)
            help_labels <- c(help_labels,help_label1)
            help_url1 <- paste0(get_help_url(),"/library/",pkgname,"/html/",basename(file),".html")
            help_urls <- c(help_urls,help_url1)
        }
    } 
    else {
        text_plain <- paste(gettextf('No documentation for %s in specified packages and libraries:', sQuote(topic)),
                      gettextf('you could try %s', sQuote(paste0('??', topic))), sep = '\n')
        text_latex <- text_plain
    }

    if(length(paths) == 1){
        help_url <- paste0(get_help_url(),"/library/",pkgname,"/html/",basename(paths),".html")
        text_html <- paste(paste0(
                           "<iframe src='",help_url,"'"),
                           "style='width: 100%;'",
                           "class='manpage'",
                           "frameborder='0' seamless onload=window.parent.scrollTo(0,0)>",
                           "</iframe>",
                           sep="\n")
    } 
    else {
        help_url <- paste0(get_help_url(),"/library/NULL/help/",URLencode(topic,reserved=TRUE))
        text_html <- paste(paste0(
                           "<iframe src='",help_url,"'"),
                           "style='width: 100%;'",
                           "class='manpage'",
                           "frameborder='0' seamless onload=window.parent.scrollTo(0,0)>",
                           "</iframe>",
                           sep="\n")
    }

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
    popout_button <- sprintf(popout_button,help_url)
    text_html <- paste(style,popout_button,text_html,sep="\n")

    mime_data <- list(
        "text/plain"=paste(text_plain,collapse="\n"),
        "text/html"=paste(text_html,collapse="\n"),
        "text/latex"=paste(text_latex,collapse="\n")
    )

    d <- list(data=mime_data)
    d$metadata <- list(height="40ex")
    d$transient <- list(display_id=id)
    if(update) cl <- "update_display_data"
    else cl <- "display_data"
    structure(d,class=cl)
}

#' @describeIn display_data 
#' @export
display_data.hsearch <- function(x,..., 
    id=UUIDgenerate(), 
    update=FALSE){
    
    matches <- x$matches
    matches <- unique(matches[c("Topic","Title","Package","Type")])
    matches <- within(matches,
                      Name <- paste0(Package,"::",Topic))
    
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


#' Prepare Javascript to be sent to the frontend
#'
#' @param text A character string with Javascript code
#' @param file Path of a file with Javascript code
#' @return An S3 object of class "display_data" with mime data of type "application/javascript"
#' @export
Javascript <- function(text,file){
    if(missing(text)){
        text <- readLines(file)
    }
    text <- paste(text,collapse="\n")
    # text_plain <- paste("Javascript: ",text,sep="\n")
    #text_html <- paste("<script>",text,"</script>",sep="\n")
    display_data(#"text/plain"="",
            "application/javascript"=text)
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

#' @describeIn display_data
#' @export
display_data.matrix <- display_data.data.frame


#' @describeIn display_data
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

#' @describeIn display_data
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
