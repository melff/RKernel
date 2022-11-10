#' Controls for Interactive Widgets
#'
#' @description A set of functions that can be used to create interactive
#'     widgets and to interact with widgets.
#'
#' @details The function \code{mkWidget} is a generic function that creates a
#'     widget that allows to manipulate the arguments of a function that is
#'     called in an interactive widget. This generic function is called by the
#'     function \code{\link{mWidgets}}. The function \code{Fixed} marks a value
#'     as fixed, so that \code{mkWidget} returns it as is.
#' 
#' @include widget-output.R widget-value.R
#' @export
mkWidget <- function(x,...) UseMethod("mkWidget")

#' @describeIn mkWidget S3 method for integer numbers
#' @export
mkWidget.integer <- function(x,description=NULL,...){
   if(length(x) == 1L){
       value <- x
       if(x < 0){
           min <- 3L*x
           max <- -x
       }
       else {
           min <- -x
           max <- 3L*x
       }
       w <- IntSlider(value=value,min=min,max=max)
   }
   else if(length(x) == 2L){
       if(is.null(names(x))){
           min <- x[1L]
           max <- x[2L]
       } else if(all(names(x) %in% c("min","max"))){
           min <- x["min"]
           max <- x["max"]
       }
       else stop("Inconsistent naming")    
       value <- (min + max)/2
       w <- IntSlider(value=value,min=min,max=max)
   }
   else if(length(x) == 3L){
       if(is.null(names(x))){
           value <- x[1L]
           min <- x[2L]
           max <- x[3L]
       } else if(all(names(x) %in% c("min","max","value"))){
           value <- x["value"]
           min <- x["min"]
           max <- x["max"]
       }
       else stop("Inconsistent naming")    
       w <- IntSlider(value=value,min=min,max=max)
   }
   else if(length(x) == 4L){
       if(is.null(names(x))){
           value <- x[1L]
           min <- x[2L]
           max <- x[3L]
           step <- x[4,L]
       } else if(all(names(x) %in% c("min","max","value","step"))){
           value <- x["value"]
           min <- x["min"]
           max <- x["max"]
           step <- x["step"]
       }
       else stop("Inconsistent naming")    
       w <- IntSlider(value=value,min=min,max=max,step=step)
   }
   if(!missing(description)){
           description <- as.character(description)
           description_width <- paste0(nchar(description),"em")
           w$description <- description
           w$style$description_width <- description_width
   }
   w
}

#' @describeIn mkWidget S3 method for floating point numbers
#' @export
mkWidget.numeric <- function(x,description=NULL,...){
   if(length(x) == 1L){
       value <- x
       if(x < 0){
           min <- 3*x
           max <- -x
       }
       else {
           min <- -x
           max <- 3*x
       }
       w <- FloatSlider(value=value,min=min,max=max)
   }
   else if(length(x) == 2L){
       if(is.null(names(x))){
           min <- x[1L]
           max <- x[2L]
       } else if(all(names(x) %in% c("min","max"))){
           min <- x["min"]
           max <- x["max"]
       }
       else stop("Inconsistent naming")    
       value <- (min + max)/2
       w <- FloatSlider(value=value,min=min,max=max)
   }
   else if(length(x) == 3L){
       if(is.null(names(x))){
           value <- x[1L]
           min <- x[2L]
           max <- x[3L]
       } else if(all(names(x) %in% c("min","max","value"))){
           value <- x["value"]
           min <- x["min"]
           max <- x["max"]
       }
       else stop("Inconsistent naming")    
       w <- FloatSlider(value=value,min=min,max=max)
   }
   else if(length(x) == 4L){
       if(is.null(names(x))){
           value <- x[1L]
           min <- x[2L]
           max <- x[3L]
           step <- x[4,L]
       } else if(all(names(x) %in% c("min","max","value","step"))){
           value <- x["value"]
           min <- x["min"]
           max <- x["max"]
           step <- x["step"]
       }
       else stop("Inconsistent naming")    
       w <- FloatSlider(value=value,min=min,max=max,step=step)
   }
    if(!missing(description)){
        description <- as.character(description)
        description_width <- paste0(nchar(description),"ex")
        w$description <- description
        w$style$description_width <- description_width
    }
   w
}

#' @describeIn mkWidget S3 method for logical numbers
#' @export
mkWidget.logical <- function(x,description=NULL,...){
    w <- Checkbox()
    if(!missing(description)){
        description <- as.character(description)
        description_width <- paste0(nchar(description),"ex")
        w$description <- description
        w$style$description_width <- description_width
    }
    w
}

#' @describeIn mkWidget S3 method for character strings
#' @export
mkWidget.character <- function(x,description=NULL,...){
    if(length(x) == 1){
        w <- TextWidget(x)
    }
    else {
        w <- Dropdown(options=x,value=x[1])
    }
    if(!missing(description)){
        description <- as.character(description)
        description_width <- paste0(nchar(description),"ex")
        w$description <- description
        w$style$description_width <- description_width
    }
    w
}

#' @describeIn mkWidget S3 method for objects 
#' @export
mkWidget.Fixed <- function(x,...) list(value=x)

#' @describeIn mkWidget S3 method for objects 
#' @export
mkWidget.ValueWidget <- function(x,...) x

#' @rdname mkWidget
#' @export
Fixed <- function(x) structure(x,class="Fixed")

#' Interactions Using Widgets
#'
#' @description A variety of functions to create interactive function calls
#' @name interaction
NULL

call_with_controls <- function(FUN,controls){
    args <- lapply(controls,"[[","value")
    do.call(FUN,args)
}

#' @rdname interaction
#' @param FUN A function to called with arguments manipulated using interactive
#'     widgets.
#' @param controls A list of controlling widgets, usually created with the
#'     function \code{mkwidgets}
#' @param out An output widget, i.e. a widget in class "OutputWidget"
#' @param button An (optional) button widget; when clicked, the function
#'     \code{FUN} is called.
#' @param continous_update A logical value, if \code{TRUE} the function
#'     \code{FUN} is called whenever one of the controlling widgets changes a
#'     value
#' @param mime_type A character string that specifies the mime type as which the
#'     return value of \code{FUN} is displayed.
#' @export
interactive_output <- function(FUN,
                               controls,
                               out,
                               button=NULL,
                               continuous_update=TRUE,
                               autorun=TRUE,
                               clear=FALSE,
                               mime_type="text/plain"
                              ){
    run <- function(...) {
        if(clear) out$clear_output()
        with(out,{
            res <- call_with_controls(FUN,controls)
            # For reasons I have not found out,
            # 'print()'ing does not work with 
            # Jupyter notebooks - but with JupyterLab and 
            # Voila it does work.
            if(length(res)){
                d <- display_data(res)
                d$data <- d$data[mime_type]
                d
            }
        })
    }
    if(autorun){
        for(cntrl in controls){
            cntrl$on_change(run)
            cntrl$continuous_update <- continuous_update
        }
    }
    if(inherits(button,"Button"))
        button$on_click(run)
    run()
}

#' @rdname interaction
#' @param ... Named arguments, transformed into widgets using the generic
#'     function \code{\link{mkWidget}}.
#' @export
mkWidgets <- function(...){
    args <- list(...)
    argnames <- names(args)
    arglabels <- format(argnames)
    res <- list()
    for(i in seq_along(argnames)){
        n <- argnames[i]
        x <- args[[n]]
        l <- arglabels[i]
        res[[n]] <- mkWidget(x,description=l)
    }
    res
}


#' @rdname interaction
#' @param graphics Logical, whether graphics output is expected, in which case
#'   an "ImageWidget" object is created to receive the graphics output.
#' @export
Interactive <- function(FUN,...,continuous_update=TRUE,
                     graphics=FALSE){
    controls <- mkWidgets(...)
    if(graphics){
        gw_res <- (getOption("jupyter.plot.res") *
                   getOption("jupyter.plot.scale"))
        gw_h <- getOption("jupyter.plot.height") * gw_res
        gw_w <- getOption("jupyter.plot.width") * gw_res
        gw_h <- paste0(gw_h,"px")
        gw_w <- paste0(gw_w,"px")
        gw <- ImageWidget(width=gw_w,height=gw_h)
    }
    else gw <- NULL
    output <- OutputWidget(append_output=FALSE,
                           graphics_widget=gw)
    io <- interactive_output(FUN=FUN,
                       out=output,
                       controls=controls,
                       continuous_update=continuous_update)
    VBox(c(controls,gw,output))
}

#' @rdname interaction
#' @param graphics Logical, whether graphics output is expected, in which case
#'   an "ImageWidget" object is created to receive the graphics output.
#' @export
interact <- function(FUN,...,continuous_update=TRUE,
                     graphics=FALSE){
    widget <- interactive(FUN,...,
                          continuous_update=continuous_update,
                          graphics=graphics)
    display(widget)
}
