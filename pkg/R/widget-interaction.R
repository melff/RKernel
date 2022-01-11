#' @include widget-output.R widget-value.R

#' @importFrom Cairo Cairo Cairo.capture
#' @importFrom png writePNG

#' @export
Fixed <- function(x) structure(x,class="Fixed")

#' @export
mkWidget <- function(x,...) UseMethod("mkWidget")
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

#' @export
mkWidget.Fixed <- function(x,...) list(value=x)

#' @export
mkWidget.ValueWidget <- function(x,...) x

call_with_controls <- function(FUN,controls){
    args <- lapply(controls,"[[","value")
    do.call(FUN,args)
}

#' @export
interactive_output <- function(FUN,
                               controls,
                               out,
                               button=NULL,
                               continuous_update=TRUE,
                               autorun=TRUE,
                               mime_type="text/plain"
                              ){
    run <- function(...) {
        with(out,{
            #out$clear_output()
            res <- call_with_controls(FUN,controls)
            # For reasons I have not found out,
            # 'print()'ing does not work with 
            # Jupyter notebooks - but with JupyterLab and 
            # Voila it does work.
            d <- display_data(res)
            d$data <- d$data[mime_type]
            d
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
