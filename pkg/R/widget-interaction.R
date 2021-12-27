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
           description_width <- paste0(nchar(description),"em")
           w$description <- description
           w$style$description_width <- description_width
   }
   w
}

#' @export
mkWidget.Fixed <- function(x) list(value=x)


call_with_controls <- function(FUN,controls){
    args <- lapply(controls,"[[","value")
    do.call(FUN,args)
}

#' @export
interactive_output <- function(FUN,controls,
                               graphics_widget=NULL,
                               button=NULL,
                               continuous_update=TRUE
                              ){
    out <- OutputWidget(append_output=FALSE,
                        graphics_widget=graphics_widget)
    run <- function(...) with(out,
         call_with_controls(FUN,controls))
    for(cntrl in controls){
        cntrl$on_change(run)
        cntrl$continuous_update <- continuous_update
    }
    if(inherits(button,"Button"))
        button$on_click(run)
    run()
    out
}
