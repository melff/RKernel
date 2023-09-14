jupyter_widgets_version <- new.env()

jupyter_widgets_base_version <- function(){

    if(!length(get0("ipywidgets",envir=jupyter_widgets_version)))
        set_jupyter_widget_versions()
     
    jupyter_widgets_version$base
}

jupyter_widgets_controls_version <- function(){

    if(!length(get0("ipywidgets",envir=jupyter_widgets_version)))
        set_jupyter_widget_versions()

    jupyter_widgets_version$controls
}

jupyter_widgets_protocol_version <- function(){

    if(!length(get0("ipywidgets",envir=jupyter_widgets_version)))
        set_jupyter_widget_versions()

    jupyter_widgets_version$protocol
}

jupyter_widgets_output_version <- function(){

    if(!length(get0("ipywidgets",envir=jupyter_widgets_version)))
        set_jupyter_widget_versions()

    jupyter_widgets_version$output
}

pull_jupyter_ipywidgets_version <- function(){
    jupy.version <- system("jupyter --version",intern=TRUE)
    iwidgets.version <- grep("ipywidgets",jupy.version,value=TRUE)
    iwidgets.version <- trimws(strsplit(iwidgets.version,":")[[1]][2])
    iwidgets.version <- strsplit(iwidgets.version,".",fixed=TRUE)[[1]]
    iwidgets.version <- as.numeric(iwidgets.version)
    iwidgets.version
}



set_jupyter_widget_versions <- function(){

    v <- pull_jupyter_ipywidgets_version()
    jupyter_widgets_version$ipywidgets <- v

    # __protocol_version__
    if(v[1] == 7)
        jupyter_widgets_version$protocol <- "2.0.0"
    else if(v[1] == 8)
        jupyter_widgets_version$protocol <- "2.1.0"
    else stop("unsupported ipywidgets version")

    # __jupyter_widgets_base_version__
    if(v[1] == 7){
        if(v[2] <= 2)
            jupyter_widgets_version$base <- "1.0.0"
        else if(v[2] <= 4)
            jupyter_widgets_version$base <- "1.1.0"
        else if(v[2] <= 6)
            jupyter_widgets_version$base <- "1.2.0"
        else stop("unsupported ipywidgets version")
    }
    else if(v[1] == 8) {
        if(v[2] <= 1)
            jupyter_widgets_version$base <- "2.0.0"
        else stop("unsupported ipywidgets version")
    }

    # __jupyter_widgets_output_version__
    if(v[1] == 7 && v[2] >= 1 || v[1] == 8)
        jupyter_widgets_version$output <- "1.0.0"

    # __jupyter_widgets_controls_version__
    if(v[1] == 7) {
        if(v[2] <= 0)
            jupyter_widgets_version$controls <- "1.0.0"
        else if(v[2] <= 1)
            jupyter_widgets_version$controls <- "1.1.0"
        else if(v[2] <= 2)
            jupyter_widgets_version$controls <- "1.2.0"
        else if(v[2] <= 3)
            jupyter_widgets_version$controls <- "1.3.0"
        else if(v[2] <= 4)
            jupyter_widgets_version$controls <- "1.4.0"
        else if(v[2] <= 6)
            jupyter_widgets_version$controls <- "1.5.0"
        else stop("unsupported ipywidgets version")
    }
    else if(v[1] == 8) {
        if(v[2] <= 1)
            jupyter_widgets_version$controls <- "2.0.0"
        else stop("unsupported ipywidgets version")
    }
    
}

get_jupyter_ipywidgets_version <- function(){
    if(!length(get0("ipywidgets",envir=jupyter_widgets_version))){
        v <- pull_jupyter_ipywidgets_version
        jupyter_widgets_version$ipywidgets <- v
    } else
        v <- jupyter_widgets_version$ipywidgets
    return(v)
}

has_iw_ver <- function(test){
    v <- get_jupyter_ipywidgets_version()
    ii <- seq_along(test)
    all(test==v[ii])
}
