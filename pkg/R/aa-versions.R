jupyter_widgets_version <- new.env()

jupyter_widgets_base_version <- function(){

    v <- widget_module_versions()

    jupyter_widgets_version$base
}

jupyter_widgets_controls_version <- function(){

    v <- widget_module_versions()

    jupyter_widgets_version$controls
}

jupyter_widgets_protocol_version <- function(){

    v <- widget_module_versions()

    jupyter_widgets_version$protocol
}

jupyter_widgets_output_version <- function(){

    v <- widget_module_versions()

    jupyter_widgets_version$output
}

ipywidgets_version_from_jupyter <- function(){
    jupy.version <- system("jupyter --version",intern=TRUE)
    iwidgets.version <- grep("ipywidgets",jupy.version,value=TRUE)
    iwidgets.version <- trimws(strsplit(iwidgets.version,":")[[1]][2])
    iwidgets.version <- strsplit(iwidgets.version,".",fixed=TRUE)[[1]]
    iwidgets.version <- as.numeric(iwidgets.version)
    iwidgets.version
}

enquire_ipywidgets_version <- function(){
    if(getOption("widgets_version_from_jupyter",FALSE))
        ipywidgets_version_from_jupyter()
    else getOption("widgets_version",c(8,0,0))
} 

install_widget_module_versions <- function(v){

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

widget_module_versions <- function(){
    if(!length(get0("ipywidgets",envir=jupyter_widgets_version))){
        v <- enquire_ipywidgets_version()
        install_widget_module_versions(v)
    } else
        v <- jupyter_widgets_version$ipywidgets
    return(v)
}

has_iw_ver <- function(test){
    v <- widget_module_versions()
    ii <- seq_along(test)
    all(test==v[ii])
}

anywidget_version <- function(...) "~0.9.*"