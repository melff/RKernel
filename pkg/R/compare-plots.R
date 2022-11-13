# cf. 'graphics.r' in package "evaluate" (Yihui Xie et al.)
empty_plot_calls <- c("palette",
                      "palette2",
                      paste0("C_",c("layout",
                                  "par",
                                  "clip",
                                  "strWidth",
                                  "strHeight",
                                  "plot_new",
                                  "plot_window")))

plot_calls <- function(plt){
    plt <- plt[[1]]
    plt <- lapply(plt,"[[",2)
    if(!length(plt)) return(NULL)
    lapply(plt,"[[",1)
}

non_empty_plot_calls <- function(plt){
    if(!length(plt)) return(NULL)
    pcalls <- plot_calls(plt)
    pcnames <- sapply(pcalls,get_name_el)
    empty <- pcnames %in% empty_plot_calls
    pcalls[!empty]
}

get_name_el <- function(x){
    if(length(x$name)) x$name 
    else deparse(x)
}

is_base_graphics <- function(plt){
    plt1 <- plt[[1]][[1]][[2]][[1]]
    get_name_el(plt1) == "C_plot_new"
}

compare_plots <- function(plt1,plt2){
    if((length(plt1) > 0) != (length(plt2) > 0)) return(FALSE)
    if(is_base_graphics(plt1) != is_base_graphics(plt2)) return(FALSE)
    if(!is_base_graphics(plt1)) return(identical(plt1[[3]],plt2[[3]]))
    else {
        ne1 <- non_empty_plot_calls(plt1)
        ne2 <- non_empty_plot_calls(plt2)
        if(!identical(ne1,ne2)) return(FALSE)
        else {
            # log_out(ne1)
            return(identical(plt1[[2]],plt2[[2]]))
        }
    }
}

plot_is_empty <- function(plt){
    if(!length(plt) || !length(plt)[[1]]) {
        return(TRUE)
    }
    ne1 <- non_empty_plot_calls(plt)
    return(length(ne1) == 0)
}

plot_has_changed <- function(current,last){
    if(!length(current) || !length(current)[[1]]) {
        # log_out("Current plot is NULL")
        return(FALSE)
    }
    ne1 <- non_empty_plot_calls(current)
    if(!length(ne1)) {
        # log_out("Current plot is empty")
        return(FALSE)
    }
    if(is_base_graphics(current)){
        if(!length(last)) {
            # log_out("Last plot is NULL")
            return(TRUE)
        }
        else if(!is_base_graphics(last)) return(TRUE)
        else {
            ne2 <- non_empty_plot_calls(last)
            if(!length(ne2)){
                # log_out("Last plot is empty")
                return(TRUE)
            }
            if(!identical(ne1,ne2)) return(TRUE)
            else {
                # log_out(digest::sha1(last[[2]]))
                # log_out(digest::sha1(current[[2]]))
                unchanged <- identical(current[[2]],last[[2]])
                # if(unchanged)
                #     log_out("Plot has not changed")
                return(!unchanged)
            }
        }
    }
    else {
        if(is_base_graphics(last)) return(TRUE)
        else if(!length(last)) return(TRUE)
        else {
            unchanged <- identical(current[[3]],last[[3]])
            return(!unchanged)
        }
    }
}

