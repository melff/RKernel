plot_is_grid <- function(plt){
    if(length(plt) < 3) return(FALSE)
    else return(length(plt[[3]][[1]]) > 0)
}
    

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

plot_is_empty <- function(plt) {
    if(!length(plt)) return(TRUE)
    pcalls <- plot_calls(plt)
    # log_out(pcalls,use.print=TRUE)
    # log_out(empty_plot_calls,use.print=TRUE)
    # log_out(pcalls%in%empty_plot_calls,use.print=TRUE)
    if(!length(pcalls)) return(TRUE)
    res <- all(pcalls %in% empty_plot_calls)
    # log_out(res)
    return(res)
}

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
        else return(identical(plt1[[2]],plt2[[2]]))
    }
}
BerkDept <- marginSums(UCBAdmissions,"Dept")

library(ggplot2)

qplot(BerkDept)
plt3 <- recordPlot()
plt3a <- recordPlot()


barplot(BerkDept)
plt1 <- recordPlot()
plt1a <- recordPlot()

PercBerkDept <- memisc::percentages(BerkDept)
barplot(PercBerkDept)
plt2 <- recordPlot()

qplot(PercBerkDept)
plt4 <- recordPlot()
plt4a <- recordPlot()

barplot(PercBerkDept)
plt5 <- recordPlot()

curve(dnorm(x),from=-3,to=3,add=TRUE)
plt6 <- recordPlot()

# plot_is_grid(plt1)
# plot_is_grid(plt2)
# plot_is_grid(plt3)
# plot_is_grid(plt4)
# plot_is_grid(plt5)

compare_plots(plt1,NULL)


compare_plots(plt1,plt1a)

#debug(compare_plots)
compare_plots(plt1,plt2)

compare_plots(plt3,plt3a)
compare_plots(plt3,plt4)
compare_plots(plt1,plt5)
compare_plots(plt4,plt5)
compare_plots(plt4,plt6)
compare_plots(plt5,plt6)

identical(plt3[[2]],plt3a[[2]])
identical(plt3[[2]],plt4[[2]])


