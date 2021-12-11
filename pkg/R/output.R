#' @include traitlets.R

OutputClass <- R6Class_("Output",
   public = list(
       display_send = function(d) NULL,
       last_plot_id = character(0),
       graphics_send = function(plt,
                                width=getOption("jupyter.plot.wdith",6),
                                height=getOption("jupyter.plot.height",6),
                                pointsize=getOption("jupyter.plot.pointsize",12),
                                resolution=getOption("jupyter.plot.res",150),
                                scale=getOption("jupyter.plot.scale",.5),
                                units=getOption("jupyter.plot.units","in"),
                                update=FALSE){
           log_out("OutputClass$display_send")
           update <- update && length(self$last_plot_id)
           if(update){
               id <- self$last_plot_id
           } 
           else {
               id <- UUIDgenerate()
               self$last_plot_id <- id
           } 

           d <- display_data(plt,
                             width=width,
                             height=height,
                             pointsize=pointsize,
                             resolution=resolution,
                             scale=scale,
                             units=units,
                             id=id,
                             update=update)
           self$display_send(d)
       },
       stream = function(text,stream) NULL
   )
)

KernelOutputClass <- R6Class_("KernelOutput",
   inherit = OutputClass,
   public = list(
       display_send = function(d){
           private$kernel$display_send(d)
       },
       stream = function(text,stream){
           private$kernel$stream(text,stream)
       },
       initialize = function(kernel){
           private$kernel <- kernel
       }
   ),
   private = list(
       kernel = NULL
   )
)


output_channels <- new.env()

#' @export
set_channel <- function(channel=NULL){
    channels <- output_channels$stack
    l <- length(channels)
    if(length(channel)){
        channels[[l+1]] <- channel
    }
    else if(l > 1)
        channels <- channels[seq.int(l-1)]
        
    output_channels$stack <- channels
}

#' @export
get_current_channel <- function(){
    channels <- output_channels$stack
    l <- length(channels)
    if(l > 0)
        channels[[l]]
    else {
        kernel <- get_current_kernel()
        channel <- KernelOutputClass(kernel=kernel)
        output_channels$stack <- channel
        kernel
    }
}

#' @export
display_graphics <- function(update=TRUE){
    plt <- recordPlot()
    channel <- get_current_channel()
    channel$graphics_send(plt,update=update)
}

#' @export
update_graphics <- function(){
    display_graphics(update=TRUE)
}
