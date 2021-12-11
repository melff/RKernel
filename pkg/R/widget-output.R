#' @include widget-dom.R

#' @export
OutputWidgetClass <- R6Class_("OutputWidget",
    inherit = DOMWidgetClass,
    public = list(
        `_view_name` = structure(Unicode("OutputView"),sync=TRUE),
        `_model_name` = structure(Unicode("OutputModel"),sync=TRUE),
        `_view_module` = structure(Unicode("@jupyter-widgets/output"),sync=TRUE),
        `_model_module` = structure(Unicode("@jupyter-widgets/output"),sync=TRUE),
        `_view_module_version` = structure(Unicode(jupyter_widgets_output_version),sync=TRUE),
        `_model_module_version` = structure(Unicode(jupyter_widgets_output_version),sync=TRUE),

        msg_id = structure(Unicode(""),sync=TRUE),
        outputs = structure(List(),sync=TRUE),
        last_plot_id = character(0),
        graphics_send = function(plt,
                                 width=getOption("jupyter.plot.wdith",6),
                                 height=getOption("jupyter.plot.height",6),
                                 pointsize=getOption("jupyter.plot.pointsize",12),
                                 resolution=getOption("jupyter.plot.res",150),
                                 scale=getOption("jupyter.plot.scale",.5),
                                 units=getOption("jupyter.plot.units","in"),
                                 update=FALSE){
            # log_out("OutputWidget$graphics_send")
            # log_out("  -- update = ",if(update)"TRUE"else"FALSE")
            # log_out("  -- id = ",self$last_plot_id)

            update <- update && (length(self$last_plot_id)>0)
            if(update){
                id <- self$last_plot_id
            } 
            else {
                id <- UUIDgenerate()
                self$last_plot_id <- id
            } 
            # log_out("  -- update = ",if(update)"TRUE"else"FALSE")
            # log_out("  -- id = ",id)

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
        stream = function(text,stream_name) {
            # log_out("OutputWidget$stream")
            self$outputs <- append(self$outputs,
                                   list(list(
                                       output_type = "stream",
                                       name = stream_name,
                                       text = text
                                   )))
        },
        display_index = integer(0),
        stdout = function(text) self$stream(text,stream_name="stdout"),
        stderr = function(text) self$stream(text,stream_name="stderr"),
        display_send = function(d){
            if(!(class(d)%in%c("display_data","update_display_data")))
                stop("'display_data' or 'update_display_data' object required")
            out_data <- list(output_type = "display_data",
                             data = d$data,
                             metadata = d$metadata)
            id <- d$transient$display_id
            update <- inherits(d,"update_display_data")
            l <- length(self$display_index)
            # log_out(id,use.print=TRUE)
            # log_out("OutputWidget$display_send")
            # log_out(" -- id = ",id)
            # log_out(self$display_index,use.print=TRUE)
            if(update){
                if(id == "last" && l > 0){
                    i <- self$display_index[l]
                    self$outputs[[i]] <- out_data
                }
                if(id %in% names(self$display_index)){
                    i <- self$display_index[id]
                    self$outputs[[i]] <- out_data
                }
            } else {
                if(l < 1 || !(id %in% names(self$display_index))){
                    i <- length(self$outputs) + 1L
                    ii <- l + 1L
                    self$display_index[ii] <- i
                    names(self$display_index)[ii] <- id
                    self$outputs[[i]] <- out_data
                }
            }
        },
        last_display = function(){
            l <- length(self$display_index)
            if(l > 0){
                names(self$display_index[l])
            }
            else
                character(0)
        }
    )
    )

#' @export
OutputWidget <- function(...) OutputWidgetClass$new(...)
