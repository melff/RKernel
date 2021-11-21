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
        stream = function(text,stream_name) {
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
            #log_out(id,use.print=TRUE)
            log_out(self$display_index,use.print=TRUE)
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
