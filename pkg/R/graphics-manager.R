#' @importFrom jsonlite fromJSON
#' @importFrom curl curl_fetch_memory
#' @importFrom jsonlite base64_enc
#' @import R6

GraphicsDisplayManager <- R6Class("GraphicsDisplayManager",
  public = list(
      force_new_display = FALSE,
      initialize = function(host, port, kernel) {
          private$host <- host
          private$port <- port
          private$kernel <- kernel
      },
      new_display = function(render_info) {
          # log_out("graphicsDisplayManager$new_display")
          # log_print(render_info)
          display_ob <- GraphicsDisplay$new(host = private$host,
                                            port = private$port,
                                            kernel = private$kernel,
                                            render_info = render_info)
          display_id <- display_ob$display_id
          private$display_obs[[display_id]] <- display_ob
          private$current_display <- display_id
          display_ob
      },
      update_displays = function(force_new_display = FALSE) {
          # log_out("graphicsDisplayManager$update_displays")
          # log_str(private$display_obs)
          for(display_ob in private$display_obs) {
              display_id <- display_ob$display_id
              if(!display_ob$rendered && display_id != private$current_display) {
                  # log_out("display_ob$render(update = TRUE)")
                  display_ob$render(update = TRUE)
                  display_ob$send()
              }
          }
          if(length(private$current_display)) {
              display_ob <- private$display_obs[[private$current_display]]
              # log_out("display_ob$poll()")
              # log_print(display_ob)
              state <- display_ob$poll()
              # log_print(state)
              needs_update <- FALSE
              needs_new_display <- FALSE
              if(!display_ob$rendered) {
                  needs_update <- TRUE
              }
              if(state$updated) {
                  if(force_new_display) {
                      needs_new_display <- TRUE
                  } else {
                      needs_update <- TRUE
                  }
              }
              # log_out(sprintf("needs_update = %s",needs_update))
              # log_out(sprintf("needs_new_display = %s",needs_new_display))
              if(needs_update) {
                  display_ob$render(update = TRUE)
                  display_ob$send()
              }
              if(needs_new_display) {
                  render_info <- state
                  render_info["updated"] <- NULL
                  display_ob <- self$new_display(render_info)
                  display_ob$render()
                  display_ob$send()
              }
          }
      },
      handle_plot_new = function(render_info) {
          display_ob <- self$new_display(render_info)
          display_ob$send()
      }
  ),
  private = list(
      host = "",
      port = -1,
      display_obs = list(),
      current_display = character(0),
      kernel = NULL
  )
)
