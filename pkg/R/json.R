#' @importFrom jsonlite fromJSON toJSON

#' @export
to_json <- function(x) UseMethod("to_json")

#' @export
to_json.default <- function(x) {
  attributes(x) <- NULL
  x
}

#' @export
to_json.list <- function(x) {
  nms <- names(x)
  attributes(x) <- NULL
  names(x) <- nms
  x
}
