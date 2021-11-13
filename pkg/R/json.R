#' @importFrom jsonlite fromJSON toJSON

#' @export
to_json <- function(x) UseMethod("to_json")

#' @export
to_json.default <- function(x) {
  attributes(x) <- NULL
  x
}

