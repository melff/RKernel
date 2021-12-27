#' @importFrom uuid UUIDgenerate

uuid <- function() {
    id <- UUIDgenerate()
    gsub("-","",id)
}
