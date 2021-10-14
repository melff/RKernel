uuid <- function() {
    id <- UUIDgenerate()
    gsub("-","",id)
}
