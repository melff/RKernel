variables_helper <- new.env()

#' @export
inspect_variables <- function(envir = parent.frame()){
  var_names <- ls(envir = envir)
  var_refs <- structure(
    seq_along(var_names),
    names = var_names
  )
  variables_helper$refs <- var_refs
  response <- list()
  for (i in seq_along(var_names)) {
    var_name <- var_names[i]
    value <- get(var_name, envir = envir)
    descr <- inspect_value(value)
    if (descr$structured) var_ref <- var_refs[i]
    else var_ref <- 0
    response[[i]] <- list(
      name = var_names[i],
      variablesReference = var_ref,
      value = descr$value,
      type = descr$type
    )
  }
  msg_send(response)
}

inspect_value <- function(x) {
  if (is.atomic(x)) {
    value <- get_atomic_val(x)
    type <- get_atomic_type(x)
    structured <- FALSE
  }
  else if(is.function(x)){
    type <- "function"
    value <- get_function_val(x)
    structured <- FALSE  
  }
  else {
    type <- paste(class(x), collapse=", ")
    value <- get_structured_val(x)
    structured <- TRUE
  }
  return(list(
    type = type,
    value = value,
    structured = structured
  ))
}

inspect_helper_types <- c(
  "integer" = "int",
  "logical" = "bool",
  "double" = "float",
  "character" = "str"
)

get_atomic_type <- function(x){
  tp <- typeof(x)
  if (length(x) <= 1 && tp %in% names(inspect_helper_types)) {
    type <- inspect_helper_types[tp]
  }
  else if(is.array(x)) {
    type <- tp
  }
  else {
    type <- paste(
      gsub(
        "NULL ...", "",
        capture.output(str_(x, vec.len = 0L, give.attr = FALSE))
      ),
      collapse = "\n"
    )
  }
}
get_atomic_val <- function(x){
  if(is.array(x)) {
    paste("dim:", paste(dim(x), collapse = " × "))
  } else if(length(x) > 1) {
    paste(
      capture.output(
        str_(x, 
             give.attr = FALSE, 
             give.length =FALSE, 
             give.head=FALSE)),
      collapse = "\n"
    )
  } else {
    format(x)
  }
}

get_function_val <- function(x) {
  res <- paste(
    capture.output(str_(x, give.attr = FALSE)),
    collapse = "\n"
  )
  sub("function ", "\\", res, fixed = TRUE)
}

get_structured_val <- function(x) UseMethod("get_structured_val")

get_structured_val.default <- function(x){
      capture.output(str_(x, give.attr = FALSE, list.len = 0))[1]
}

get_structured_val.list <- function(x) {
  paste(length(x), "elements")
}

get_structured_val.data.frame <- function(x) {
  paste(nrow(x), "obs. of", ncol(x), "variables")
}

get_structured_val.data.set <- function(x) {
  paste(nrow(x), "obs. of", ncol(x), "variables")
}

get_structured_val.array <- function(x) {

}