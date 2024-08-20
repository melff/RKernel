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
    var_ref <- 0
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
    if (length(x) <= 1) {
      value <- format(x)
    } else {
      value <- get_atomic_val(x)
    }
    type <- get_atomic_type(x)
  }
  else if(is.function(x)){
      type <- "function"
      value <- get_function_val(x)
  }
  else {
      type <- class(x)
      value <- get_structured_val(x)
  }
  return(list(
    type = type,
    value = value
  ))
}

inspect_helper_types <- c(
  "integer" = "int",
  "logical" = "bool",
  "numeric" = "float",
  "character" = "str",
  "factor" = "str"
)

get_atomic_type <- function(x){
  cl <- class(x)
  if(cl %in% names(inspect_helper_types)) {
    type <- inspect_helper_types[cl]
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
    paste(
      capture.output(str_(x, give.attr = FALSE)),
      collapse = "\n"
    )
}

get_function_val <- function(x) {
  paste(
    capture.output(str_(x, give.attr = FALSE)),
    collapse = "\n"
  )
}

get_structured_val = function(x){
    paste(
      capture.output(str_(x, give.attr = FALSE)),
      collapse = "\n"
    )
}
