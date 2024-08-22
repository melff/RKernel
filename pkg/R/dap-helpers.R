variables_helper <- new.env()

#' @export
inspect_variables <- function(envir = parent.frame()){
  var_names <- ls(envir = envir)
  var_refs <- structure(
    seq_along(var_names),
    names = var_names
  )
  enames <- structure(
    var_names,
    names = as.character(var_refs)
  )
  variables_helper$refs <- var_refs
  variables_helper$enames <- enames
  variables_helper$names <- enames
  response <- lapply(var_names, get_variable, envir = envir)
  msg_send(response)
}

get_variable <- function(ename, envir = parent.frame()) {
    # log_out("get_variable")
    # log_out(ename)
    value <- get_value(ename, envir = envir)
    descr <- inspect_value(value)
    var_ref <- variables_helper$refs[ename]
    name <- variables_helper$names[var_ref]
    list(
      name = name,
      variablesReference = if(descr$structured) var_ref else 0,
      value = descr$value,
      type = descr$type,
      evaluateName = ename
    )
}

get_value <- function(ename, envir = parent.frame()){
  eval(str2expression(ename), envir = envir)
}

inspect_value <- function(x) {
  if (is.atomic(x)) {
    value <- get_atomic_val(x)
    type <- get_atomic_type(x)
    structured <- (length(x) > 1)
  }
  else if(is.function(x)){
    type <- "function"
    value <- get_function_val(x)
    structured <- TRUE  
  }
  else if(isS4(x)) {
    type <- paste("S4 class",sQuote(class(x)))
    value <- get_structured_val(x)
    structured <- TRUE
  }
  else {
    type <- paste(class(x), collapse=", ")
    value <- get_structured_val(x)
    structured <- TRUE
  }
  if(has_nontriv_attribs(x)){
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
  if(is.language(x)) paste(deparse(x), collapse=" ")
  else capture.output(str_(x, give.attr = FALSE, list.len = 0))[1]
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

ref2children <- function(ref, envir = parent.frame()) {
  # log_out("ref2children")
  # log_out(ref)
  if(ref == 0) return(NULL)
  ename <- variables_helper$enames[ref]
  # log_out(ename)
  obj <- get_value(ename, envir = envir)
  response <- tryCatch(
    get_children(obj, ename, envir = envir),
    error = function(e) conditionMessage(e)
  )
  if (is.character(response)) log_error(response)
  else msg_send(response)
}

get_children <- function(obj, ename, envir) {
  children <- list()
  if (is.atomic(obj) && length(obj) > 1) {
    elts_ename <- paste0("RKernel:::str_minimal(", ename, ")")
    elts_ref <- max(variables_helper$refs) + 1
    names(elts_ref) <- elts_ename
    variables_helper$refs <- c(variables_helper$refs, elts_ref)
    variables_helper$enames <- c(
      variables_helper$enames,
      structure(elts_ename,
        names = as.character(elts_ref)
      )
    )
    variables_helper$names <- c(
      variables_helper$names,
      structure("[ ]",
        names = as.character(elts_ref)
      )
    )
    ev <- get_variable(elts_ename, envir = envir)
    children <- list(ev)
  }
  else if (is.list(obj)) {
    nms <- names(obj)
    n <- length(obj)
    if (!length(nms)) {
      nms <- rep("", n)
    }
    i <- seq_len(n)
    char_idx <- ifelse(safe_names(nms), nms, paste0("`", nms, "`"))
    num_idx <- paste0("[[", i, "]]")
    idx <- ifelse(nzchar(nms), char_idx, num_idx)
    inames <- ifelse(nzchar(nms),nms,i)
    cpl <- ifelse(nzchar(nms), "$", "")
    # inames <- paste0(cpl, idx)
    enames <- paste0(ename, cpl, idx)
    m <- max(variables_helper$refs)
    new_refs <- structure(
      seq.int(from = m + 1, to = m + n),
      names = enames
    )
    variables_helper$refs <- c(variables_helper$refs, new_refs)
    variables_helper$enames <- c(
      variables_helper$enames,
      structure(enames,
        names = as.character(new_refs)
      )
    )
    variables_helper$names <- c(
      variables_helper$names,
      structure(inames,
        names = as.character(new_refs)
      )
    )
    children <- lapply(enames, get_variable, envir = envir)
  } 
  else if (is.environment(obj)) {
    e_ename <- paste0("format(", ename, ")")
    e_ref <- max(variables_helper$refs) + 1
    names(e_ref) <- e_ename
    variables_helper$refs <- c(variables_helper$refs, e_ref)
    variables_helper$enames <- c(
      variables_helper$enames,
      structure(e_ename,
        names = as.character(e_ref)
      )
    )
    variables_helper$names <- c(
      variables_helper$names,
      structure("",
        names = as.character(e_ref)
      )
    )
    ee <- get_variable(e_ename, envir = envir)
    children <- list(ee)
    if(!identical(obj,.GlobalEnv)) {
      nms <- ls(obj)
      n <- length(nms)
      i <- seq_len(n)
      cpl <- ifelse(nzchar(nms), "$", "")
      enames <- ifelse(safe_names(nms), nms, paste0("`", nms, "`"))
      enames <- paste0(ename,cpl,enames)
      m <- max(variables_helper$refs)
      new_refs <- structure(
        seq.int(from = m + 1, to = m + n),
        names = enames
      )
      variables_helper$refs <- c(variables_helper$refs, new_refs)
      variables_helper$enames <- c(
        variables_helper$enames,
        structure(enames,
          names = as.character(new_refs)
        )
      )
      variables_helper$names <- c(
        variables_helper$names,
        structure(nms,
          names = as.character(new_refs)
        )
      )
      ov <- lapply(enames, get_variable, envir = envir)
      children <- c(children, ov)
    }
  } 
  else if (is.function(obj)) {
    args_ename <- paste0("RKernel:::get_args(", ename, ")")
    args_ref <- max(variables_helper$refs) + 1
    names(args_ref) <- args_ename
    variables_helper$refs <- c(variables_helper$refs, args_ref)
    variables_helper$enames <- c(
      variables_helper$enames,
      structure(args_ename,
        names = as.character(args_ref)
      )
    )
    variables_helper$names <- c(
      variables_helper$names,
      structure("args()",
        names = as.character(args_ref)
      )
    )
    arv <- get_variable(args_ename, envir = envir)
    children <- c(children, list(arv))

    e_ename <- paste0("environment(", ename, ")")
    e_ref <- max(variables_helper$refs) + 1
    names(e_ref) <- e_ename
    variables_helper$refs <- c(variables_helper$refs, e_ref)
    variables_helper$enames <- c(
      variables_helper$enames,
      structure(e_ename,
        names = as.character(e_ref)
      )
    )
    variables_helper$names <- c(
      variables_helper$names,
      structure("environment()",
        names = as.character(e_ref)
      )
    )
    ee <- get_variable(e_ename, envir = envir)
    children <- c(children, list(ee))
  }
  else if (is.language(obj)) {
    lang_ename <- paste0("paste(deparse(", ename, "), collapse=' ')")
    lang_ref <- max(variables_helper$refs) + 1
    names(lang_ref) <- lang_ename
    variables_helper$refs <- c(variables_helper$refs, lang_ref)
    variables_helper$enames <- c(
      variables_helper$enames,
      structure(lang_ename,
        names = as.character(lang_ref)
      )
    )
    variables_helper$names <- c(
      variables_helper$names,
      structure("",
        names = as.character(lang_ref)
      )
    )
    lv <- get_variable(lang_ename, envir = envir)
    children <- list(lv)
  }
  
  if(isS4(obj)) {
    nms <- setdiff(slotNames(obj), c(".Data", "names"))
    n <- length(nms)
    nms <- ifelse(safe_names(nms), nms, paste0("`", nms, "`"))
    nms <- paste0("@", nms)
    enames <- paste0(ename, nms)
    m <- max(variables_helper$refs)
    slot_refs <- structure(
      seq.int(from = m + 1, to = m + n),
      names = enames
    )
    variables_helper$refs <- c(variables_helper$refs, slot_refs)
    variables_helper$enames <- c(
      variables_helper$enames,
      structure(enames,
        names = as.character(slot_refs)
      )
    )
    variables_helper$names <- c(
      variables_helper$names,
      structure(nms,
        names = as.character(slot_refs)
      )
    )
    slots <- lapply(enames, get_variable, envir = envir)
    children <- c(children, slots)
  }
  else if(has_nontriv_attribs(obj)){
    attr_ename <- paste0("RKernel:::nontriv_attribs(", ename, ")")
    attr_ref <- max(variables_helper$refs) + 1
    names(attr_ref) <- attr_ename
    variables_helper$refs <- c(variables_helper$refs, attr_ref)
    variables_helper$enames <- c(
      variables_helper$enames,
      structure(attr_ename,
        names = as.character(attr_ref)
      )
    )
    variables_helper$names <- c(
      variables_helper$names,
      structure("attributes()",
        names = as.character(attr_ref)
      )
    )
    av <- get_variable(attr_ename, envir = envir)
    children <- c(children, list(av))
  }
  return(children)
}

nontriv_attribs <- function(x) {
  a <- attributes(x)
  triv <- names(a) %in% c("class", "dim", "names")
  a[!triv]
}

has_nontriv_attribs <- function(x) {
  length(nontriv_attribs(x)) > 0
}

str_minimal <- function(x){
  capture.output(
    str_(
      x,
      give.head = FALSE, 
      give.length = FALSE, 
      give.attr = FALSE
  ))
}

get_args <- function(x) {
  a <- as.list(args(x))
  a <- a[-length(a)]
  lapply(a, deparse)
}

safe_names <- function(x) grepl("^[a-zA-Z.][a-zA-Z0-9._]*$", x)
