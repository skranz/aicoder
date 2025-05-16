merge_lines = function(txt) {
  paste0(txt, collapse="\n")
}

sep_lines = function(txt) {
  unlist(stri_split_fixed(txt, "\n"))
}

copy_into_list = function(dest_list = NULL, env = parent.frame()) {
  if (is.null(dest_list)) {
    dest_list = list()
  } else if (!is.list(dest_list)) {
    stop("dest_list must be NULL or a list")
  }

  obj_names = ls(env, all.names = TRUE)           # grab everything (incl. hidden)
  dest_list[obj_names] = mget(obj_names,          # vectorised fetch of objects
                              envir = env,
                              inherits = FALSE)
  return(dest_list)
}

is_empty = function(x) {
  if (is.null(x) | all(is.na(x))) return(TRUE)
  if (isTRUE(x=="")) return(TRUE)
  return(FALSE)
}

lapply_extract = function(li, field, null_val=NULL) {
  lapply(li, function(x) {
    res = x[[field]]
    if (is.null(res)) return(null_val)
    res
  })
}
sapply_extract = function(li, field, null_val=NULL) {
  sapply(li, function(x) {
    res = x[[field]]
    if (is.null(res)) return(null_val)
    res
  })
}

read_utf8 = function(file) {
  txt = readLines(file, encoding="UTF8", warn=FALSE)
  merge_lines(txt)
}
