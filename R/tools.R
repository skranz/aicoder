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


aic_write_utf8 = function(aic, x, file_base) {
  if (is.null(aic$aic_dir)) {
    cat("\nNot aic_dir specified.")
    return(NULL)
  }
  file = file.path(aic$aic_dir, file_base)
  write_utf8(x, file)
}

aic_read_utf8 = function(aic, file_base) {
  if (is.null(aic$aic_dir)) {
    cat("\nNot aic_dir specified.")
    return(NULL)
  }

  file = file.path(aic$aic_dir, file_base)
  if (!file.exists(file)) return(NULL)
  read_utf8(file)
}


write_utf8 = function(x,file) {
  txt = merge_lines(as.character(x))
  writeLines(txt, file)
}
read_utf8 = function(file, allow_empty=FALSE) {
  if (allow_empty) {
    if (!file.exists(file)) return(NULL)
  }
  txt = readLines(file, encoding="UTF8", warn=FALSE)
  merge_lines(txt)
}
