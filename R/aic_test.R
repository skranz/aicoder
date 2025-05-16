example = function() {
  pkg_dir = "C:/libraries/aicoder/stata2r"
}

aic_clear_tests = function(aic) {
  aic$test_num = 0
  aic
}

aic_test_logs = function(aic) {
  aic$test_logs[seq_len(aic$num_logs)]
}

aic_num_test_failed = function(aic) {
  sum(!sapply_extract(aic_test_logs(aic), "ok"))
}

aic_num_test = function(aic) {
  aic$num_logs
}

aic_all_test_string = function(aic, merge_lines=TRUE) {
  if (aic$num_logs==0) return("")
  str = sapply(seq_len(aic$num_logs), function(i) {
    aic_single_test_string(aic, i)
  })
  if (merge_lines) return(paste0(str, collapse="\n\n"))
  str
}

aic_single_test_string = function(aic, test_num, test_log = aic$test_logs[[test_num]]) {
  restore.point("aic_show_test")
  log = test_log
  out = paste0(
    "\n---\nOutcome of test '", log$test_name,"': ", if(log$ok) "ok" else "not ok","\n",
    log$msg,"\n"
  )
  if (!log$ok & !is.null(log[["log"]])) {
    out = paste0(out, "\nDetails:\n", log$log)
  }
  out
}



aic_show_test = function(aic, test_num=NULL, test_log = aic$test_logs[[test_num]]) {
  restore.point("aic_show_test")
  out = aic_single_test_string(aic, test_num, test_log)
  cat(out)
  invisible(aic)
}

aic_add_test = function(aic, test_log=list(ok=ok,test_name=test_name, msg=msg, log=log),ok, test_name, msg="", log=NULL, show_test = aic$show_test,  show_failed_test=aic$show_failed_test) {
  restore.point("aic_add_test")
  aic$last_test_ok = test_log$ok
  aic$num_logs = aic$num_logs+1
  if (length(aic$test_logs)<aic$num_logs) {
    aic$test_logs = c(aic$test_logs, vector("list",max(10,length(aic$test_logs))))
  }
  aic$test_logs[[aic$num_logs]] = test_log
  if (show_test | (show_failed_test & !test_log$ok)) {
    aic_show_test(aic, test_log=test_log)
  }
  aic
}

aic_rel_path = function(aic, files) {
  repo_dir = normalizePath(aic$repo_dir)
  files = normalizePath(files,mustWork = FALSE,winslash = "/")
  rel_paths = stri_replace_first_fixed(files, paste0(repo_dir,"/"),"")
  rel_paths
}

# Test whether all R files can be sourced
aic_test_source_r_files = function(aic, dir = aic$repo_dir) {
  restore.point("aic_test_source_r_files")
  files = list.files(file.path(dir,"R"),full.names = TRUE)
  res_li = lapply(files, function(file) {
    code_str = paste0('source("', file, '")')
    run_with_log(code_str = code_str)
  })
  has_err = sapply_extract(res_li, "has_error", null_val=FALSE)
  if (any(has_err)) {
    num_err = sum(has_err)
    paths = aic_rel_path(aic, files[has_err])
    details = merge_lines(sapply_extract(res_li[has_err],"log"))
    msg = paste0("\nDetails of failed 'source' commands:\n",details,"\n"
    )
    return(aic_add_test(aic,ok=FALSE, test_name="source_r", msg=msg))
  }
  return(aic_add_test(aic, ok=TRUE,test_name="source_r", msg="All R files could be sourced."))
}


aic_test_build_pkg = function(aic) {
  restore.point("aic_test_build_pkg")
  pkg_dir = aic$repo_dir
  aic = aic_test_source_r_files(aic)
  if (!aic$last_test_ok) return(aic)

  log = local_install_with_log(pkg_dir)
  if (log$has_error) {
    return(aic_add_test(aic,ok=FALSE, test_name="build_pkg", msg="Failed to build the package", log=log$log))
  }
  return(aic_add_test(aic, ok=TRUE, test_name="build_pkg", msg="Package builds without error."))
}
