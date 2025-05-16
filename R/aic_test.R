example = function() {
  pkg_dir = "C:/libraries/aicoder/stata2r"
}


aic_tests_finish = function(aic) {
  restore.point("aic_tests_finish")
  header =  paste0("In total ", aic_num_test_failed(aic), " of ", aic_num_test(aic), " tests failed.\n\n")
  aic$test_report = paste0(header, aic_all_test_string(aic))
  aic_write_utf8(aic, aic$test_report, "test_report.txt")
  aic
}

aic_clear_tests = function(aic) {
  aic$num_tests = 0
  aic$test_logs =  vector("list",10)
  aic
}

aic_test_logs = function(aic) {
  aic$test_logs[seq_len(aic$num_tests)]
}

aic_num_test_failed = function(aic) {
  sum(!sapply_extract(aic_test_logs(aic), "ok"))
}

aic_num_test = function(aic) {
  aic$num_tests
}

aic_all_test_string = function(aic, merge_lines=TRUE) {
  restore.point("aic_all_test_string")
  if (aic$num_tests==0) return("")
  str = sapply(seq_len(aic$num_tests), function(i) {
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
  aic$num_tests = aic$num_tests+1
  if (length(aic$test_logs)<aic$num_tests) {
    aic$test_logs = c(aic$test_logs, vector("list",max(10,length(aic$test_logs))))
  }
  aic$test_logs[[aic$num_tests]] = test_log
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

aic_test_script = function(aic, file, change_wd = TRUE) {
  if (!file.exists(file)) {
    cat("\ntest script ", file, "does not exist.")
    return(aic)
  }
  oldwd = getwd()
  if (change_wd) {
    dir = dirname(file)
    setwd(dir)
  }
  log = source_and_capture(file)
  aic = aic_add_test(aic,ok=!log$has_error, test_name=basename(file), msg="", log=log$log)
  try(setwd(oldwd), silent=TRUE)
  aic
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
