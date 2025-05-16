aic_new = function(repo_dir = NULL, aic_dir = file.path(repo_dir,"aicoder_work"), is_pkg=file.exists(file.path(repo_dir,"DESCRIPTION")), prompt_config_file=NULL, mod_protected_files = NULL, mod_fixed_dirs = NULL, mod_just_ext = "r", pat_file=NULL, response_file=NULL, show_test=FALSE, show_failed_test=TRUE, temp_dir = aic_dir, ...) {
  num_tests = 0
  test_logs =  vector("list",10)
  aic = copy_into_list()
  restore.point("aic_new")
  if (is_pkg) {
    aic$pkg_dir = repo_dir
    aic$pkg_name = basename(repo_dir)
  }
  if (!is.null(prompt_config_file)) {
    cfg = files2prompt::fp_parse_config(prompt_config_file)
    aic$cfg = cfg
  }
  aic
}

aic_make_prompt = function(aic, cfg=aic$cfg) {
  aic$prompt = files2prompt::files2prompt(root_dir = aic$repo_dir, open="{{", close="}}", cfg=cfg)
  aic_write_utf8(aic, aic$prompt, "prompt.txt")
  aic
}

aic_view_prompt = function(aic, prompt = aic[["prompt"]]) {
  restore.point("aic_view_prompt")
  if (is.null(prompt)) {
    cat("\nNo prompt was generated.\n")
    return(invsible(aic))
  }
  temp_dir = aic[["temp_dir"]]
  if (is.null(temp_dir)) {
    temp_dir = tempdir()
  }
  file = file.path(temp_dir, "aic_prompt_temp.txt")
  writeLines(prompt, file)
  try({
    writeClipboard(prompt)
    cat("\nPrompt copied to clipboard.")
  })

  rstudioapi::navigateToFile(file)
  return(invisible(aic))

}
