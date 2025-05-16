example = function() {
  text = readLines("C:/libraries/aicoder/ai_resp.txt")
  changes = aic_parse_response(text)
  protected_files = "main.R"
  just_ext = "r"
  fixed_dirs = c("r"="R")
  pat_file = "C:/libraries/aicoder/pat.txt"
  repo_dir = "C:/libraries/aicoder/stata2r"
  aic_apply_changes_and_github(changes, repo_dir=repo_dir, protected_files = protected_files, just_ext=just_ext, fixed_dirs=fixed_dirs, pat_file=pat_file)

}

aic_parse_and_commit_changes = function(aic, resp_text = NULL, overwrite=FALSE) {
  if (is_null(resp_text)) {
    resp_text = read_utf8(aic$response_file)
  }
  if (!overwrite) {
    if (isTRUE(identical(resp_text, aic$old_resp_text))) {
      cat("\nNo new AI response. Skip.\n")
      return(aic)
    }
  }
  aic = aic_parse_response(aic, resp_text)
  aic = aic_comit_changes(aic)
  aic
}

aic_commit_changes = function(aic) {
  restore.point("aic_apply_changes_and_github")
  library(GithubActions)
  repo_dir = aic$repo_dir
  if (is.null(aic[["pat"]])) {
    aic$pat = readLines(aic$pat_file)
  }
  GithubActions::gh_set_pat(aic$pat)
  GithubActions::gh_auth_status()

  msg=paste0("pre ", format(Sys.time()))
  GithubActions::gh_update(aic$repo_dir,msg)

  aic = aic_apply_file_changes(aic)


  msg=paste0("post ", format(Sys.time()))
  GithubActions::gh_update(repo_dir,msg)
  aic$old_resp_text = aic$resp_text

  aic
}

aic_apply_file_changes = function(aic) {
  restore.point("aic_apply_file_changes")
  changes = aic[["changes"]]
  inds = seq_len(NROW(changes))
  i = 1
  for (i in inds) {
    file = changes$file[i]
    base = basename(file)
    if (tolower(base) %in% aic$mod_protected_files) {
      cat("\nDon't change protected file ", base)
      next
    }
    ext = tolower(changes$ext[i])
    if (!is.null(aic$mod_just_ext)) {
      if (!ext %in% aic$mod_just_ext) {
        cat("\nDon't change file ", base, " since the extension is not alled.\n")
        next
      }
    }
    fixed_dir = aic$mod_fixed_dirs[ext]
    if (!is_empty(fixed_dir)) {
      file = file.path(fixed_dir, basename(file))
    }
    long_file = file.path(base_dir, file)
    if (changes$type[i]=="change") {
      content = changes$content[i]
      writeLines(content, long_file)
    } else if (changes$type == "remove") {
      if (file.exists(long_file)) {
        file.remove(long_file)
      }
    }
  }
  aic
}
